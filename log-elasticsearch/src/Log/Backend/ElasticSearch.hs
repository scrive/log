-- | Elasticsearch logging back-end.
module Log.Backend.ElasticSearch
  ( ElasticSearchConfig
  , esServer
  , esIndex
  , esShardCount
  , esReplicaCount
  , esMapping
  , esLogin
  , esLoginInsecure
  , checkElasticSearchLogin
  , checkElasticSearchConnection
  , defaultElasticSearchConfig
  , withElasticSearchLogger
  , elasticSearchLogger
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.IORef
import Data.Maybe
import Data.Semigroup
import Data.Time
import Data.Word
import Log
import Log.Internal.Logger
import Network.HTTP.Client
import Prelude
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Traversable as F
import qualified Data.Vector as V

import Log.Backend.ElasticSearch.Internal

----------------------------------------
-- | Create an 'elasticSearchLogger' for the duration of the given
-- action, and shut it down afterwards, making sure that all buffered
-- messages are actually written to the Elasticsearch store.
withElasticSearchLogger :: ElasticSearchConfig -> (Logger -> IO r) -> IO r
withElasticSearchLogger conf act = do
  logger <- elasticSearchLogger conf
  withLogger logger act

{-# DEPRECATED elasticSearchLogger "Use 'withElasticSearchLogger' instead!" #-}

-- | Start an asynchronous logger thread that stores messages using
-- Elasticsearch.
--
-- Please use 'withElasticSearchLogger' instead, which is more
-- exception-safe (see the note attached to 'mkBulkLogger').
elasticSearchLogger
  :: ElasticSearchConfig -- ^ Configuration.
  -> IO Logger
elasticSearchLogger esConf@ElasticSearchConfig{..} = do
  checkElasticSearchLogin esConf
  env <- mkEsEnv esConf
  versionRef <- newIORef Nothing
  indexRef <- newIORef T.empty
  mkBulkLogger "ElasticSearch" (\msgs -> do
    now <- getCurrentTime
    oldIndex <- readIORef indexRef
    retryOnException versionRef $ do
      -- We need to consider version of ES because ES >= 5.0.0 and ES >= 7.0.0
      -- have slight differences in parts of API used for logging.
      version <- readIORef versionRef >>= \case
        Just version -> pure version
        Nothing -> serverInfo env >>= \case
          Left (ex :: HttpException) -> error
            $  "elasticSearchLogger: unexpected error: "
            <> show ex
            <> " (is ElasticSearch server running?)"
          Right reply -> case parseEsVersion $ responseBody reply of
            Nothing -> error
              $  "elasticSearchLogger: invalid response when parsing version number: "
              <> show reply
            Just version -> pure version
      -- Elasticsearch index names are additionally indexed by date so that each
      -- day is logged to a separate index to make log management easier.
      let index = T.concat
            [ esIndex
            , "-"
            , T.pack $ formatTime defaultTimeLocale "%F" now
            ]
      when (oldIndex /= index) $ do
        -- There is an obvious race condition in presence of more than one
        -- logger instance running, but it's irrelevant as attempting to create
        -- index that already exists is harmless.
        ixExists <- indexExists env index
        unless ixExists $ do
          reply <- createIndexWithMapping version env esConf index
          unless (isSuccess reply) $ do
            printEsError "error while creating index" $ responseBody reply
        writeIORef indexRef index
      let jsonMsgs = V.fromList $ map (toJsonMsg now) $ zip [1..] msgs
      reply <- bulkIndex version env esConf index jsonMsgs
      -- Try to parse parts of reply to get information about log messages that
      -- failed to be inserted for some reason.
      let replyBody = responseBody reply
          result = do
            Object response <- return replyBody
            Bool hasErrors  <- "errors" `H.lookup` response
            Array jsonItems <- "items"  `H.lookup` response
            items <- F.forM jsonItems $ \v -> do
              Object item   <- return v
              Object index_ <- "index" `H.lookup` item
                -- ES <= 2.x returns 'create' for some reason, so consider both.
                <|> "create" `H.lookup` item
              return index_
            guard $ V.length items == V.length jsonMsgs
            return (hasErrors, items)
      case result of
        Nothing -> printEsError "unexpected response" replyBody
        Just (hasErrors, items) -> when hasErrors $ do
          -- If any message failed to be inserted because of type mismatch, go
          -- back to them, log the insertion failure and add type suffix to each
          -- of the keys in their "data" fields to work around type errors.
          let failed = V.findIndices (H.member "error") items
              newMsgs = (`V.map` failed) $ \n ->
                let modifyData :: Bool -> Value -> Value
                    modifyData addError (Object hm) = Object $
                      let newData = H.foldlWithKey' keyAddValueTypeSuffix H.empty hm
                      in if addError
                         then newData `H.union` H.fromList
                              [ "__es_error" .= H.lookup "error" (items V.! n)
                              ]
                         else newData
                    modifyData _ v = v

                    keyAddValueTypeSuffix acc k v = H.insert
                      (case v of
                          Object{} -> k <> "_object"
                          Array{}  -> k <> "_array"
                          String{} -> k <> "_string"
                          Number{} -> k <> "_number"
                          Bool{}   -> k <> "_bool"
                          Null{}   -> k <> "_null"
                      ) (modifyData False v) acc
                in H.adjust (modifyData True) "data" $ jsonMsgs V.! n
          -- Attempt to put modified messages and ignore any further errors.
          void $ bulkIndex version env esConf index newMsgs)
    (refreshIndex env =<< readIORef indexRef)
  where
    printEsError msg body =
      T.putStrLn $ "elasticSearchLogger: " <> msg <> " " <> prettyJson body

    retryOnException :: forall r. IORef (Maybe EsVersion) -> IO r -> IO r
    retryOnException versionRef m = try m >>= \case
      Left (ex::SomeException) -> do
        putStrLn $ "ElasticSearch: unexpected error: "
          <> show ex <> ", retrying in 10 seconds"
        -- If there was an exception, ElasticSearch version might've changed, so
        -- reset it.
        writeIORef versionRef Nothing
        threadDelay $ 10 * 1000000
        retryOnException versionRef m
      Right result -> return result

    prettyJson :: Value -> T.Text
    prettyJson = TL.toStrict
               . T.toLazyText
               . encodePrettyToTextBuilder' defConfig { confIndent = Spaces 2 }

    toJsonMsg :: UTCTime -> (Word32, LogMessage) -> H.HashMap T.Text Value
    toJsonMsg now (n, msg) = H.union jMsg $ H.fromList
      [ ("insertion_order", toJSON n)
      , ("insertion_time",  toJSON now)
      ]
      where
        Object jMsg = toJSON msg

----------------------------------------

-- | Check that login credentials are specified properly.
--
-- @since 0.10.0.0
checkElasticSearchLogin :: ElasticSearchConfig -> IO ()
checkElasticSearchLogin ElasticSearchConfig{..} =
    when (isJust esLogin
          && not esLoginInsecure
          && not ("https:" `T.isPrefixOf` esServer)) $
      error $ "ElasticSearch: insecure login: "
        <> "Attempting to send login credentials over an insecure connection. "
        <> "Set esLoginInsecure = True to disable this check."

-- | Check that we can connect to the ES server.
--
-- @since 0.10.0.0
checkElasticSearchConnection :: ElasticSearchConfig -> IO (Either HttpException ())
checkElasticSearchConnection esConf =
  fmap (const ()) <$> (serverInfo =<< mkEsEnv esConf)
