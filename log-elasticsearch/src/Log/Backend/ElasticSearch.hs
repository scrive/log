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
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.IORef
import Data.Maybe
import Data.Semigroup
import Data.Time
import Log
import Log.Internal.Logger
import Network.HTTP.Client
import Prelude
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Traversable as F
import qualified Data.Vector as V

import Log.Backend.ElasticSearch.Internal
import qualified Log.Internal.Aeson.Compat as AC

----------------------------------------
-- | Create an 'elasticSearchLogger' for the duration of the given
-- action, and shut it down afterwards, making sure that all buffered
-- messages are actually written to the Elasticsearch store.
withElasticSearchLogger :: MonadUnliftIO m => ElasticSearchConfig -> (Logger -> m r) -> m r
withElasticSearchLogger conf act = withRunInIO $ \unlift -> do
  logger <- elasticSearchLogger conf
  withLogger logger (unlift . act)

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
      let jsonMsgs = V.fromList $ map toJsonMsg msgs
      reply <- responseBody <$> bulkIndex version env esConf index jsonMsgs
      -- Try to parse parts of reply to get information about log messages that
      -- failed to be inserted for some reason.
      case checkForBulkErrors jsonMsgs reply of
        Nothing -> printEsError "unexpected response" reply
        Just (hasErrors, responses) -> when hasErrors $ do
          -- If any message failed to be inserted because of type mismatch, go
          -- back to them, log the insertion failure and add type suffix to each
          -- of the keys in their "data" fields to work around type errors.
          let newMsgs =
                let modifyData :: Maybe Value -> Value -> Value
                    modifyData merr (Object hm) = Object $
                      let newData = AC.foldrWithKey keyAddValueTypeSuffix AC.empty hm
                      in case merr of
                        -- We have the error message, i.e. we're at the top
                        -- level object, so add it to the data.
                        Just err -> newData `AC.union` AC.singleton "__es_error" err
                        Nothing  -> newData
                    modifyData _ v = v

                    keyAddValueTypeSuffix k v acc = AC.insert
                      (case v of
                          Object{} -> k <> "_object"
                          Array{}  -> k <> "_array"
                          String{} -> k <> "_string"
                          Number{} -> k <> "_number"
                          Bool{}   -> k <> "_bool"
                          Null{}   -> k <> "_null"
                      ) (modifyData Nothing v) acc
                in adjustFailedMessagesWith modifyData jsonMsgs responses
          -- Attempt to put modified messages.
          newReply <- responseBody <$> bulkIndex version env esConf index newMsgs
          case checkForBulkErrors newMsgs newReply of
            Nothing -> printEsError "unexpected response" newReply
            Just (newHasErrors, newResponses) -> when newHasErrors $ do
              -- If some of the messages failed again (it might happen e.g. if
              -- data contains an array with elements of different types), drop
              -- their data field.
              let newerMsgs =
                    let modifyData :: Maybe Value -> Value -> Value
                        modifyData (Just err) Object{} = object [ "__es_error" .= err ]
                        modifyData _ v = v
                    in adjustFailedMessagesWith modifyData newMsgs newResponses
              -- Ignore any further errors.
              void $ bulkIndex version env esConf index newerMsgs)
    (refreshIndex env =<< readIORef indexRef)
  where
    -- Process reply of bulk indexing to get responses for each index operation
    -- and check whether any insertion failed.
    checkForBulkErrors
      :: V.Vector a
      -> Value
      -> Maybe (Bool, V.Vector Object)
    checkForBulkErrors jsonMsgs replyBody = do
      Object response <- pure replyBody
      Bool hasErrors  <- "errors" `AC.lookup` response
      Array jsonItems <- "items"  `AC.lookup` response
      items <- F.forM jsonItems $ \v -> do
        Object item   <- pure v
        Object index_ <- "index" `AC.lookup` item
          -- ES <= 2.x returns 'create' for some reason, so consider both.
          <|> "create" `AC.lookup` item
        pure index_
      guard $ V.length items == V.length jsonMsgs
      pure (hasErrors, items)

    adjustFailedMessagesWith
      :: (Maybe err -> obj -> obj)
      -> V.Vector (AC.KeyMap obj)
      -> V.Vector (AC.KeyMap err)
      -> V.Vector (AC.KeyMap obj)
    adjustFailedMessagesWith f jsonMsgs responses =
      let failed = V.imapMaybe (\n item -> (n, ) <$> "error" `AC.lookup` item) responses
          adjust act key m = case AC.lookup key m of
            Nothing -> m
            Just v -> AC.insert key (act v) m
      in (`V.map` failed) $ \(n, err) -> adjust (f $ Just err) "data" $ jsonMsgs V.! n

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

    toJsonMsg :: LogMessage -> Object
    toJsonMsg msg = let Object jMsg = toJSON msg in jMsg

----------------------------------------

-- | Check that login credentials are specified properly.
--
-- @since 0.10.0.0
checkElasticSearchLogin :: MonadIO m => ElasticSearchConfig -> m ()
checkElasticSearchLogin ElasticSearchConfig{..} = liftIO $ do
  when (isJust esLogin
        && not esLoginInsecure
        && not ("https:" `T.isPrefixOf` esServer)) $
    error $ "ElasticSearch: insecure login: "
      <> "Attempting to send login credentials over an insecure connection. "
      <> "Set esLoginInsecure = True to disable this check."

-- | Check that we can connect to the ES server.
--
-- @since 0.10.0.0
checkElasticSearchConnection :: MonadIO m => ElasticSearchConfig -> m (Either HttpException ())
checkElasticSearchConnection esConf = liftIO $ do
  fmap (const ()) <$> (serverInfo =<< mkEsEnv esConf)
