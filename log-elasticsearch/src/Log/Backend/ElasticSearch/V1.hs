-- | Elasticsearch logging back-end.
module Log.Backend.ElasticSearch.V1 (
    ElasticSearchConfig
  , esServer
  , esIndex
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
import Control.Arrow (second)
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Bits
import Data.IORef
import Data.Maybe (isJust)
import Data.Semigroup
import Data.Time
import Data.Time.Clock.POSIX
import Data.Word
import Database.V1.Bloodhound hiding (Status)
import Log
import Log.Internal.Logger
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Prelude
import System.IO
import TextShow
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Traversable as F
import qualified Data.Vector as V

import Log.Backend.ElasticSearch.V1.Internal

----------------------------------------
-- | Create an 'elasticSearchLogger' for the duration of the given
-- action, and shut it down afterwards, making sure that all buffered
-- messages are actually written to the Elasticsearch store.
withElasticSearchLogger :: ElasticSearchConfig -> IO Word32 -> (Logger -> IO r)
                        -> IO r
withElasticSearchLogger conf randGen act = do
  logger <- elasticSearchLogger conf randGen
  withLogger logger act

{-# DEPRECATED elasticSearchLogger "Use 'withElasticSearchLogger' instead!" #-}

-- | Start an asynchronous logger thread that stores messages using
-- Elasticsearch.
--
-- Please use 'withElasticSearchLogger' instead, which is more
-- exception-safe (see the note attached to 'mkBulkLogger').
elasticSearchLogger ::
  ElasticSearchConfig -- ^ Configuration.
  -> IO Word32        -- ^ Generate a random 32-bit word for use in
                      -- document IDs.
  -> IO Logger
elasticSearchLogger esConf@ElasticSearchConfig{..} genRandomWord = do

  checkElasticSearchLogin esConf >>= \case
    Left (ex :: IOException) -> error . show $ ex
    Right _ -> return ()

  checkElasticSearchConnection esConf >>= \case
      Left (ex :: HttpException) ->
        hPutStrLn stderr $
                  "ElasticSearch: unexpected error: " <>
                  show ex <>
                  " (is ElasticSearch server running?)"
      Right () -> return ()

  indexRef <- newIORef $ IndexName T.empty
  mkBulkLogger "ElasticSearch" (\msgs -> do
    now <- getCurrentTime
    oldIndex <- readIORef indexRef
    -- Bloodhound doesn't support letting ES autogenerate IDs, so let's generate
    -- them ourselves. An ID of a log message is 12 bytes (4 bytes: random, 4
    -- bytes: current time as epoch, 4 bytes: insertion order) encoded as
    -- Base64. This makes eventual collisions practically impossible.
    baseID <- (<>)
      <$> (littleEndianRep <$> liftIO genRandomWord)
      <*> pure (littleEndianRep . floor $ timeToDouble now)
    retryOnException . runBH_ esConf $ do
      -- Elasticsearch index names are additionally indexed by date so that each
      -- day is logged to a separate index to make log management easier.
      let index = IndexName $ T.concat [
              esIndex
            , "-"
            , T.pack $ formatTime defaultTimeLocale "%F" now
            ]
      when (oldIndex /= index) $ do
        -- There is an obvious race condition in the presence of more than one
        -- logger instance running, but it's irrelevant as attempting to create
        -- index that already exists is harmless.
        indexExists' <- indexExists index
        unless indexExists' $ do
          -- Note that Bloodhound won't let us create index using
          -- default settings.
          let indexSettings = IndexSettings {
                  indexShards   = ShardCount esShardCount
                , indexReplicas = ReplicaCount esReplicaCount
                }
          void $ createIndex indexSettings index
          reply <- putMapping index mapping LogsMapping
          when (not $ isSuccess reply) $ do
            error $ "ElasticSearch: error while creating mapping: "
              <> T.unpack (T.decodeUtf8 . BSL.toStrict . jsonToBSL
                            $ decodeReply reply)
        liftIO $ writeIORef indexRef index
      let jsonMsgs = V.fromList $ map (toJsonMsg now) $ zip [1..] msgs
      reply <- bulk $ V.map (toBulk index baseID) jsonMsgs
      -- Try to parse parts of reply to get information about log messages that
      -- failed to be inserted for some reason.
      let replyBody = decodeReply reply
          result = do
            Object response <- return replyBody
            Bool hasErrors  <- "errors" `H.lookup` response
            Array jsonItems <- "items"  `H.lookup` response
            items <- F.forM jsonItems $ \v -> do
              Object item   <- return v
              Object index_ <- "index" `H.lookup` item
              return index_
            guard $ V.length items == V.length jsonMsgs
            return (hasErrors, items)
      case result of
        Nothing -> liftIO . BSL.putStrLn
          $ "ElasticSearch: unexpected response: " <> jsonToBSL replyBody
        Just (hasErrors, items) -> when hasErrors $ do
          -- If any message failed to be inserted because of type mismatch, go
          -- back to them, replace their data with elastic search error and put
          -- old data into its own namespace to work around insertion errors.
          let failed = V.findIndices (H.member "error") items
          dummyMsgs <- V.forM failed $ \n -> do
            dataNamespace <- liftIO genRandomWord
            let modifyData oldData = object [
                    "__es_error" .= H.lookup "error" (items V.! n)
                  , "__es_modified" .= True
                  , ("__data_" <> showt dataNamespace) .= oldData
                  ]
            return . second (H.adjust modifyData "data") $ jsonMsgs V.! n
          -- Attempt to put modified messages and ignore any further errors.
          void $ bulk (V.map (toBulk index baseID) dummyMsgs))
    (elasticSearchSync indexRef)
  where
    mapping = MappingName esMapping

    elasticSearchSync :: IORef IndexName -> IO ()
    elasticSearchSync indexRef = do
      indexName <- readIORef indexRef
      void . runBH_ esConf $ refreshIndex indexName

    retryOnException :: forall r. IO r -> IO r
    retryOnException m = try m >>= \case
      Left (ex::SomeException) -> do
        putStrLn $ "ElasticSearch: unexpected error: "
          <> show ex <> ", retrying in 10 seconds"
        threadDelay $ 10 * 1000000
        retryOnException m
      Right result -> return result

    timeToDouble :: UTCTime -> Double
    timeToDouble = realToFrac . utcTimeToPOSIXSeconds

    jsonToBSL :: Value -> BSL.ByteString
    jsonToBSL = encodePretty' defConfig { confIndent = Spaces 2 }

    toJsonMsg :: UTCTime -> (Word32, LogMessage)
              -> (Word32, H.HashMap T.Text Value)
    toJsonMsg now (n, msg) = (n, H.union jMsg $ H.fromList [
        ("insertion_order", toJSON n)
      , ("insertion_time",  toJSON now)
      ])
      where
        Object jMsg = toJSON msg

    mkDocId :: BS.ByteString -> Word32 -> DocId
    mkDocId baseID insertionOrder = DocId . T.decodeUtf8
                                    . B64.encode $ BS.concat [
        baseID
      , littleEndianRep insertionOrder
      ]

    toBulk :: IndexName -> BS.ByteString -> (Word32, H.HashMap T.Text Value)
           -> BulkOperation
    toBulk index baseID (n, obj) =
      BulkIndex index mapping (mkDocId baseID n) $ Object obj

data LogsMapping = LogsMapping
instance ToJSON LogsMapping where
  toJSON LogsMapping = object [
    "properties" .= object [
        "insertion_order" .= object [
            "type" .= ("integer"::T.Text)
          ]
        , "insertion_time" .= object [
            "type"   .= ("date"::T.Text)
          , "format" .= ("date_time"::T.Text)
          ]
        , "time" .= object [
            "type"   .= ("date"::T.Text)
          , "format" .= ("date_time"::T.Text)
          ]
        , "domain" .= object [
            "type" .= ("string"::T.Text)
          ]
        , "level" .= object [
            "type" .= ("string"::T.Text)
          ]
        , "component" .= object [
            "type" .= ("string"::T.Text)
          ]
        , "message" .= object [
            "type" .= ("string"::T.Text)
          ]
        ]
    ]

  toEncoding LogsMapping = Aeson.pairs $ mconcat
    [ Aeson.pair "properties" $ Aeson.pairs $ mconcat
      [ Aeson.pair "insertion_order"  $ Aeson.pairs $ mconcat
        [ "type" .= ("integer"::T.Text)
        ]
      , Aeson.pair "insertion_time" $ Aeson.pairs $ mconcat
        [ "type"   .= ("date"::T.Text)
        , "format" .= ("date_time"::T.Text)
        ]
      , Aeson.pair "time" $ Aeson.pairs $ mconcat
        [ "type"   .= ("date"::T.Text)
        , "format" .= ("date_time"::T.Text)
        ]
      , Aeson.pair "domain" $ Aeson.pairs $ mconcat
        [ "type" .= ("string"::T.Text)
        ]
      , Aeson.pair "level" $ Aeson.pairs $ mconcat
        [ "type" .= ("string"::T.Text)
        ]
      , Aeson.pair "component" $ Aeson.pairs $ mconcat
        [ "type" .= ("string"::T.Text)
        ]
      , Aeson.pair "message" $ Aeson.pairs $ mconcat
        [ "type" .= ("string"::T.Text)
        ]
      ]
    ]

----------------------------------------

littleEndianRep :: Word32 -> BS.ByteString
littleEndianRep = fst . BS.unfoldrN 4 step
  where
    step n = Just (fromIntegral $ n .&. 0xff, n `shiftR` 8)

decodeReply :: Reply -> Value
decodeReply reply = case eitherDecode' $ responseBody reply of
  Right body -> body
  Left  err  -> object ["decoding_error" .= err]

checkElasticSearchLogin :: ElasticSearchConfig -> IO (Either IOException ())
checkElasticSearchLogin ElasticSearchConfig{..} =
  try $
    when (isJust esLogin
          && not esLoginInsecure
          && not ("https:" `T.isPrefixOf` esServer)) $
      error $ "ElasticSearch: insecure login: "
        <> "Attempting to send login credentials over an insecure connection. "
        <> "Set esLoginInsecure = True to disable this check."

checkElasticSearchConnection :: ElasticSearchConfig -> IO (Either HttpException ())
checkElasticSearchConnection esConf =
    try (void $ runBH_ esConf listIndices)

runBH_ :: forall r. ElasticSearchConfig -> BH IO r -> IO r
runBH_ ElasticSearchConfig{..} f = do
  mgr <- newManager tlsManagerSettings
  let hook = maybe return (uncurry basicAuthHook) esLogin
  let env = (mkBHEnv (Server esServer) mgr) { bhRequestHook = hook }
  runBH env f
