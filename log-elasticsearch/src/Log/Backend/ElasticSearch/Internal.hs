{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
module Log.Backend.ElasticSearch.Internal
  ( ElasticSearchConfig(..)
  , defaultElasticSearchConfig
  -- * ES version
  , EsVersion(..)
  , parseEsVersion
  , esV5, esV7
  -- * ES commands
  , serverInfo
  , indexExists
  , createIndexWithMapping
  , bulkIndex
  , refreshIndex
  -- * ES communication details
  , EsEnv(..)
  , mkEsEnv
  , dispatch
  , decodeReply
  , isSuccess
  ) where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Ix (inRange)
import Data.Maybe
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Types
#if OPENSSL
import Network.HTTP.Client.OpenSSL (newOpenSSLManager, withOpenSSL)
#else
import Network.HTTP.Client.TLS (tlsManagerSettings)
#endif
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import qualified Log.Internal.Aeson.Compat as AC

-- | Configuration for the Elasticsearch 'Logger'. See
-- <https://www.elastic.co/guide/en/elasticsearch/reference/current/glossary.html>
-- for the explanation of terms.
data ElasticSearchConfig = ElasticSearchConfig
  { esServer        :: !T.Text -- ^ Elasticsearch server address.
  , esIndex         :: !T.Text -- ^ Elasticsearch index name.
  , esShardCount    :: !Int
    -- ^ Elasticsearch shard count for the named index.
    --
    -- @since 0.10.0.0
  , esReplicaCount  :: !Int
    -- ^ Elasticsearch replica count for the named index.
    --
    -- @since 0.10.0.0
  , esMapping       :: !T.Text
    -- ^ Elasticsearch mapping name (unused with ES >= 7.0.0)
  , esLogin         :: Maybe (T.Text, T.Text)
    -- ^ Elasticsearch basic authentication username and password.
  , esLoginInsecure :: !Bool
    -- ^ Allow basic authentication over non-TLS connections.
  } deriving (Eq, Show, Generic)

-- | Sensible defaults for 'ElasticSearchConfig'.
defaultElasticSearchConfig :: ElasticSearchConfig
defaultElasticSearchConfig = ElasticSearchConfig
  { esServer        = "http://localhost:9200"
  , esIndex         = "logs"
  , esShardCount    = 4
  , esReplicaCount  = 1
  , esMapping       = "log"
  , esLogin         = Nothing
  , esLoginInsecure = False
  }

----------------------------------------
-- ES communication

-- Most of the below code is taken from the bloodhound library
-- (https://github.com/bitemyapp/bloodhound).

data EsVersion = EsVersion !Int !Int !Int
  deriving (Eq, Ord)

parseEsVersion :: Value -> Maybe EsVersion
parseEsVersion js = do
  Object props <- pure js
  Object version <- "version" `AC.lookup` props
  case "distribution" `AC.lookup` version of
    Just "opensearch" -> do
      -- OpenSearch is compatible (so far) with esV7 mappings.
      pure esV7
    _ -> do
      String number <- "number" `AC.lookup` version
      [v1, v2, v3] <- mapM (maybeRead . T.unpack) $ T.splitOn "." number
      pure $ EsVersion v1 v2 v3
  where
    maybeRead s = do
      [(v, "")] <- pure $ reads s
      pure v

-- | Minimum version with split 'string' type.
esV5 :: EsVersion
esV5 = EsVersion 5 0 0

-- | Minimum version without mapping types.
esV7 :: EsVersion
esV7 = EsVersion 7 0 0

----------------------------------------

-- | Check the ElasticSearch server for info. Result can be fed to
-- 'parseEsVersion' to determine version of the server.
serverInfo :: EsEnv -> IO (Either HttpException (Response Value))
serverInfo env = try $ dispatch env methodGet [] Nothing

-- | Check that given index exists.
indexExists :: EsEnv -> T.Text -> IO Bool
indexExists env index =
  isSuccess <$> dispatch env methodHead [index] Nothing

-- | Create an index with given mapping.
createIndexWithMapping
  :: EsVersion
  -> EsEnv
  -> ElasticSearchConfig
  -> T.Text
  -> IO (Response Value)
createIndexWithMapping version env ElasticSearchConfig{..} index = do
  dispatch env methodPut [index] . Just . encode $ object
    [ "settings" .= object
      [ "number_of_shards" .= esShardCount
      , "number_of_replicas" .= esReplicaCount
      ]
    , "mappings" .= if version >= esV7
                    then logsMapping
                    else object [ AC.fromText esMapping .= logsMapping ]
    ]
  where
    logsMapping = object
      [ "properties" .= object
        [ "time" .= object
          [ "type"   .= timeTy
          , "format" .= ("date_time"::T.Text)
          ]
        , "domain" .= object
          [ "type" .= textTy
          ]
        , "level" .= object
          [ "type" .= textTy
          ]
        , "component" .= object
          [ "type" .= textTy
          ]
        , "message" .= object
          [ "type" .= textTy
          ]
        ]
      ]
      where
        timeTy :: T.Text
        timeTy = if version >= esV7
                 then "date_nanos"
                 else "date"

        textTy :: T.Text
        textTy = if version >= esV5
                 then "text"
                 else "string"

-- Index multiple log messages.
bulkIndex
  :: EsVersion
  -> EsEnv
  -> ElasticSearchConfig
  -> T.Text
  -> V.Vector Object
  -> IO (Response Value)
bulkIndex version env conf index objs = do
  dispatch env methodPost route . Just . BSB.toLazyByteString $ foldMap ixOp objs
  where
    route = if version >= esV7
            then [index, "_bulk"]
            else [index, esMapping conf, "_bulk"]

    ixOp obj = ixCmd
            <> BSB.char8 '\n'
            <> BSB.lazyByteString (encode $ Object obj)
            <> BSB.char8 '\n'
      where
        ixCmd = BSB.lazyByteString . encode $ object
          [ "index" .= object []
          ]

-- Refresh given index.
refreshIndex :: EsEnv -> T.Text -> IO ()
refreshIndex env index =
  void $ dispatch env methodPost [index, "_refresh"] Nothing

----------------------------------------

data EsEnv = EsEnv
  { envServer      :: !T.Text
  , envManager     :: !Manager
  , envRequestHook :: !(Request -> Request)
  }

mkEsEnv :: ElasticSearchConfig -> IO EsEnv
mkEsEnv ElasticSearchConfig{..} = do
#if OPENSSL
  envManager <- withOpenSSL newOpenSSLManager
#else
  envManager <- newManager tlsManagerSettings
#endif
  let envServer = esServer
      envRequestHook = maybe id mkAuthHook esLogin
  pure EsEnv{..}
  where
    mkAuthHook (u, p) = applyBasicAuth (T.encodeUtf8 u) (T.encodeUtf8 p)

----------------------------------------

dispatch :: EsEnv
         -> Method
         -> [T.Text]
         -> Maybe BSL.ByteString
         -> IO (Response Value)
dispatch EsEnv{..} dMethod url body = do
  initReq <- parseRequest $ T.unpack $ T.intercalate "/" $ envServer : url
  let req = envRequestHook . setRequestIgnoreStatus $ initReq
        { method = dMethod
        , requestBody = RequestBodyLBS $ fromMaybe BSL.empty body
        , requestHeaders = ("Content-Type", "application/json") : requestHeaders initReq
        }
  fmap decodeReply <$> httpLbs req envManager

decodeReply :: BSL.ByteString -> Value
decodeReply bs = case eitherDecode' bs of
  Right js  -> js
  Left  err -> object ["decoding_error" .= err]

isSuccess :: Response a -> Bool
isSuccess = statusCheck (inRange (200, 299))
  where
    statusCheck :: (Int -> Bool) -> Response a -> Bool
    statusCheck p = p . statusCode . responseStatus
