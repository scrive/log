module Test.ElasticSearch
  (ElasticSearchTestConfig(..)
  ,defaultElasticSearchConfig
  ,defaultElasticSearchTestConfig
  ,getNumHits
  ,refreshTestIndex)
where

import Log.Backend.ElasticSearch

import Control.Monad
import Data.Aeson
import Data.Text (Text)
import Data.Time
import Database.Bloodhound
import Network.HTTP.Client
import Test.Tasty.HUnit

import qualified Data.Text as T

defaultElasticSearchConfig :: ElasticSearchConfig
defaultElasticSearchConfig = ElasticSearchConfig {
  esServer  = "http://localhost:9200",
  esIndex   = "logs",
  esMapping = "log"
  }

data ElasticSearchTestConfig = ElasticSearchTestConfig {
  testServer :: Server,
  testIndex  :: IndexName
  }

defaultElasticSearchTestConfig :: ElasticSearchConfig
                               -> IO ElasticSearchTestConfig
defaultElasticSearchTestConfig ElasticSearchConfig{..} = do
  now <- getCurrentTime
  let testServer = Server esServer
      testIndex  = IndexName $ T.concat
                   [ esIndex
                   , "-"
                   , T.pack $ formatTime defaultTimeLocale "%F" now
                   ]
  return ElasticSearchTestConfig{..}


getNumHits :: ElasticSearchTestConfig -> Text -> IO Int
getNumHits ElasticSearchTestConfig{..} query = do
  let tquery = TermQuery (Term "message" query) Nothing
      search = mkSearch (Just tquery) Nothing
  reply <- withBH' $ searchByIndex testIndex search
  let result = eitherDecode (responseBody reply)
        :: Either String (SearchResult Value)
  case result of
    Left err -> assertString err >> return (-1)
    Right res -> return . hitsTotal . searchHits $ res

  where
    withBH' = withBH defaultManagerSettings testServer

refreshTestIndex :: ElasticSearchTestConfig -> IO ()
refreshTestIndex ElasticSearchTestConfig{..} =
  void . withBH' $ refreshIndex testIndex

  where
    withBH' = withBH defaultManagerSettings testServer
