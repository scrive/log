module Main where

import Log
import Log.Backend.ElasticSearch

import Data.Aeson
import Data.Either (Either(..))
import Data.Text (Text)
import Data.Time
import Database.Bloodhound
import Network.HTTP.Client
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T

data TestConfig = TestConfig {
  testServer :: Server,
  testIndex  :: IndexName
  }

mkTestConfig :: ElasticSearchConfig -> IO TestConfig
mkTestConfig ElasticSearchConfig{..} = do
  now <- getCurrentTime
  let testServer  = Server esServer
      testIndex = IndexName $ T.concat
                  [ esIndex
                  , "-"
                  , T.pack $ formatTime defaultTimeLocale "%F" now
                  ]
  return TestConfig{..}


tests :: TestConfig -> Logger -> TestTree
tests config logger = testGroup "Unit Tests" [
  testCase "After logging 'foo', 'foo' is there" $ do
      runLogT "log-test" logger $ do
        logTrace_ "foo"
      waitForLogger logger
      hits <- getNumHits config "foo"
      assertBool "expected at least one hit for 'foo'" (hits > 0),
  testCase "After logging 'foo' thrice, searching 'foo' gives >=3 hits" $ do
      runLogT "log-test" logger $ do
        logTrace_ "foo"
        logTrace_ "foo"
        logTrace_ "foo"
      waitForLogger logger
      hits <- getNumHits config "foo"
      assertBool "expected at least 3 hits for 'foo'" (hits >= 3),
  testCase "After logging 'foo' and 'bar', searching 'baz' gives 0 hits" $ do
      runLogT "log-test" logger $ do
        logTrace_ "foo"
        logTrace_ "bar"
      waitForLogger logger
      hits <- getNumHits config "baz"
      assertBool "expected zero hits for 'baz'"  (hits == 0)
  ]

getNumHits :: TestConfig -> Text -> IO Int
getNumHits TestConfig{..} query = do
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

main :: IO ()
main = do
  let config = ElasticSearchConfig {
        esServer  = "http://localhost:9200",
        esIndex   = "logs",
        esMapping = "log"
        }
  logger <- elasticSearchLogger config randomIO
  testConfig <- mkTestConfig config
  defaultMain $ tests testConfig logger
