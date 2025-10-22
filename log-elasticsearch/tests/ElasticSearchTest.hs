module ElasticSearchTest where

import Control.Exception
import Data.Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time
import Data.Traversable (for)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Log
import Log.Backend.ElasticSearch
import Log.Backend.ElasticSearch.Internal
import Network.HTTP.Client
import Network.HTTP.Types
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout
import Test.Tasty hiding (Timeout)
import Test.Tasty.HUnit

-- NOTE: The test suite assumes a running instance of elasticsearch. Each test
-- suite then interacts against this instance with unique global indices / date
-- info. The run id and run date globals are fixed for the duration of the run.

testRunId :: UUID.UUID
{-# NOINLINE testRunId #-}
testRunId = unsafePerformIO UUID.nextRandom

testRunDate :: UTCTime
{-# NOINLINE testRunDate #-}
testRunDate = unsafePerformIO getCurrentTime

-- | Key we keep constant across all indices we use in tests
indexTestKey :: T.Text
indexTestKey = "-log-elasticsearch-"

test_elasticSearchLogger :: TestTree
test_elasticSearchLogger = do
  -- Clear the elasticsearch instance of all test indices we are known to use
  before_ (resetTestIndices (testConfig 0)) $
    testGroup
      "elasticSearchLogger"
      [ testCase "can log to elasticsearch" $ do
          let config = testConfig 0
          withElasticSearchLogger config $ \logger -> do
            runLogT "test" logger LogInfo $ do
              logInfo_ "test-message"
          logs <- readElasticSearchLogs config
          assertEqual "found expected log messages" logs ["test-message"]
      , testCase "logging creates the expected index" $ do
          -- I think there's a race condition here where the logging action and
          -- the fetch request happen on different days. Should be fine :shrug:.
          let config = testConfig 1
          withElasticSearchLogger config $ \logger -> do
            runLogT "test" logger LogInfo $ do
              logInfo_ "test-message"

          indices <- getIndices config
          assertBool "found expected logged indices" (loggedTestIndex config `Set.member` indices)
      , testCase "does not hang with no available instance" $ do
          let config = (testConfig 2) {esServer = "http://127.0.0.1:0"}
              success (_ :: SomeException) = pure (Just ())

          result <- handle success $ timeout 10_000_000 $ do
            withElasticSearchLogger config $ \logger ->
              runLogT "test" logger LogInfo $
                logInfo_ "test-message"
            assertFailure "should have failed with an exception"
          assertEqual "should not have timed out" result (Just ())
      ]
  where
    testIndex ix = T.pack (show testRunId) <> indexTestKey <> T.pack (show (ix :: Int))
    testConfig ix = defaultElasticSearchConfig {esIndex = testIndex ix}
    before_ f = withResource f (const (pure ())) . const

loggedTestIndex :: ElasticSearchConfig -> T.Text
loggedTestIndex config = esIndex config <> "-" <> T.pack (formatTime defaultTimeLocale "%F" testRunDate)

readElasticSearchLogs :: ElasticSearchConfig -> IO [T.Text]
readElasticSearchLogs config = do
  -- TODO: Might be better to reuse this environment across tests.
  esEnv <- mkEsEnv config
  response <-
    dispatch esEnv methodGet [loggedTestIndex config, "_search"] $
      Just "{\"query\":{\"match_all\":{}}}"
  let parser v = flip (withObject ".hits") v $ \obj -> do
        hits <- obj .: "hits"
        msgs <- hits .: "hits"
        for msgs $ \msg -> do
          source <- msg .: "_source"
          source .: "message"

  runParser parser (responseBody response)

getIndices :: ElasticSearchConfig -> IO (Set T.Text)
getIndices config = do
  -- TODO: Might be better to reuse this environment across tests.
  esEnv <- mkEsEnv config
  response <- dispatch esEnv methodGet ["_aliases"] Nothing
  let parser v = flip (withObject "_aliases.keys") v $ \obj ->
        pure $ map fst $ KM.toList obj
  Set.fromList . map Key.toText <$> runParser parser (responseBody response)

runParser :: Applicative f => (a -> Parser b) -> a -> f b
runParser parser body = case parse parser body of
  Error b -> error ("incorrect parser: " <> b)
  Success s -> pure s

resetTestIndices :: ElasticSearchConfig -> IO ()
resetTestIndices config = do
  -- TODO: Might be better to reuse this environment across tests, to reuse
  -- HTTPManager and such.
  esEnv <- mkEsEnv config
  _ <- dispatch esEnv methodDelete ["*" <> indexTestKey <> "*"] Nothing
  pure ()
