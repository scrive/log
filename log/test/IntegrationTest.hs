module Main where

import Log
import Log.Backend.StandardOutput
import Log.Backend.StandardOutput.Bulk
import Log.Backend.ElasticSearch.V5
import Test.ElasticSearch

import Data.List
import System.Environment
import System.Process
import System.Random
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test-simple-stdout"] -> do
      withSimpleStdOutLogger $ \logger ->
        runLogT "log-test-integration" logger $ logTrace_ "kaboozle"
    ["test-bulk-stdout"] -> do
      withBulkStdOutLogger $ \logger ->
        runLogT "log-test-integration" logger $ logTrace_ "kaboozle"
    ["test-elasticsearch"] -> do
      let config = defaultElasticSearchConfig
      withElasticSearchLogger config randomIO $ \logger ->
        runLogT "log-test-integration" logger $ logTrace_ "kaboozle"
    _ -> runTests

runTests :: IO ()
runTests = do
  path <- getExecutablePath
  let config = defaultElasticSearchConfig
  testConfig <- defaultElasticSearchTestConfig config
  defaultMain $ testGroup "Integration Tests" [
    testCase "Log messages are not lost (simple stdout back-end)" $ do
        out <- readProcess path ["test-simple-stdout"] ""
        assertBool "Output doesn't contain 'kaboozle'"
          $ "kaboozle" `isInfixOf` out,

    testCase "Log messages are not lost (bulk stdout back-end)" $ do
        out <- readProcess path ["test-bulk-stdout"] ""
        assertBool "Output doesn't contain 'kaboozle'"
          $ "kaboozle" `isInfixOf` out,

    testCase "Log messages are not lost (Elasticsearch back-end)" $ do
        callProcess path ["test-elasticsearch"]
        refreshTestIndex testConfig
        hits <- getNumHits testConfig "kaboozle"
        assertBool "Elasticsearch returned zero hits for 'kaboozle'"
          $ (hits > 0)
    ]
