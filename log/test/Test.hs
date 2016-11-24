module Main where

import Log
import Log.Backend.ElasticSearch
import Test.ElasticSearch

import System.Random
import Test.Tasty
import Test.Tasty.HUnit

tests :: ElasticSearchTestConfig -> Logger -> TestTree
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

main :: IO ()
main = do
  let config = defaultElasticSearchConfig
  testConfig <- defaultElasticSearchTestConfig config
  withElasticSearchLogger config randomIO $ \logger ->
    defaultMain $ tests testConfig logger
