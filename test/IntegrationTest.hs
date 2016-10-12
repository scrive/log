module Main where

import Log
import Log.Backend.StandardOutput
import Log.Backend.StandardOutput.Bulk

import Data.List

import Control.Concurrent.Async
import Test.Tasty
import Test.Tasty.HUnit
import System.Environment
import System.Process

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test-simple-stdout"] -> do
      logger <- stdoutLogger
      a <- async (runLogT "main" logger $ logTrace_ "foo")
      wait a
    ["test-bulk-stdout"] -> do
      logger <- bulkStdoutLogger
      a <- async (runLogT "main" logger $ logTrace_ "foo")
      wait a
    _ -> runTests

runTests :: IO ()
runTests = do
  path <- getExecutablePath
  defaultMain $ testGroup "Integration Tests" [
    testCase "Log messages are not lost (simple stdout back-end)" $ do
        out <- readProcess path ["test-simple-stdout"] ""
        assertBool "Output doesn't contain 'foo'" $ "foo" `isInfixOf` out,

    testCase "Log messages are not lost (bulk stdout back-end)" $ do
        out <- readProcess path ["test-bulk-stdout"] ""
        assertBool "Output doesn't contain 'foo'" $ "foo" `isInfixOf` out
    ]
