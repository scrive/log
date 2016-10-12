module Main where

import Log
import Log.Backend.StandardOutput
import Log.Backend.StandardOutput.Bulk

import Data.List

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
      runLogT "main" logger $ do
        logTrace_ "foo"
    ["test-bulk-stdout"] -> do
      logger <- bulkStdoutLogger
      runLogT "main" logger $ do
        logTrace_ "foo"
    _ -> do
      path <- getExecutablePath
      defaultMain $ testGroup "Integration Tests" [
        testCase "Log messages are not lost (simple stdout back-end)" $ do
            out <- readProcess path ["test-simple-stdout"] ""
            assertBool "Output doesn't contain 'foo'" $ "foo" `isInfixOf` out,

        testCase "Log messages are not lost (bulk stdout back-end)" $ do
            out <- readProcess path ["test-bulk-stdout"] ""
            assertBool "Output doesn't contain 'foo'" $ "foo" `isInfixOf` out
        ]
