-- | Bulk stdout logging back-end, useful mainly for testing.
module Log.Backend.StandardOutput.Bulk (
    withBulkStdOutLogger,
    bulkStdoutLogger
  ) where

import Control.Exception
import Prelude
import qualified Data.Text.IO as T

import Log.Data
import Log.Logger

-- | Create a 'bulkStdoutLogger' for the duration of the given action,
-- and shut it down afterwards, making sure that all buffered messages
-- are actually written to stdout.
withBulkStdOutLogger :: (Logger -> IO r) -> IO r
withBulkStdOutLogger act = do
  logger <- bulkStdoutLogger
  (act logger) `finally` (do { waitForLogger logger; shutdownLogger logger; })

{-# DEPRECATED bulkStdoutLogger "Use 'withBulkStdOutLogger' instead!" #-}

-- | Start an asynchronous logger thread that prints messages to
-- standard output.
--
-- Please use 'withBulkStdOutLogger'' instead, which is more exception-safe
-- (see the note attached to 'mkBulkLogger').
bulkStdoutLogger :: IO Logger
bulkStdoutLogger = mkBulkLogger "stdout-bulk"
                   (mapM_ $ T.putStrLn . showLogMessage Nothing)
                   (return ())
