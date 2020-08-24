-- | Bulk stdout logging back-end, useful mainly for testing.
module Log.Backend.StandardOutput.Bulk (
    withBulkStdOutLogger,
    bulkStdoutLogger
  ) where

import Prelude
import qualified Data.Text.IO as T
import System.IO (hFlush, stdout)

import Log.Data
import Log.Logger
import Log.Internal.Logger

-- | Create a 'bulkStdoutLogger' for the duration of the given action,
-- and shut it down afterwards, making sure that all buffered messages
-- are actually written to stdout. Flushes 'stdout' on each bulk write.
withBulkStdOutLogger :: (Logger -> IO r) -> IO r
withBulkStdOutLogger act = do
  logger <- bulkStdoutLogger
  withLogger logger act

{-# DEPRECATED bulkStdoutLogger "Use 'withBulkStdOutLogger' instead!" #-}

-- | Start an asynchronous logger thread that prints messages to
-- standard output.
--
-- Please use 'withBulkStdOutLogger'' instead, which is more exception-safe
-- (see the note attached to 'mkBulkLogger').
bulkStdoutLogger :: IO Logger
bulkStdoutLogger = mkBulkLogger "stdout-bulk"
  (\msgs -> do
      mapM_ (T.putStrLn . showLogMessage Nothing) msgs
      hFlush stdout
  ) (return ())
