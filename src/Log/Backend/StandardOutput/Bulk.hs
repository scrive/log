-- | Bulk stdout logging back-end, useful mainly for testing.
module Log.Backend.StandardOutput.Bulk (
    bulkStdoutLogger
  ) where

import Prelude
import qualified Data.Text.IO as T

import Log.Data
import Log.Internal.Logger
import Log.Logger

-- | Start an asynchronous logger thread that prints messages to
-- standard output.
--
-- Implemented using 'mkBulkLogger', see the note attached to that
-- function.
bulkStdoutLogger :: IO Logger
bulkStdoutLogger = mkBulkLogger "stdout-bulk"
                   (mapM_ $ T.putStrLn . showLogMessage Nothing)
                   (return ())
