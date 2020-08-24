-- | Bulk stdout logging back-end, useful mainly for testing.
module Log.Backend.StandardOutput.Bulk
  ( withBulkStdOutLogger
  ) where

import Prelude
import qualified Data.Text.IO as T
import System.IO (hFlush, stdout)

import Log.Data
import Log.Logger
import Log.Internal.Logger

-- | Create an asynchronouis logger thread that prints messages to standard
-- output once per second for the duration of the given action. Flushes 'stdout'
-- on each bulk write.
withBulkStdOutLogger :: (Logger -> IO r) -> IO r
withBulkStdOutLogger act = do
  logger <- mkBulkLogger "stdout-bulk"
    (\msgs -> do
        mapM_ (T.putStrLn . showLogMessage Nothing) msgs
        hFlush stdout
    ) (return ())
  withLogger logger act
