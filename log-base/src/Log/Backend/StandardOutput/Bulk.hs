-- | Bulk stdout logging back-end.
module Log.Backend.StandardOutput.Bulk
  ( withBulkStdOutLogger
  , withBulkJsonStdOutLogger
  ) where

import Data.Aeson
import Prelude
import System.IO (hFlush, stdout)
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as BSL

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

-- | Create a bulk logger that prints messages in the JSON format to standard
-- output once per second for the duration of the given action. Flushes 'stdout'
-- on each bulk write.
withBulkJsonStdOutLogger :: (Logger -> IO r) -> IO r
withBulkJsonStdOutLogger act = do
  logger <- mkBulkLogger "stdout-bulk-json"
    (\msgs -> do
        mapM_ (BSL.putStrLn . encode) msgs
        hFlush stdout
    ) (return ())
  withLogger logger act
