-- | Stdout logging back-end.
module Log.Backend.StandardOutput
  ( withSimpleStdOutLogger
  , withStdOutLogger
  ) where

import Prelude
import System.IO
import qualified Data.Text.IO as T

import Log.Data
import Log.Internal.Logger
import Log.Logger

-- | Create a simple, synchronous logger that prints messages to standard output
-- and flushes 'stdout' on each call to 'loggerWriteMessage' for the duration of
-- the given action.
withSimpleStdOutLogger :: (Logger -> IO r) -> IO r
withSimpleStdOutLogger = withLogger $ Logger
  { loggerWriteMessage = \msg -> do
      T.putStrLn $ showLogMessage Nothing msg
      hFlush stdout
  , loggerWaitForWrite = return ()
  , loggerShutdown     = return ()
  }

-- | Create a logger that prints messages to standard output for the duration of
-- the given action.
withStdOutLogger :: (Logger -> IO r) -> IO r
withStdOutLogger act = do
  logger <- mkLogger "stdout" $ \msg -> do
    T.putStrLn $ showLogMessage Nothing msg
    hFlush stdout
  withLogger logger act
