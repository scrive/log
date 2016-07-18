module Log.Backend.StandardOutput (
    simpleStdoutLogger
  , stdoutLogger
  ) where

import Prelude
import qualified Data.Text.IO as T

import Log.Data
import Log.Internal.Logger
import Log.Logger

-- | Simple, synchronous logger that prints messages to standard output.
simpleStdoutLogger :: Logger
simpleStdoutLogger = Logger {
    loggerWriteMessage = T.putStrLn . showLogMessage Nothing
  , loggerWaitForWrite = return ()
  , loggerFinalizers   = []
  }

-- | Create logger that prints messages to standard output.
stdoutLogger :: IO Logger
stdoutLogger = mkLogger "stdout" $ T.putStrLn . showLogMessage Nothing
