-- | LogList logging backed.
module Log.Backend.LogList
  ( LogList
  , newLogList
  , getLogList
  , putLogList
  , clearLogList
  , withLogListLogger
  ) where

import Control.Concurrent.MVar
import System.IO
import Prelude

import Log.Data
import Log.Internal.Logger

newtype LogList = LogList (MVar [LogMessage])
  deriving Eq

-- | Create a new, empty list.
newLogList :: IO LogList
newLogList = LogList <$> newMVar []

-- | Retrieve messages stored in the list.
getLogList :: LogList -> IO [LogMessage]
getLogList (LogList ll) = reverse <$> readMVar ll

-- | Put a message into the list.
putLogList :: LogList -> LogMessage -> IO ()
putLogList (LogList ll) msg = modifyMVar_ ll $ \msgs -> return $! msg : msgs

-- | Clear the list.
clearLogList :: LogList -> IO ()
clearLogList (LogList ll) = modifyMVar_ ll . const $ return []

-- | Creates a logger that stores messages in the given 'LogList'.
withLogListLogger :: LogList -> (Logger -> IO r) -> IO r
withLogListLogger ll = withLogger $ Logger
  { loggerWriteMessage = putLogList ll
  , loggerWaitForWrite = return ()
  , loggerShutdown     = return ()
  }
