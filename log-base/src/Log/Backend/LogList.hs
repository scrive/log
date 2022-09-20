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
import Control.Monad.IO.Unlift

import Log.Data
import Log.Internal.Logger

newtype LogList = LogList (MVar [LogMessage])
  deriving Eq

-- | Create a new, empty list.
newLogList :: MonadIO m => m LogList
newLogList = LogList <$> liftIO (newMVar [])

-- | Retrieve messages stored in the list.
getLogList :: MonadIO m => LogList -> m [LogMessage]
getLogList (LogList ll) = reverse <$> liftIO (readMVar ll)

-- | Put a message into the list.
putLogList :: MonadIO m => LogList -> LogMessage -> m ()
putLogList (LogList ll) msg = liftIO . modifyMVar_ ll $ \msgs -> return $! msg : msgs

-- | Clear the list.
clearLogList :: MonadIO m => LogList -> m ()
clearLogList (LogList ll) = liftIO . modifyMVar_ ll . const $ return []

-- | Creates a logger that stores messages in the given 'LogList'.
withLogListLogger :: MonadUnliftIO m => LogList -> (Logger -> m r) -> m r
withLogListLogger ll act = withRunInIO $ \unlift -> withLogger logger (unlift . act)
  where
    logger = Logger
      { loggerWriteMessage = putLogList ll
      , loggerWaitForWrite = return ()
      , loggerShutdown     = return ()
      }
