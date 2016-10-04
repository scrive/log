-- | The 'Logger' type of logging back-ends.
module Log.Logger (
    Logger
  , mkLogger
  , mkBulkLogger
  , execLogger
  , waitForLogger
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Monoid
import Prelude
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Log.Data
import Log.Internal.Logger

-- | Create a 'Logger' that consumes one queued message at a time.
mkLogger :: T.Text -> (LogMessage -> IO ()) -> IO Logger
mkLogger = mkLoggerImpl
  newTQueueIO isEmptyTQueue readTQueue writeTQueue $ return ()

-- | Create a 'Logger' that consumes all queued messages once per second.
mkBulkLogger :: T.Text -> ([LogMessage] -> IO ()) -> IO Logger
mkBulkLogger = mkLoggerImpl
  newSQueueIO isEmptySQueue readSQueue writeSQueue $ threadDelay 1000000

----------------------------------------

-- | A simple STM based queue.
newtype SQueue a = SQueue (TVar [a])

-- | Create an instance of 'SQueue'.
newSQueueIO :: IO (SQueue a)
newSQueueIO = SQueue <$> newTVarIO []

-- | Check if an 'SQueue' is empty.
isEmptySQueue :: SQueue a -> STM Bool
isEmptySQueue (SQueue queue) = null <$> readTVar queue

-- | Read all the values stored in an 'SQueue'.
readSQueue :: SQueue a -> STM [a]
readSQueue (SQueue queue) = do
  elems <- readTVar queue
  when (null elems) retry
  writeTVar queue []
  return $ reverse elems

-- | Write a value to an 'SQueue'.
writeSQueue :: SQueue a -> a -> STM ()
writeSQueue (SQueue queue) a = modifyTVar queue (a :)

----------------------------------------

mkLoggerImpl :: IO queue
             -> (queue -> STM Bool)
             -> (queue -> STM msgs)
             -> (queue -> LogMessage -> STM ())
             -> IO ()
             -> T.Text
             -> (msgs -> IO ())
             -> IO Logger
mkLoggerImpl newQueue isQueueEmpty readQueue writeQueue afterExecDo name exec = do
  (queue, inProgress) <- (,) <$> newQueue <*> newTVarIO False
  finalizer <- newIORef ()
  mask $ \release -> do
    tid <- forkIO . (`finally` printLoggerTerminated) . release . forever $ do
      msgs <- atomically $ do
        writeTVar inProgress True
        readQueue queue
      exec msgs
      atomically $ writeTVar inProgress False
      afterExecDo
    let waitForWrite = do
          isEmpty <- isQueueEmpty queue
          isInProgress <- readTVar inProgress
          when (not isEmpty || isInProgress) retry
    void . mkWeakIORef finalizer $ do
      atomically waitForWrite
      killThread tid
    return Logger {
      loggerWriteMessage = atomically . writeQueue queue
    , loggerWaitForWrite = waitForWrite
    , loggerFinalizers = [finalizer]
    }
  where
    printLoggerTerminated = T.putStrLn $ name <> ": logger thread terminated"
