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
import Control.Monad
import Data.Monoid
import Prelude
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Log.Data
import Log.Internal.Logger

-- | Start a logger thread that consumes one queued message at a time.
mkLogger :: T.Text -> (LogMessage -> IO ()) -> IO Logger
mkLogger name exec = mkLoggerImpl
  newTQueueIO isEmptyTQueue readTQueue writeTQueue (return ())
  name exec (return ())

-- | Start an asynchronous logger thread that consumes all queued
-- messages once per second.
--
-- Note: some messages can be lost when the main thread shuts down
-- without making sure that its children have finished because in that
-- case child threads are not given a chance to clean up by the
-- RTS. This is apparently a feature:
-- <https://mail.haskell.org/pipermail/haskell-cafe/2014-February/112754.html>
--
-- To work around it, make sure that child threads that write to the
-- log finish before the main thread. If you're passing a 'Logger'
-- object around explicitly instead of using 'runLogT', you must also
-- add a @`finally` waitForLogger logger@ block after you're done with
-- the logger ('runLogT' does this automatically).
--
-- Problematic example:
--
-- @
-- main :: IO ()
-- main = do
--    logger <- mkLogger
--    void $ forkIO (runLogT "main" logger $ logTrace_ "foo")
--    -- Main thread exits, child thread is aborted
--    -- without a chance to do cleanup.
-- @
--
-- Fixed example:
--
-- @
-- import Control.Concurrent.Async
--
-- main :: IO ()
-- main = do
--    logger <- mkLogger
--    a <- async (runLogT "main" logger $ logTrace_ "foo")
--    wait a
--    -- Main thread waits for its child
--    -- to finish and do cleanup.
-- @
mkBulkLogger :: T.Text -> ([LogMessage] -> IO ()) -> IO () -> IO Logger
mkBulkLogger = mkLoggerImpl
  newSQueueIO isEmptySQueue readSQueue writeSQueue (threadDelay 1000000)

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
             -> IO ()
             -> IO Logger
mkLoggerImpl newQueue isQueueEmpty readQueue writeQueue afterExecDo
  name exec sync = do
  (queue, inProgress) <- (,) <$> newQueue <*> newTVarIO False
  void $ forkFinally (forever $ loop queue inProgress)
                     (\_ -> printLoggerTerminated)
  return Logger {
    loggerWriteMessage = atomically . writeQueue queue,
    loggerWaitForWrite = do atomically $ waitForWrite queue inProgress
                            sync
    }
  where
    loop queue inProgress = do
      step queue inProgress
      afterExecDo

    step queue inProgress = do
      msgs <- atomically $ do
        writeTVar inProgress True
        readQueue queue
      exec msgs
      atomically $ writeTVar inProgress False

    waitForWrite queue inProgress = do
      isEmpty <- isQueueEmpty queue
      isInProgress <- readTVar inProgress
      when (not isEmpty || isInProgress) retry

    printLoggerTerminated = T.putStrLn $ name <> ": logger thread terminated"
