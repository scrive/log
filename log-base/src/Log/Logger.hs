-- | The 'Logger' type of logging back-ends.
module Log.Logger
  ( LoggerEnv(..)
  , Logger
  , mkLogger
  , mkBulkLogger
  , mkBulkLogger'
  , execLogger
  , waitForLogger
  , shutdownLogger
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Semigroup
import Prelude
import qualified Data.Aeson.Types as A
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Log.Data
import Log.Internal.Logger

-- | The state that every 'LogT' carries around.
data LoggerEnv = LoggerEnv
  { leLogger    :: !Logger   -- ^ The 'Logger' to use.
  , leComponent :: !T.Text   -- ^ Current application component.
  , leDomain    :: ![T.Text] -- ^ Current application domain.
  , leData      :: ![A.Pair] -- ^ Additional data to be merged with the log
                             -- message\'s data.
  }

-- | Start a logger thread that consumes one queued message at a time.
mkLogger :: T.Text -> (LogMessage -> IO ()) -> IO Logger
mkLogger name exec = mkLoggerImpl
  newTQueueIO isEmptyTQueue readTQueue writeTQueue (return ())
  name exec (return ())

-- | Start an asynchronous logger thread that consumes all queued
-- messages once per second. Uses a bounded queue internally to avoid
-- space leaks. To make sure that the messages get written out in the
-- presence of exceptions, use high-level wrappers like 'withLogger',
-- 'Log.Backend.ElasticSearch.withElasticSearchLogger' or
-- 'Log.Backend.StandardOutput.Bulk.withBulkStdOutLogger' instead of
-- this function directly.
--
-- Note: some messages can be lost when the main thread shuts down
-- without making sure that all logger threads have written out all
-- messages, because in that case child threads are not given a chance
-- to clean up by the RTS. This is apparently a feature:
-- <https://mail.haskell.org/pipermail/haskell-cafe/2014-February/112754.html>
--
-- To work around this issue, make sure that the main thread doesn't
-- exit until all its children have terminated. The 'async' package
-- makes this easy.
--
-- Problematic example:
--
-- @
-- import Control.Concurrent.Async
--
-- main :: IO ()
-- main = do
--    logger \<- 'Log.Backend.ElasticSearch.elasticSearchLogger'
--    a \<- 'Control.Concurrent.Async.async' ('Log.Backend.ElasticSearch.withElasticSearchLogger' $ \\logger ->
--                'Log.Monad.runLogT' "main" logger $ 'Log.Class.logTrace_' "foo")
--    -- Main thread exits without waiting for the child
--    -- to finish and without giving the child a chance
--    -- to do proper cleanup.
-- @
--
-- Fixed example:
--
-- @
-- import Control.Concurrent.Async
--
-- main :: IO ()
-- main = do
--    logger \<- 'Log.Backend.ElasticSearch.elasticSearchLogger'
--    a \<- 'Control.Concurrent.Async.async' ('Log.Backend.ElasticSearch.withElasticSearchLogger' $ \\logger ->
--                'Log.Monad.runLogT' "main" logger $ 'Log.Class.logTrace_' "foo")
--    'Control.Concurrent.Async.wait' a
--    -- Main thread waits for the child to finish, giving
--    -- it a chance to shut down properly. This works even
--    -- in the presence of exceptions in the child thread.
-- @
mkBulkLogger :: T.Text -> ([LogMessage] -> IO ()) -> IO () -> IO Logger
mkBulkLogger = mkBulkLogger' sbDefaultCapacity 1000000

-- | Like 'mkBulkLogger', but with configurable queue size and thread delay.
--
-- @since 0.7.4.0
mkBulkLogger'
    :: Int                      -- ^ queue capacity (default 1000000)
    -> Int                      -- ^ thread delay (microseconds, default 1000000)
    -> T.Text                   -- ^ logger name
    -> ([LogMessage] -> IO ())  -- ^ write
    -> IO ()                    -- ^ flush
    -> IO Logger
mkBulkLogger' cap dur = mkLoggerImpl
  (newSBQueueIO cap) isEmptySBQueue readSBQueue writeSBQueue
  (threadDelay dur)

----------------------------------------

-- | A simple STM based bounded queue.
data SBQueue a = SBQueue !(TVar [a]) !(TVar Int) !Int

-- | Default capacity of a 'SBQueue'. This corresponds to
-- approximately 200 MiB memory residency when the queue is full.
sbDefaultCapacity :: Int
sbDefaultCapacity = 1000000

-- | Create an instance of 'SBQueue' with a given capacity.
newSBQueueIO :: Int -> IO (SBQueue a)
newSBQueueIO capacity = SBQueue <$> newTVarIO [] <*> newTVarIO 0 <*> pure capacity

-- | Check if an 'SBQueue' is empty.
isEmptySBQueue :: SBQueue a -> STM Bool
isEmptySBQueue (SBQueue queue count _capacity) = do
  isEmpty  <- null <$> readTVar queue
  numElems <- readTVar count
  assert (if isEmpty then numElems == 0 else numElems > 0) $
    return isEmpty

-- | Read all the values stored in an 'SBQueue'.
readSBQueue :: SBQueue a -> STM [a]
readSBQueue (SBQueue queue count _capacity) = do
  elems <- readTVar queue
  when (null elems) retry
  writeTVar queue []
  writeTVar count 0
  return $ reverse elems

-- | Write a value to an 'SBQueue'.
writeSBQueue :: SBQueue a -> a -> STM ()
writeSBQueue (SBQueue queue count capacity) a = do
  numElems <- readTVar count
  if numElems < capacity
    then do modifyTVar queue (a :)
            modifyTVar count (+1)
    else return ()

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
  queue      <- newQueue
  inProgress <- newTVarIO False
  isRunning  <- newTVarIO True
  tid <- forkFinally (forever $ loop queue inProgress)
                     (\_ -> cleanup queue inProgress)
  return Logger {
    loggerWriteMessage = \msg -> atomically $ do
        checkIsRunning isRunning
        writeQueue queue msg,
    loggerWaitForWrite = do
        atomically $ waitForWrite queue inProgress
        sync,
    loggerShutdown     = do
        killThread tid
        atomically $ writeTVar isRunning False
    }
  where
    checkIsRunning isRunning' = do
      isRunning <- readTVar isRunning'
      when (not isRunning) $
        throwSTM (AssertionFailed $ "Log.Logger.mkLoggerImpl: "
                   ++ "attempt to write to a shut down logger")

    loop queue inProgress = do
      step queue inProgress
      afterExecDo

    step queue inProgress = do
      msgs <- atomically $ do
        writeTVar inProgress True
        readQueue queue
      exec msgs
      atomically $ writeTVar inProgress False

    cleanup queue inProgress = do
      step queue inProgress
      sync
      -- Don't call afterExecDo, since it's either a no-op or a
      -- threadDelay.
      printLoggerTerminated

    waitForWrite queue inProgress = do
      isEmpty <- isQueueEmpty queue
      isInProgress <- readTVar inProgress
      when (not isEmpty || isInProgress) retry

    printLoggerTerminated = T.putStrLn $ name <> ": logger thread terminated"
