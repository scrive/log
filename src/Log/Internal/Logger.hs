-- | The 'Logger' type - implementation details.
{-# OPTIONS_HADDOCK hide #-}
module Log.Internal.Logger (
    Logger(..)
  , execLogger
  , waitForLogger
  ) where

import Control.Concurrent.STM
import Data.IORef
import Data.Monoid
import Prelude

import Log.Data

-- | A logging back-end that outputs 'LogMessage's using
-- e.g. PostgreSQL, Elasticsearch or stdout.
data Logger = Logger {
  loggerWriteMessage :: !(LogMessage -> IO ()) -- ^ Output a 'LogMessage'.
, loggerWaitForWrite :: !(STM ())
                     -- ^ Wait for the logger to output all messages
                     -- in its input queue (in the case logging is
                     -- done asynchronously).
, loggerFinalizers   :: ![IORef ()]
                     -- ^ When this list is garbage collected, the
                     -- finalizers associated with each 'IORef' are
                     -- run, ensuring that the logger shuts down
                     -- properly.
}

-- | Execute logger to serialize a 'LogMessage'.
execLogger :: Logger -> LogMessage -> IO ()
execLogger Logger{..} = loggerWriteMessage

-- | Wait until logs stored in an internal queue are serialized.
waitForLogger :: Logger -> IO ()
waitForLogger Logger{..} = atomically loggerWaitForWrite

-- | Composition of 'Logger' objects.
instance Monoid Logger where
  mempty = Logger (const $ return ()) (return ()) []
  l1 `mappend` l2 = Logger {
    loggerWriteMessage = \msg -> do
      loggerWriteMessage l1 msg
      loggerWriteMessage l2 msg
  , loggerWaitForWrite = do
      loggerWaitForWrite l1
      loggerWaitForWrite l2
  , loggerFinalizers = loggerFinalizers l1 ++ loggerFinalizers l2
  }
