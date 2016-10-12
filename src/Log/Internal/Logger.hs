-- | The 'Logger' type - implementation details.
{-# OPTIONS_HADDOCK hide #-}
module Log.Internal.Logger (
    Logger(..)
  , execLogger
  , waitForLogger
  ) where

import Data.Monoid
import Prelude

import Log.Data

-- | A logging back-end that outputs 'LogMessage's using
-- e.g. PostgreSQL, Elasticsearch or stdout.
data Logger = Logger {
  loggerWriteMessage :: !(LogMessage -> IO ()) -- ^ Output a 'LogMessage'.
, loggerWaitForWrite :: !(IO ())
                     -- ^ Wait for the logger to output all messages
                     -- in its input queue (in the case logging is
                     -- done asynchronously).
}

-- | Execute logger to serialize a 'LogMessage'.
execLogger :: Logger -> LogMessage -> IO ()
execLogger Logger{..} = loggerWriteMessage

-- | Wait until logs stored in an internal queue are serialized.
waitForLogger :: Logger -> IO ()
waitForLogger Logger{..} = loggerWaitForWrite

-- | Composition of 'Logger' objects.
instance Monoid Logger where
  mempty = Logger (const $ return ()) (return ())
  l1 `mappend` l2 = Logger {
    loggerWriteMessage = \msg -> do
      loggerWriteMessage l1 msg
      loggerWriteMessage l2 msg
  , loggerWaitForWrite = do
      loggerWaitForWrite l1
      loggerWaitForWrite l2
  }
