{-# LANGUAGE RecordWildCards #-}
module Log.Internal.Logger (
    Logger(..)
  , execLogger
  , waitForLogger
  ) where

import Control.Concurrent.STM
import Data.IORef
import Data.Monoid

import Log.Data

-- | Data type representing logger.
data Logger = Logger {
  loggerWriteMessage :: !(LogMessage -> IO ())
, loggerWaitForWrite :: !(STM ())
, loggerFinalizers   :: ![IORef ()]
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
