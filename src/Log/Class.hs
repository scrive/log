-- | The 'MonadLog' type class of monads with logging capabilities.
{-# OPTIONS_GHC -fno-warn-deprecated-flags #-}
{-# LANGUAGE OverlappingInstances #-}
module Log.Class (
    UTCTime
  , MonadTime(..)
  , MonadLog(..)
  , logAttention
  , logInfo
  , logTrace
  , logAttention_
  , logInfo_
  , logTrace_
  ) where

import Control.Monad.Time
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Aeson.Types
import Data.Time
import Prelude
import qualified Data.Text as T

import Log.Data

-- | Represents the family of monads with logging capabilities. Each
-- 'MonadLog' carries with it some associated state (the logging
-- environment) that can be modified locally with 'localData' and
-- 'localDomain'.
class MonadTime m => MonadLog m where
  -- | Write a message to the log.
  logMessage  :: UTCTime  -- ^ Time of the event.
              -> LogLevel -- ^ Log level.
              -> T.Text   -- ^ Log message.
              -> Value    -- ^ Additional data associated with the message.
              -> m ()
  -- | Extend the additional data associated with each log message locally.
  localData   :: [Pair] -> m a -> m a
  -- | Extend the current application domain locally.
  localDomain :: T.Text -> m a -> m a

-- | Generic, overlapping instance.
instance (
    MonadLog m
  , Monad (t m)
  , MonadTransControl t
  ) => MonadLog (t m) where
    logMessage time level message = lift . logMessage time level message
    localData data_ m = controlT $ \run -> localData data_ (run m)
    localDomain domain m = controlT $ \run -> localDomain domain (run m)

controlT :: (MonadTransControl t, Monad (t m), Monad m)
         => (Run t -> m (StT t a)) -> t m a
controlT f = liftWith f >>= restoreT . return

----------------------------------------

-- | Log a message and its associated data using current time as the
-- event time and the 'LogAttention' log level.
logAttention :: MonadLog m => T.Text -> Value -> m ()
logAttention = logNow LogAttention

-- | Log a message and its associated data using current time as the
-- event time and the 'LogInfo' log level.
logInfo :: MonadLog m => T.Text -> Value -> m ()
logInfo = logNow LogInfo

-- | Log a message and its associated data using current time as the
-- event time and the 'LogTrace' log level.
logTrace :: MonadLog m => T.Text -> Value -> m ()
logTrace = logNow LogTrace

-- | Like 'logAttention', but without any additional associated data.
logAttention_ :: MonadLog m => T.Text -> m ()
logAttention_ = (`logAttention` emptyObject)

-- | Like 'logInfo', but without any additional associated data.
logInfo_ :: MonadLog m => T.Text -> m ()
logInfo_ = (`logInfo` emptyObject)

-- | Like 'logTrace', but without any additional associated data.
logTrace_ :: MonadLog m => T.Text -> m ()
logTrace_ = (`logTrace` emptyObject)

----------------------------------------

-- | Write a log message using the given log level, message text and
-- additional data. Current time is used as the event time.
logNow :: MonadLog m => LogLevel -> T.Text -> Value -> m ()
logNow level message data_ = do
  time <- currentTime
  logMessage time level message data_
