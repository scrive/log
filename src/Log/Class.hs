{-# OPTIONS_GHC -fno-warn-deprecated-flags #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverlappingInstances
  , RankNTypes, UndecidableInstances #-}
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
import qualified Data.Text as T

import Log.Data

-- | Represents the family of monads with logging capabilities.
class MonadTime m => MonadLog m where
  logMessage  :: UTCTime -> LogLevel -> T.Text -> Value -> m ()
  localData   :: [Pair] -> m a -> m a
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

logAttention :: MonadLog m => T.Text -> Value -> m ()
logAttention = logNow LogAttention

logInfo :: MonadLog m => T.Text -> Value -> m ()
logInfo = logNow LogInfo

logTrace :: MonadLog m => T.Text -> Value -> m ()
logTrace = logNow LogTrace

logAttention_ :: MonadLog m => T.Text -> m ()
logAttention_ = (`logAttention` emptyObject)

logInfo_ :: MonadLog m => T.Text -> m ()
logInfo_ = (`logInfo` emptyObject)

logTrace_ :: MonadLog m => T.Text -> m ()
logTrace_ = (`logTrace` emptyObject)

----------------------------------------

logNow :: MonadLog m => LogLevel -> T.Text -> Value -> m ()
logNow level message data_ = do
  time <- currentTime
  logMessage time level message data_
