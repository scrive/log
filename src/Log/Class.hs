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
import Control.Monad.Time.Instances ()
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
