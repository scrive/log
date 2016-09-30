-- | The 'LogT' monad transformer for adding logging capabilities to any monad.
{-# LANGUAGE CPP #-}
module Log.Monad (
    Logger
  , LoggerEnv(..)
  , InnerLogT
  , LogT(..)
  , runLogT
  , mapLogT
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Prelude
import qualified Control.Exception as E
import qualified Data.HashMap.Strict as H

import Log.Class
import Log.Data
import Log.Logger

-- | The state that every 'LogT' carries around.
data LoggerEnv = LoggerEnv {
  leLogger    :: !Logger -- ^ The 'Logger' to use.
, leComponent :: !Text   -- ^ Current application component.
, leDomain    :: ![Text] -- ^ Current application domain.
, leData      :: ![Pair] -- ^ Additional data to be merged with the
                         -- log message\'s data.
}

type InnerLogT = ReaderT LoggerEnv

-- | Monad transformer that adds logging capabilities to the underlying monad.
newtype LogT m a = LogT { unLogT :: InnerLogT m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadBase b, MonadCatch
           ,MonadIO, MonadMask, MonadPlus, MonadThrow, MonadTrans)

-- | Run a 'LogT' computation.
runLogT :: Text     -- ^ Application component name to use.
        -> Logger   -- ^ The logging back-end to use.
        -> LogT m a -- ^ The 'LogT' computation to run.
        -> m a
runLogT component logger m = runReaderT (unLogT m) LoggerEnv {
  leLogger = logger
, leComponent = component
, leDomain = []
, leData = []
}

-- | Transform the computation inside a 'LogT'.
mapLogT :: (m a -> n b) -> LogT m a -> LogT n b
mapLogT f = LogT . mapReaderT f . unLogT

instance MonadTransControl LogT where
#if MIN_VERSION_monad_control(1,0,0)
  type StT LogT m = StT InnerLogT m
  liftWith = defaultLiftWith LogT unLogT
  restoreT = defaultRestoreT LogT
#else
  newtype StT LogT m = StLogT { unStLogT :: StT InnerLogT m }
  liftWith = defaultLiftWith LogT unLogT StLogT
  restoreT = defaultRestoreT LogT unStLogT
#endif
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance MonadBaseControl b m => MonadBaseControl b (LogT m) where
#if MIN_VERSION_monad_control(1,0,0)
  type StM (LogT m) a = ComposeSt LogT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
#else
  newtype StM (LogT m) a = StMLogT { unStMLogT :: ComposeSt LogT m a }
  liftBaseWith = defaultLiftBaseWith StMLogT
  restoreM     = defaultRestoreM unStMLogT
#endif
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance (MonadBase IO m, MonadTime m) => MonadLog (LogT m) where
  logMessage time level message data_ = LogT $ ReaderT logMsg
    where
      logMsg LoggerEnv{..} = liftBase $ do
        execLogger leLogger =<< E.evaluate (force lm)
        where
          lm = LogMessage {
            lmComponent = leComponent
          , lmDomain = leDomain
          , lmTime = time
          , lmLevel = level
          , lmMessage = message
          , lmData = case data_ of
            Object obj -> Object . H.union obj $ H.fromList leData
            _ | null leData -> data_
              | otherwise -> object $ ("_data", data_) : leData
          }

  localData data_ =
    LogT . local (\e -> e { leData = data_ ++ leData e }) . unLogT

  localDomain domain =
    LogT . local (\e -> e { leDomain = leDomain e ++ [domain] }) . unLogT
