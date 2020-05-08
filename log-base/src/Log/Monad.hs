-- | The 'LogT' monad transformer for adding logging capabilities to any monad.
{-# LANGUAGE CPP #-}
module Log.Monad (
    Logger
  , LoggerEnv(..)
  , InnerLogT
  , LogT(..)
  , runLogT
  , mapLogT
  , logMessageIO
  , getLoggerIO
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Unlift
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Control
import Control.Monad.Writer.Class
import Data.Aeson
import Data.Text (Text)
import Prelude
import qualified Control.Exception as E
import qualified Data.HashMap.Strict as H

import Log.Class
import Log.Data
import Log.Logger

type InnerLogT = ReaderT LoggerEnv

-- | Monad transformer that adds logging capabilities to the underlying monad.
newtype LogT m a = LogT { unLogT :: InnerLogT m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadBase b, MonadCatch
           ,MonadIO, MonadMask, MonadPlus, MonadThrow, MonadTrans, MonadFail
           ,MonadError e, MonadWriter w, MonadState s)

instance MonadReader r m => MonadReader r (LogT m) where
    ask   = lift ask
    local = mapLogT . local

-- | Run a 'LogT' computation.
--
-- Note that in the case of asynchronous/bulk loggers 'runLogT'
-- doesn't guarantee that all messages are actually written to the log
-- once it finishes. Use 'withPGLogger' or 'withElasticSearchLogger'
-- for that.
runLogT :: Text     -- ^ Application component name to use.
        -> Logger   -- ^ The logging back-end to use.
        -> LogT m a -- ^ The 'LogT' computation to run.
        -> m a
runLogT component logger m = runReaderT (unLogT m) LoggerEnv {
  leLogger = logger
, leComponent = component
, leDomain = []
, leData = []
} -- We can't do synchronisation here, since 'runLogT' can be invoked
  -- quite often from the application (e.g. on every request).

-- | Transform the computation inside a 'LogT'.
mapLogT :: (m a -> n b) -> LogT m a -> LogT n b
mapLogT f = LogT . mapReaderT f . unLogT

-- | Base implementation of 'logMessage' for use with a specific
-- 'LoggerEnv'. Useful for reimplementation of 'MonadLog' instance.
logMessageIO :: LoggerEnv -> UTCTime -> LogLevel -> Text -> Value -> IO ()
logMessageIO LoggerEnv{..} time level message data_ =
  execLogger leLogger =<< E.evaluate (force lm)
  where
    lm = LogMessage
      { lmComponent = leComponent
      , lmDomain = leDomain
      , lmTime = time
      , lmLevel = level
      , lmMessage = message
      , lmData = case data_ of
          Object obj -> Object . H.union obj $ H.fromList leData
          _ | null leData -> data_
            | otherwise -> object $ ("_data", data_) : leData
      }

-- | Return an IO action that logs messages using the current 'MonadLog'
-- context. Useful for interfacing with libraries such as @aws@ or @amazonka@
-- that accept logging callbacks operating in IO.
getLoggerIO :: MonadLog m => m (UTCTime -> LogLevel -> Text -> Value -> IO ())
getLoggerIO = logMessageIO <$> getLoggerEnv

-- | @'hoist' = 'mapLogT'@
--
-- @since 0.7.2
instance MFunctor LogT where
    hoist = mapLogT

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

instance MonadUnliftIO m => MonadUnliftIO (LogT m) where
  askUnliftIO = do
    UnliftIO runInIO <- LogT askUnliftIO
    return $ UnliftIO $ runInIO . unLogT

instance (MonadBase IO m, MonadTime m) => MonadLog (LogT m) where
  logMessage time level message data_ = LogT . ReaderT $ \logEnv ->
    liftBase $ logMessageIO logEnv time level message data_

  localData data_ =
    LogT . local (\e -> e { leData = data_ ++ leData e }) . unLogT

  localDomain domain =
    LogT . local (\e -> e { leDomain = leDomain e ++ [domain] }) . unLogT

  getLoggerEnv = LogT ask
