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
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Error.Class
import Control.Monad.IO.Unlift
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Control
import Control.Monad.Writer.Class
import Data.Aeson
import Data.Text (Text)
import Data.Time
import qualified Control.Monad.Fail as MF
import qualified Control.Exception as E

import Log.Class
import Log.Data
import Log.Logger
import qualified Log.Internal.Aeson.Compat as AC

type InnerLogT = ReaderT LoggerEnv

-- | Monad transformer that adds logging capabilities to the underlying monad.
newtype LogT m a = LogT { unLogT :: InnerLogT m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadBase b, MonadCatch
           ,MonadIO, MonadMask, MonadPlus, MonadThrow, MonadTrans, MF.MonadFail
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
        -> LogLevel -- ^ The maximum log level allowed to be logged.
                    --   Only messages less or equal than this level with be logged.
        -> LogT m a -- ^ The 'LogT' computation to run.
        -> m a
runLogT component logger maxLogLevel m = runReaderT (unLogT m) LoggerEnv {
  leLogger = logger
, leComponent = component
, leDomain = []
, leData = []
, leMaxLogLevel = maxLogLevel
} -- We can't do synchronisation here, since 'runLogT' can be invoked
  -- quite often from the application (e.g. on every request).

-- | Transform the computation inside a 'LogT'.
mapLogT :: (m a -> n b) -> LogT m a -> LogT n b
mapLogT f = LogT . mapReaderT f . unLogT

-- | Base implementation of 'logMessage' for use with a specific
-- 'LoggerEnv'. Useful for reimplementation of 'MonadLog' instance.
logMessageIO :: LoggerEnv -> UTCTime -> LogLevel -> Text -> Value -> IO ()
logMessageIO LoggerEnv{..} time level message data_ =
  when (level <= leMaxLogLevel) $
    execLogger leLogger =<< E.evaluate (force lm)
  where
    lm = LogMessage
      { lmComponent = leComponent
      , lmDomain = leDomain
      , lmTime = time
      , lmLevel = level
      , lmMessage = message
      , lmData = case data_ of
          -- If lmData is not an object, we make it so and put previous data as
          -- the singleton value with key reflecting its type. It's required for
          -- ElasticSearch as ES needs fields with the same name to be of the
          -- same type in all log messages.
          Object obj      -> Object . AC.union obj $ AC.fromList leData
          _ | null leData -> object [dataTyped data_ .= data_]
            | otherwise   -> object $ (dataTyped data_, data_) : leData
      }

    dataTyped = \case
      Object{} -> "__data_object"
      Array{}  -> "__data_array"
      String{} -> "__data_string"
      Number{} -> "__data_number"
      Bool{}   -> "__data_bool"
      Null{}   -> "__data_null"

-- | Return an IO action that logs messages using the current 'MonadLog'
-- context. Useful for interfacing with libraries such as @aws@ or @amazonka@
-- that accept logging callbacks operating in IO.
getLoggerIO :: MonadLog m => m (UTCTime -> LogLevel -> Text -> Value -> IO ())
getLoggerIO = logMessageIO <$> getLoggerEnv

-- | @'hoist' = 'mapLogT'@
--
-- @since 0.7.2
instance MFunctor LogT where
    hoist f = mapLogT f

instance MonadTransControl LogT where
  type StT LogT m = StT InnerLogT m
  liftWith = defaultLiftWith LogT unLogT
  restoreT = defaultRestoreT LogT

instance MonadBaseControl b m => MonadBaseControl b (LogT m) where
  type StM (LogT m) a = ComposeSt LogT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

instance MonadUnliftIO m => MonadUnliftIO (LogT m) where
  withRunInIO inner = LogT $ withRunInIO $ \run -> inner (run . unLogT)

instance MonadBase IO m => MonadLog (LogT m) where
  logMessage level message data_ = LogT . ReaderT $ \logEnv -> liftBase $ do
    time <- getCurrentTime
    logMessageIO logEnv time level message data_

  localData data_ =
    LogT . local (\e -> e { leData = data_ ++ leData e }) . unLogT

  localDomain domain =
    LogT . local (\e -> e { leDomain = leDomain e ++ [domain] }) . unLogT

  localMaxLogLevel level =
    LogT . local (\e -> e { leMaxLogLevel = level }) . unLogT

  getLoggerEnv = LogT ask
