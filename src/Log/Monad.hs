{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances
  , GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings
  , RecordWildCards, TypeFamilies, UndecidableInstances #-}
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
import qualified Control.Exception as E
import qualified Data.HashMap.Strict as H

import Log.Class
import Log.Data
import Log.Logger

-- | 'LogT' environment.
data LoggerEnv = LoggerEnv {
  leLogger    :: !Logger
, leComponent :: !Text
, leData      :: ![Pair]
}

type InnerLogT = ReaderT LoggerEnv

-- | Monad transformer that adds logging capabilities to the underlying monad.
newtype LogT m a = LogT { unLogT :: InnerLogT m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadCatch, MonadIO, MonadMask, MonadThrow, MonadTrans)

runLogT :: Text -> Logger -> LogT m a -> m a
runLogT component logger m = runReaderT (unLogT m) LoggerEnv {
  leLogger = logger
, leComponent = component
, leData = []
}

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
          , lmTime = time
          , lmLevel = level
          , lmMessage = message
          , lmData = case data_ of
            Object obj -> Object . H.union obj $ H.fromList leData
            _ | null leData -> data_
              | otherwise -> object $ ("_data", data_) : leData
          }

  localData data_ = LogT . local (\e -> e { leData = data_ ++ leData e }) . unLogT
