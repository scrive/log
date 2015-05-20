{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverlappingInstances
  , RankNTypes, UndecidableInstances #-}
module Log.Class.Instances () where

import Control.Monad.Trans
import Control.Monad.Trans.Control

import Log.Class

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
