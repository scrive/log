-- | Stdout logging back-end.
module Log.Backend.StandardOutput
  ( withSimpleStdOutLogger
  , withStdOutLogger
  , withJsonStdOutLogger
  ) where

import Control.Monad.IO.Unlift
import Data.Aeson
import Prelude
import System.IO
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as BSL

import Log.Data
import Log.Internal.Logger
import Log.Logger

withSimpleStdOutLogger :: MonadUnliftIO m => (Logger -> m r) -> m r
withSimpleStdOutLogger = withStdOutLogger
{-# DEPRECATED withSimpleStdOutLogger "Use withStdOutLogger instead" #-}

-- | Create a logger that prints messages to standard output for the duration of
-- the given action.
withStdOutLogger :: MonadUnliftIO m => (Logger -> m r) -> m r
withStdOutLogger act = withRunInIO $ \unlift -> do
  logger <- mkLogger "stdout" $ \msg -> do
    T.putStrLn $ showLogMessage Nothing msg
    hFlush stdout
  withLogger logger (unlift . act)

-- | Create a logger that prints messages in the JSON format to standard output
-- for the duration of the given action.
withJsonStdOutLogger :: MonadUnliftIO m => (Logger -> m r) -> m r
withJsonStdOutLogger act = withRunInIO $ \unlift -> do
  logger <- mkLogger "stdout-json" $ \msg -> do
    BSL.putStrLn $ encode msg
    hFlush stdout
  withLogger logger (unlift . act)
