{-# LANGUAGE OverloadedStrings #-}
module Log.Backend.StandardOutput (stdoutLogger) where

import qualified Data.Text.IO as T

import Log.Data
import Log.Logger

-- | Create logger that prints messages to standard output.
stdoutLogger :: IO Logger
stdoutLogger = mkLogger "stdout" $ T.putStrLn . showLogMessage
