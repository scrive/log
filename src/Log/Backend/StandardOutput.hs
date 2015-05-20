{-# LANGUAGE CPP, OverloadedStrings, RecordWildCards #-}
module Log.Backend.StandardOutput (stdoutLogger) where

import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Data.ByteString.Lazy (toStrict)
import Data.Time
#if !MIN_VERSION_time(1,5,0)
import System.Locale
#endif
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import Log.Data
import Log.Logger

-- | Create logger that prints messages to standard output.
stdoutLogger :: IO Logger
stdoutLogger = mkLogger "stdout" printLogMessage

printLogMessage :: LogMessage -> IO ()
printLogMessage LogMessage{..} = T.putStrLn . T.concat $ [
    T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" lmTime
  , " "
  , textifyLevel lmLevel
  , " "
  , T.intercalate "/" $ lmComponent : lmDomain
  , ": "
  , lmMessage
  ] ++ if lmData == emptyObject
    then []
    else [" ", textifyData lmData]
  where
    textifyData :: Value -> T.Text
    textifyData = T.decodeUtf8 . toStrict . encodePretty' defConfig {
      confIndent = 2
    }

    textifyLevel :: LogLevel -> T.Text
    textifyLevel LogAttention = "ATTENTION"
    textifyLevel LogInfo = "INFO"
    textifyLevel LogTrace = "TRACE"
