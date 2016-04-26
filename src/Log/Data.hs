{-# LANGUAGE CPP, OverloadedStrings, RecordWildCards #-}
module Log.Data (
    LogLevel(..)
  , showLogLevel
  , readLogLevel
  , LogMessage(..)
  , showLogMessage
  ) where

import Control.DeepSeq
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Data.ByteString.Lazy (toStrict)
import Data.Time
#if !MIN_VERSION_time(1,5,0)
import System.Locale
#endif
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Available log levels.
data LogLevel = LogAttention | LogInfo | LogTrace
  deriving (Bounded, Eq, Ord, Show)

readLogLevel :: T.Text -> LogLevel
readLogLevel "attention" = LogAttention
readLogLevel "info"      = LogInfo
readLogLevel "trace"     = LogTrace
readLogLevel level       = error $ "readLogLevel: unknown level: " ++ T.unpack level

showLogLevel :: LogLevel -> T.Text
showLogLevel LogAttention = "attention"
showLogLevel LogInfo      = "info"
showLogLevel LogTrace     = "trace"

instance NFData LogLevel where
  rnf = (`seq` ())

-- | Represents message to be logged.
data LogMessage = LogMessage {
  -- | Component of an application.
  lmComponent :: !T.Text
  -- | Aplication log domain.
, lmDomain    :: ![T.Text]
  -- | Time of log.
, lmTime      :: !UTCTime
  -- | Log level.
, lmLevel     :: !LogLevel
  -- | Message to be logged.
, lmMessage   :: !T.Text
  -- | Additional data associated with the message.
, lmData      :: !Value
} deriving (Eq, Show)

showLogMessage :: Maybe UTCTime -> LogMessage -> T.Text
showLogMessage mInsertionTime LogMessage{..} = T.concat $ [
    T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" lmTime
  , case mInsertionTime of
      Nothing -> " "
      Just it -> T.pack $ formatTime defaultTimeLocale " (%H:%M:%S) " it
  , T.toUpper $ showLogLevel lmLevel
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

instance ToJSON LogMessage where
  toJSON LogMessage{..} = object [
      "component" .= lmComponent
    , "domain"    .= lmDomain
    , "time"      .= lmTime
    , "level"     .= showLogLevel lmLevel
    , "message"   .= lmMessage
    , "data"      .= lmData
    ]

instance NFData LogMessage where
  rnf LogMessage{..} = rnf lmComponent
    `seq` rnf lmDomain
    `seq` rnf lmTime
    `seq` rnf lmLevel
    `seq` rnf lmMessage
    `seq` rnf lmData
