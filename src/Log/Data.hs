{-# LANGUAGE RecordWildCards #-}
module Log.Data (
    LogLevel(..)
  , LogMessage(..)
  ) where

import Control.DeepSeq
import Data.Aeson
import Data.Text (Text)
import Data.Time

-- | Available log levels.
data LogLevel = LogAttention | LogInfo | LogTrace
  deriving (Bounded, Eq, Ord, Show)

instance NFData LogLevel

-- | Represents message to be logged.
data LogMessage = LogMessage {
  -- | Component of an application.
  lmComponent :: !Text
  -- | Time of log.
, lmTime      :: !UTCTime
  -- | Log level.
, lmLevel     :: !LogLevel
  -- | Message to be logged.
, lmMessage   :: !Text
  -- | Additional data associated with the message.
, lmData      :: !Value
} deriving (Eq, Show)

instance NFData LogMessage where
  rnf LogMessage{..} = rnf lmComponent
    `seq` rnf lmTime
    `seq` rnf lmLevel
    `seq` rnf lmMessage
    `seq` rnf lmData
