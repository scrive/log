-- | Basic data types used throughout the package.
module Log.Data (
    LogLevel(..)
  , showLogLevel
  , readLogLevel
  , readLogLevelEither
  , LogMessage(..)
  , showLogMessage
  , defaultLogLevel
  ) where

import Control.DeepSeq
import Control.Applicative
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types
import Data.ByteString.Lazy (toStrict)
import Data.Time
import Prelude
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Monoid as Monoid

-- | Available log levels.
-- Note that ordering in this definintion determines what the maximum log level is.
-- See 'Log.Monad.leMaxLogLevel'.
data LogLevel = LogAttention | LogInfo | LogTrace
  deriving (Bounded, Eq, Ord, Show)

-- | This function is partial.
readLogLevel :: T.Text -> LogLevel
readLogLevel = either error id . readLogLevelEither

readLogLevelEither :: T.Text -> Either String LogLevel
readLogLevelEither "attention" = Right LogAttention
readLogLevelEither "info"      = Right LogInfo
readLogLevelEither "trace"     = Right LogTrace
readLogLevelEither level       = Left $ "readLogLevel: unknown level: "
                                 ++ T.unpack level

showLogLevel :: LogLevel -> T.Text
showLogLevel LogAttention = "attention"
showLogLevel LogInfo      = "info"
showLogLevel LogTrace     = "trace"

-- | The default log level. Returns `LogInfo`.
defaultLogLevel :: LogLevel
defaultLogLevel = LogInfo

instance ToJSON LogLevel where
  toJSON = toJSON . showLogLevel
  toEncoding = toEncoding . showLogLevel

instance FromJSON LogLevel where
  parseJSON = withText "LogLevel" $
    either fail pure . readLogLevelEither

instance NFData LogLevel where
  rnf = (`seq` ())

-- | Represents message to be logged.
data LogMessage = LogMessage {
  -- | Component of an application.
  lmComponent :: !T.Text
  -- | Application log domain.
, lmDomain    :: ![T.Text]
  -- | Time of the logged event.
, lmTime      :: !UTCTime
  -- | Log level.
, lmLevel     :: !LogLevel
  -- | Message to be logged.
, lmMessage   :: !T.Text
  -- | Additional data associated with the message.
, lmData      :: !Value
} deriving (Eq, Show)

-- | Render a 'LogMessage' to 'Text'.
showLogMessage :: Maybe UTCTime -- ^ The time that message was added to the log.
               -> LogMessage    -- ^ The actual message.
               -> T.Text
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
      confIndent = Spaces 2
    }

instance ToJSON LogMessage where
  toJSON LogMessage{..} = object [
      "component" .= lmComponent
    , "domain"    .= lmDomain
    , "time"      .= lmTime
    , "level"     .= lmLevel
    , "message"   .= lmMessage
    , "data"      .= lmData
    ]

  toEncoding LogMessage{..} = pairs $ Monoid.mconcat [
      "component" .= lmComponent
    , "domain"    .= lmDomain
    , "time"      .= lmTime
    , "level"     .= lmLevel
    , "message"   .= lmMessage
    , "data"      .= lmData
    ]

instance FromJSON LogMessage where
  parseJSON = withObject "LogMessage" $ \obj -> LogMessage
    -- to suppress warnings
    Control.Applicative.<$> obj .: "component"
    <*> obj .: "domain"
    <*> obj .: "time"
    <*> obj .: "level"
    <*> obj .: "message"
    <*> obj .: "data"

instance NFData LogMessage where
  rnf LogMessage{..} = rnf lmComponent
    `seq` rnf lmDomain
    `seq` rnf lmTime
    `seq` rnf lmLevel
    `seq` rnf lmMessage
    `seq` rnf lmData
