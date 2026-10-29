-- | OpenTelemetry logging back-end.
--
-- Forwards 'LogMessage's to the OpenTelemetry Logs pipeline via the Logs
-- Bridge API of @hs-opentelemetry-api@. This means:
--
-- * __Trace correlation is automatic__: log records are emitted
--   synchronously in the thread that logs the message, so if it happens
--   inside an 'OpenTelemetry.Trace.Core.inSpan' block, the emitted log
--   record carries the active trace and span IDs.
--
-- * __Severity mapping__: 'LogAttention' → 'Error', 'LogInfo' → 'Info',
--   'LogTrace' → 'Debug' (with the original level name preserved in
--   @severityText@).
--
-- * __Structured data__: the fields of 'lmData' become log record
--   attributes. The component and the domain are stored in the
--   @log.component@ and @log.domain@ attributes respectively.
--
-- Batching and exporting of log records is the responsibility of the
-- processors of the 'LoggerProvider', so this back-end doesn't run a separate
-- logger thread.
--
-- = Usage
--
-- @
-- import Log
-- import Log.Backend.OpenTelemetry
-- import OpenTelemetry.Log.Core (getGlobalLoggerProvider, instrumentationLibrary, makeLogger)
--
-- main :: IO ()
-- main = do
--   lp <- getGlobalLoggerProvider
--   let otelLogger = makeLogger lp (instrumentationLibrary \"my-app\" \"1.0.0\")
--   withOpenTelemetryLogger otelLogger $ \\logger -> do
--     runLogT \"main\" logger defaultLogLevel $ do
--       logInfo_ \"Hi there\"
-- @
module Log.Backend.OpenTelemetry
  ( withOpenTelemetryLogger
  , openTelemetryLogger
  , toLogRecordArguments
  , logLevelToSeverity
  , valueToAnyValue
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Data.Aeson (Value(..))
import Data.Int
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.Scientific as S
import qualified OpenTelemetry.Common as OTel (Timestamp(..))
import qualified OpenTelemetry.Log.Core as OTel
import qualified OpenTelemetry.Internal.Log.Types as OTel (emptyLogRecordArguments)

import Log.Data
import Log.Internal.Logger

-- | Create an 'openTelemetryLogger' for the duration of the given action, and
-- flush the 'LoggerProvider' of the given OpenTelemetry logger afterwards.
--
-- /Note:/ the 'LoggerProvider' is not shut down, that's the responsibility of
-- its owner.
withOpenTelemetryLogger
  :: MonadUnliftIO m
  => OTel.Logger
  -> (Logger -> m r)
  -> m r
withOpenTelemetryLogger otelLogger act = withRunInIO $ \unlift -> do
  withLogger (openTelemetryLogger otelLogger) (unlift . act)

-- | A 'Logger' that emits every 'LogMessage' as an OpenTelemetry log record.
--
-- Messages are emitted synchronously (so that the log record picks up the
-- OpenTelemetry context of the calling thread), 'waitForLogger' forces a flush
-- of the 'LoggerProvider' and 'shutdownLogger' is a no-op.
openTelemetryLogger :: OTel.Logger -> Logger
openTelemetryLogger otelLogger = Logger
  { loggerWriteMessage = void . OTel.emitLogRecord otelLogger . toLogRecordArguments
  , loggerWaitForWrite = void $ OTel.forceFlushLoggerProvider lp Nothing
  , loggerShutdown     = pure ()
  }
  where
    lp = OTel.loggerLoggerProvider otelLogger

-- | Convert a 'LogMessage' to the arguments of an OpenTelemetry log record.
toLogRecordArguments :: LogMessage -> OTel.LogRecordArguments
toLogRecordArguments LogMessage{..} = OTel.emptyLogRecordArguments
  { OTel.timestamp      = Just $ utcTimeToTimestamp lmTime
  , OTel.severityNumber = Just $ logLevelToSeverity lmLevel
  , OTel.severityText   = Just $ showLogLevel lmLevel
  , OTel.body           = OTel.TextValue lmMessage
  , OTel.attributes     = HM.union baseAttrs dataAttrs
  }
  where
    baseAttrs = HM.fromList $
      ("log.component", OTel.TextValue lmComponent)
      : [ ("log.domain", OTel.ArrayValue $ map OTel.TextValue lmDomain)
        | not $ null lmDomain
        ]

    dataAttrs = case lmData of
      Object obj -> HM.map valueToAnyValue $ KM.toHashMapText obj
      Null       -> HM.empty
      v          -> HM.singleton "log.data" $ valueToAnyValue v

-- | Map a 'LogLevel' to an OpenTelemetry 'OTel.SeverityNumber'.
logLevelToSeverity :: LogLevel -> OTel.SeverityNumber
logLevelToSeverity = \case
  LogAttention -> OTel.Error
  LogInfo      -> OTel.Info
  LogTrace     -> OTel.Debug

-- | Convert a JSON 'Value' to an OpenTelemetry 'OTel.AnyValue'.
--
-- Integral numbers that fit into 'Int64' are converted to 'OTel.IntValue',
-- other numbers to 'OTel.DoubleValue'.
valueToAnyValue :: Value -> OTel.AnyValue
valueToAnyValue = \case
  Object obj -> OTel.HashMapValue . HM.map valueToAnyValue $ KM.toHashMapText obj
  Array arr  -> OTel.ArrayValue . map valueToAnyValue $ F.toList arr
  String s   -> OTel.TextValue s
  Number n   -> case S.toBoundedInteger n :: Maybe Int64 of
    Just i  -> OTel.IntValue i
    Nothing -> OTel.DoubleValue $ S.toRealFloat n
  Bool b     -> OTel.BoolValue b
  Null       -> OTel.NullValue

----------------------------------------

utcTimeToTimestamp :: UTCTime -> OTel.Timestamp
utcTimeToTimestamp t = OTel.Timestamp . fromIntegral
  $ (truncate (utcTimeToPOSIXSeconds t * 1000000000) :: Integer)
