module Main (main) where

import Data.Aeson (toJSON)
import Data.IORef
import qualified Data.HashMap.Strict as HM
import OpenTelemetry.Context (insertSpan)
import OpenTelemetry.Context.ThreadLocal (adjustContext)
import OpenTelemetry.Exporter.InMemory.LogRecord
import OpenTelemetry.Internal.Log.Types
import OpenTelemetry.Log.Core
import OpenTelemetry.Processor.Simple.LogRecord
import OpenTelemetry.Trace.Core
  (SpanContext(..), defaultTraceFlags, wrapSpanContext)
import OpenTelemetry.Trace.Id (bytesToSpanId, bytesToTraceId)
import Test.Hspec
import qualified OpenTelemetry.LogAttributes as LA
import qualified OpenTelemetry.Trace.TraceState as TraceState

import Log
import Log.Backend.OpenTelemetry

main :: IO ()
main = hspec $ describe "Log.Backend.OpenTelemetry" $ do
  describe "logLevelToSeverity" $ do
    it "maps log levels to severities" $ do
      map logLevelToSeverity [LogAttention, LogInfo, LogTrace]
        `shouldBe` [Error, Info, Debug]

  describe "valueToAnyValue" $ do
    it "converts JSON values" $ do
      valueToAnyValue (object ["a" .= (1 :: Int), "b" .= [1.5 :: Double]])
        `shouldBe` HashMapValue (HM.fromList
                     [ ("a", IntValue 1)
                     , ("b", ArrayValue [DoubleValue 1.5])
                     ])
      valueToAnyValue (toJSON (2 ^ (70 :: Int) :: Integer))
        `shouldBe` DoubleValue (2 ^ (70 :: Int))

  describe "withOpenTelemetryLogger" $ do
    it "emits log records" $ do
      records <- withRecords $ \logger ->
        runLogT "main" logger LogTrace $ localDomain "sub" $ do
          logAttention "attention" $ object ["key" .= ("value" :: String)]
          logInfo_ "info"
          logTrace_ "trace"
      ilrs <- mapM readLogRecord records
      map logRecordBody ilrs
        `shouldBe` map TextValue ["attention", "info", "trace"]
      map (toBaseMaybe . logRecordSeverityNumber) ilrs
        `shouldBe` map Just [Error, Info, Debug]
      map (toBaseMaybe . logRecordSeverityText) ilrs
        `shouldBe` map Just ["attention", "info", "trace"]
      case ilrs of
        ilr : _ -> do
          let attrs = snd . LA.getAttributeMap $ logRecordAttributes ilr
          HM.lookup "log.component" attrs `shouldBe` Just (TextValue "main")
          HM.lookup "log.domain" attrs
            `shouldBe` Just (ArrayValue [TextValue "sub"])
          HM.lookup "key" attrs `shouldBe` Just (TextValue "value")
        [] -> expectationFailure "no log records"

    it "correlates log records with the active span" $ do
      let tid = either error id $ bytesToTraceId "\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10"
          sid = either error id $ bytesToSpanId "\xaa\xbb\xcc\xdd\xee\xff\x11\x22"
          sc = SpanContext
            { traceFlags = defaultTraceFlags
            , isRemote   = False
            , traceId    = tid
            , spanId     = sid
            , traceState = TraceState.empty
            }
      adjustContext . insertSpan $ wrapSpanContext sc
      records <- withRecords $ \logger ->
        runLogT "main" logger defaultLogLevel $ logInfo_ "in span"
      ilrs <- mapM readLogRecord records
      map logRecordTracingDetails ilrs
        `shouldBe` [TracingDetails tid sid defaultTraceFlags]
  where
    withRecords act = do
      (exporter, ref) <- inMemoryLogRecordExporter
      processor <- simpleLogRecordProcessor
        (SimpleLogRecordProcessorConfig exporter 30000000)
      lp <- createLoggerProvider [processor] emptyLoggerProviderOptions
      let otelLogger = makeLogger lp (instrumentationLibrary "test" "0.0.0")
      _ <- withOpenTelemetryLogger otelLogger act
      reverse <$> readIORef ref
