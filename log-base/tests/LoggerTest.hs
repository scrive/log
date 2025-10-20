module LoggerTest where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Foldable (for_, traverse_)
import Data.List
import qualified Data.Text as T
import qualified Hedgehog as H
import qualified Hedgehog.Gen as H
import qualified Hedgehog.Range as HR
import Log
import Log.Internal.Logger
import Test.Tasty
import Test.Tasty.Hedgehog

test_logger :: [TestTree]
test_logger =
  [ testProperty "Sends all messages in order" $ H.property $ do
      inorderTest mkTestLogger
  , testProperty "Drops messages after capacity is reached" $ H.property $ do
      dropMessagesTest mkTestLogger
  , testProperty "Obeys log levels" $ H.property $ do
      inputs <- H.forAll $ H.list (HR.linear 0 1000) (H.int HR.linearBounded)
      mask <- H.forAll $ H.list (HR.singleton (length inputs)) H.enumBounded
      let logInputs = zip (fmap (T.pack . show) inputs) mask

      logTrail@LogTrail {..} <- liftIO mkLogTrail
      logger <- liftIO $ mkTestLogger 1000 logTrail
      runLogT "test" logger LogInfo $ do
        for_ logInputs $ \(msg, level) -> do
          case level of
            LogAttention -> logAttention_ msg
            LogInfo -> logInfo_ msg
            LogTrace -> logTrace_ msg

      liftIO $ loggerWaitForWrite logger
      let expectedOutput = map fst $ filter ((<= LogInfo) . snd) logInputs
      let traceOutput = filter ((== LogTrace) . snd) logInputs

      outputs <- liftIO trail
      expectedOutput H.=== fmap lmMessage (concat outputs)
      length logInputs - length (concat outputs) H.=== length traceOutput
  ]

test_bulkLogger :: [TestTree]
test_bulkLogger =
  [ testProperty "Sends all messages in order" $ H.property $ do
      inorderTest $ \cap -> mkBulkTestLogger cap 10_000
  , testProperty "Drops messages after capacity is reached" $ H.property $ do
      dropMessagesTest $ \cap -> mkBulkTestLogger cap 10_000
  , testProperty "Sends all messages in multiple bulks" $ H.property $ do
      inputs <- H.forAll $ H.list (HR.singleton 70) (H.int HR.linearBounded)
      bulkSizes <- H.forAll $ H.int (HR.linear 10 40)
      let logInputs = fmap (T.pack . show) inputs

      logTrail@LogTrail {..} <- liftIO mkLogTrail
      logger <- liftIO $ mkBulkTestLogger 1000 10_000 logTrail
      let chunks = chunksOf bulkSizes logInputs
      for_ chunks $ \chunk -> runLogT "test" logger LogInfo $ do
        traverse_ logInfo_ chunk
        liftIO $ loggerWaitForWrite logger

      outputs <- liftIO trail
      logInputs H.=== fmap lmMessage (concat outputs)
      (length outputs >= 2) H.=== True
  ]

inorderTest :: (Monad m, MonadIO m) => (Int -> LogTrail -> IO Logger) -> H.PropertyT m ()
inorderTest mkTestLog = do
  inputs <- H.forAll $ H.list (HR.linear 0 100) (H.int HR.linearBounded)
  let logInputs = fmap (T.pack . show) inputs

  logTrail@LogTrail {..} <- liftIO mkLogTrail
  logger <- liftIO $ mkTestLog 1000 logTrail
  liftIO $ runLogT "test" logger LogInfo $ do
    traverse_ logInfo_ logInputs
  liftIO $ loggerWaitForWrite logger

  outputs <- liftIO trail
  logInputs H.=== fmap lmMessage (concat outputs)
  if null logInputs
    then length outputs H.=== 0
    else not (null outputs) H.=== True

dropMessagesTest :: (Monad m, MonadIO m) => (Int -> LogTrail -> IO Logger) -> H.PropertyT m ()
dropMessagesTest mkTestLog = do
  let capacity = 10
  inputs <- H.forAll $ H.list (HR.linear capacity 100) (H.int HR.linearBounded)
  let logInputs = fmap (T.pack . show) inputs

  logTrail@LogTrail {..} <- liftIO mkLogTrail
  logger <- liftIO $ mkTestLog 10 logTrail
  liftIO $ runLogT "test" logger LogInfo $ do
    traverse_ logInfo_ logInputs
  liftIO $ loggerWaitForWrite logger

  outputs <- liftIO trail
  fmap lmMessage (concat outputs) `isSubsequenceOf` logInputs H.=== True
  (length outputs <= length logInputs) H.=== True

-- | Test utility for tracking the calls the bulk logger makes in the background
-- grouping logmessages that were sent in the same action.
data LogTrail = LogTrail
  { trail :: IO [[LogMessage]]
  , trailAdd :: [LogMessage] -> IO ()
  }

mkLogTrail :: IO LogTrail
mkLogTrail = do
  logTrailRef <- newMVar []
  let addLogs new = modifyMVar_ logTrailRef (\logs -> pure (new : logs))
  pure $
    LogTrail
      { trail = reverse <$> swapMVar logTrailRef []
      , trailAdd = addLogs
      }

mkBulkTestLogger :: Int -> Int -> LogTrail -> IO Logger
mkBulkTestLogger capacity delayUSec LogTrail {..} = do
  mkBulkLogger' capacity delayUSec "testLogger" trailAdd (pure ())

mkTestLogger :: Int -> LogTrail -> IO Logger
mkTestLogger capacity LogTrail {..} = do
  mkLogger' capacity "testLogger" (trailAdd . (: []))

-- | Test utility for tracking the calls the bulk logger makes in the background
-- grouping logmessages that were sent in the same action.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
