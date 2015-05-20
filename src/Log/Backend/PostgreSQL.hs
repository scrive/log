{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
module Log.Backend.PostgreSQL (pgLogger) where

import Control.Concurrent
import Control.Exception
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Monoid
import Data.Monoid.Utils
import Data.List.Split
import Database.PostgreSQL.PQTypes

import Log.Data
import Log.Logger

-- | Create logger that inserts log messages into PostgreSQL database.
pgLogger :: ConnectionSource -> IO Logger
pgLogger cs = mkBulkLogger "PostgreSQL" $ mapM_ serialize . chunksOf 1000
  where
    serialize :: [LogMessage] -> IO ()
    serialize msgs = runDBT cs ts (runSQL_ $ "INSERT INTO logs (insertion_time, insertion_order, time, level, component, domain, message, data) VALUES" <+> mintercalate ", " (map sqlifyMessage $ zip [1..] msgs)) `catches` [
        Handler $ \(e::AsyncException) -> throwIO e
      , Handler $ \(e::SomeException) -> do
        putStrLn $ "PostgreSQL: couldn't serialize logs: " ++ show e ++ ", retrying in 10 seconds"
        threadDelay $ 10 * 1000000
        serialize msgs
      ]

    sqlifyMessage :: (Int, LogMessage) -> SQL
    sqlifyMessage (n, LogMessage{..}) = mconcat [
        "("
      , "now()"
      , "," <?> n
      , "," <?> lmTime
      , "," <?> sqlifyLevel lmLevel
      , "," <?> lmComponent
      , "," <?> Array1 lmDomain
      , "," <?> lmMessage
      , "," <?> toStrict (encode lmData) <> "::jsonb"
      , ")"
      ]

    sqlifyLevel :: LogLevel -> ByteString
    sqlifyLevel LogAttention = "attention"
    sqlifyLevel LogInfo = "info"
    sqlifyLevel LogTrace = "trace"

    ts :: TransactionSettings
    ts = def {
      tsAutoTransaction = False
    }
