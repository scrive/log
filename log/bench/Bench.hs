module Main where

import Log
import Log.Backend.ElasticSearch.V5
import Log.Backend.PostgreSQL

import Control.Concurrent
import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Database.PostgreSQL.PQTypes
import Data.Monoid
import System.Environment
import System.Random
import TextShow

import qualified Data.Text as T
import qualified System.Remote.Monitoring as EKG

-- Usage:
--
-- 1. (Optional) Start elasticsearch/postgres
-- 2. Run the benchmark exe with the appropriate argument ('postgres'/'elastic').
-- 3. Open htop and/or localhost:8000 in the browser
-- 4. Stop elasticsearch/postgres
-- 5. Observe the memory allocation behaviour.

main :: IO ()
main = do
  void $ EKG.forkServer "localhost" 8000
  args <- getArgs
  case args of
    ["postgres"]             -> benchPostgres defConnString
    ["postgres", connString] -> benchPostgres (T.pack connString)
    ["elastic"]              -> benchElastic
    _                        -> benchElastic

defConnString :: T.Text
defConnString = "postgresql://user:password@localhost/log-postgresql-bench"

benchPostgres :: T.Text -> IO ()
benchPostgres connString = do
  putStrLn "postgres"
  ConnectionSource connSource <- poolSource def { csConnInfo = connString } 1 10 1
  withPgLogger connSource $
    \logger -> forever $ benchLogger logger

benchElastic :: IO ()
benchElastic = do
  putStrLn "elastic"
  let config = defaultElasticSearchConfig
  withElasticSearchLogger config randomIO $
    \logger -> forever $ benchLogger logger


benchLogger :: (MonadIO m, MonadTime m, MonadBase IO m) => Logger -> m ()
benchLogger logger= do
  liftIO $ putStrLn "writing 100 000 log messages..."

  runLogT "log-bench-elasticsearch" logger $
    forM_ [0..10000] $ \(i :: Int) ->
    logTrace_ ("kaboozle kaboozle kaboozle kaboozle kaboozle " <> showt i)

  liftIO $ putStrLn "sleeping for 1 s..."
  liftIO $ threadDelay 1000000 {- 1 sec -}
