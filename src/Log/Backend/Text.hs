-- | A logger that produces in-memory 'Text' values. Mainly useful for
-- testing.
module Log.Backend.Text ( withSimpleTextLogger ) where

import Control.Applicative
import Data.IORef
import Data.Monoid
import Prelude
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Builder as B

import Log.Data
import Log.Internal.Logger

-- | Create an in-memory logger for the duration of the given action,
-- returning both the result of the action and the logger's output as
-- a 'Text' value afterwards.
withSimpleTextLogger :: (Logger -> IO r) -> IO (T.Text, r)
withSimpleTextLogger act = do
  builderRef <- newIORef mempty
  let logger = Logger
        { loggerWriteMessage = \msg -> do
            let msg' = B.fromText $ showLogMessage Nothing msg
            modifyIORef builderRef (<> msg' <> B.fromText "\n")
        , loggerWaitForWrite = return ()
        , loggerShutdown     = return ()
        }
  r <- act logger
  txt <- L.toStrict . B.toLazyText <$> readIORef builderRef
  return (txt, r)
