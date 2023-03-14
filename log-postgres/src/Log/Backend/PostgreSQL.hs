-- | PostgreSQL logging back-end.
module Log.Backend.PostgreSQL (withPgLogger) where

import Control.Concurrent
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.IO.Unlift
import Data.Aeson ((.=), Value(..), object, encode)
import Data.List.Split
import Data.Monoid.Utils
import Data.String
import Data.Typeable
import Database.PostgreSQL.PQTypes
import qualified Data.ByteString.Base64 as B64
import qualified Data.Foldable as Foldable
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

import Log.Data
import Log.Logger
import Log.Internal.Logger
import qualified Log.Internal.Aeson.Compat as AC

newtype InvalidEncodingRecoveryAttempt = Attempt Int
  deriving Enum

-- | Create a 'pgLogger' for the duration of the given action, and
-- shut it down afterwards, making sure that all buffered messages are
-- actually written to the DB.
withPgLogger :: MonadUnliftIO m => ConnectionSourceM IO -> (Logger -> m r) -> m r
withPgLogger cs act = withRunInIO $ \unlift -> do
  logger <- pgLogger cs
  withLogger logger (unlift . act)

-- | Start an asynchronous logger thread that inserts log messages
-- into a PostgreSQL database.
--
-- Please use 'withPglogger' instead, which is more exception-safe
-- (see the note attached to 'mkBulkLogger').
pgLogger :: ConnectionSourceM IO -> IO Logger
pgLogger cs = mkBulkLogger loggerName
              (mapM_ (serialize $ Attempt 1) . chunksOf 1000)
              (return ())
  where
    loggerName :: IsString s => s
    loggerName = "PostgreSQL"

    sqlInsertLog :: SQL
    sqlInsertLog = "INSERT INTO logs "
      <+> "(time, level, component, domain, message, data) VALUES"

    serialize :: InvalidEncodingRecoveryAttempt -> [LogMessage] -> IO ()
    serialize !attempt msgs = runDBT cs ts
      (runSQL_ $ sqlInsertLog
       <+> mintercalate ", " (map sqlifyMessage msgs))
      `catches` [
        -- Propagate base async exceptions thrown by the runtime system.
        Handler $ \(e::AsyncException) -> throwIO e
      , Handler $ \(e::SomeException) -> case fromException e of
        Just dbe@DBException{}
          | Just qe <- getEncodingQueryError dbe -> case attempt of
            Attempt 1 -> do
              -- If a client uses UTF-8 encoding (TODO: in fact it should
              -- always be the case as Text is encoded as UTF-8 for sql
              -- serialization), then this error occurs only when any of the
              -- strings we want to serialize contains NULL bytes. In such
              -- case we scan the logs and replace each NULL with "\0".
              putStrLn $ loggerName
                ++ ": couldn't serialize logs due to character encoding error \""
                ++ qeMessagePrimary qe ++ "\", removing NULL bytes and retrying"
              serialize (succ attempt) $ map (\msg ->
                -- If any text inside the message had NULL bytes,
                -- add acknowledgment of that fact to its data.
                case runState (mapTexts removeNULLs msg) False of
                  (newMsg, True) -> newMsg {
                    lmData = lmData newMsg
                             `addPair` ("_log", "NULL bytes were escaped")
                  }
                  (_, False) -> msg) msgs
            Attempt 2 -> do
              -- This should never happen, but let us be paranoid for
              -- a minute. If the error is still happening after removal
              -- of NULL bytes, go through each message and encode all
              -- texts as base64, effectively transforming them into ASCII.
              putStrLn $ loggerName
                ++ ": couldn't serialize logs due to character encoding error \""
                ++ qeMessagePrimary qe
                ++ "\" after NULL bytes were removed, encoding all texts"
                ++ " in the problematic batch as base64 to make them ASCII"
              serialize (succ attempt) $ map (\msg ->
                let newMsg = runIdentity $ mapTexts convertBase64 msg
                 in newMsg {
                  lmData = lmData newMsg
                    `addPair` ("_log", "Texts encoded as base64")
                }) msgs
            Attempt _ -> do
              -- This can't happen, all texts are ASCII now.
              putStrLn $ loggerName
                ++ ": impossible happened "
                ++ "(>2 attempt failed because of character encoding error \""
                ++ qeMessagePrimary qe
                ++ "\" even though all texts are ASCII), skipping the batch"
        _ -> do
          putStrLn $ loggerName
            ++ ": couldn't serialize logs:"
            <+> show e ++ ", retrying in 10 seconds"
          threadDelay $ 10 * 1000000
          -- Do not increment the attempt here, it's used to
          -- track invalid encoding recovery attempts only.
          serialize attempt msgs
      ]

    addPair :: Value -> (T.Text, Value) -> Value
    addPair data_ (name, value) = case data_ of
      Object obj -> Object $ AC.insert (AC.fromText name) value obj
      _          -> object
                    [ "_data" .= data_
                    , "_log"  .= value
                    ]

    getEncodingQueryError :: DBException -> Maybe DetailedQueryError
    getEncodingQueryError DBException{..}
      | Just (qe::DetailedQueryError) <- cast dbeError
      ,    qeErrorCode qe == CharacterNotInRepertoire
        || qeErrorCode qe == UntranslatableCharacter = Just qe
      | otherwise = Nothing

    convertBase64 :: T.Text -> Identity T.Text
    convertBase64 = return . T.decodeLatin1 . B64.encode . T.encodeUtf8

    removeNULLs :: T.Text -> State Bool T.Text
    removeNULLs s = do
      let newS = T.replace "\0" "\\0" s
      when (T.length newS /= T.length s) $ put True
      return newS

    mapTexts :: forall m. (Applicative m, Monad m)
             => (T.Text -> m T.Text) -> LogMessage -> m LogMessage
    mapTexts doText lm = do
      component <- doText      $ lmComponent lm
      domain    <- mapM doText $ lmDomain lm
      message   <- doText      $ lmMessage lm
      data_     <- doValue     $ lmData lm
      return lm {
        lmComponent = component
      , lmDomain    = domain
      , lmMessage   = message
      , lmData      = data_
      }
      where
        doValue :: Value -> m Value
        doValue (Object obj) =
          Object <$> Foldable.foldrM
          (\(name, value) acc ->
             AC.insert <$> AC.doName doText name <*> doValue value <*> pure acc)
          AC.empty (AC.toList obj)
        doValue (Array arr)  = Array <$> V.mapM doValue arr
        doValue (String s)   = String <$> doText s
        doValue v            = return v

    sqlifyMessage :: LogMessage -> SQL
    sqlifyMessage LogMessage{..} = mconcat [
        "("
      , "," <?> lmTime
      , "," <?> showLogLevel lmLevel
      , "," <?> lmComponent
      , "," <?> Array1 lmDomain
      , "," <?> lmMessage
      , "," <?> JSONB (encode lmData)
      , ")"
      ]

    ts :: TransactionSettings
    ts = defaultTransactionSettings {
      tsAutoTransaction = False
    }
