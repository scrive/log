{-# LANGUAGE CPP #-}
module Log.Internal.Aeson.Compat
  ( module Map
#if !MIN_VERSION_aeson(2,0,1)
  , KeyMap
#endif
  , fromText
  , doName
  ) where

import Data.Text
import Prelude

#if MIN_VERSION_aeson(2,0,1)
import Data.Aeson.KeyMap as Map 
import qualified Data.Aeson.Key as K

fromText :: Text -> K.Key
fromText = K.fromText

doName :: Monad m => (Text -> m Text) -> K.Key -> m K.Key
doName f = fmap K.fromText . f . K.toText

#else

import Data.HashMap.Strict as Map

type KeyMap a = Map.HashMap Text a

fromText :: Text -> Text
fromText = id

doName :: (Text -> m Text) -> Text -> m Text
doName = id

#endif
