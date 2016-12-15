{-# LANGUAGE RankNTypes #-}
module Log.Backend.ElasticSearch.Lens (
    esServer
  , esIndex
  , esMapping
  , esLogin
  , esLoginInsecure
  ) where

import Database.Bloodhound hiding (Status)
import Prelude
import qualified Data.Text as T
import qualified Log.Backend.ElasticSearch.Internal as I

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

esServer :: Lens' I.ElasticSearchConfig T.Text
esServer f esc = fmap (\x -> esc { I.esServer = x }) $ f (I.esServer esc)

esIndex :: Lens' I.ElasticSearchConfig T.Text
esIndex f esc = fmap (\x -> esc { I.esIndex = x }) $ f (I.esIndex esc)

esMapping :: Lens' I.ElasticSearchConfig T.Text
esMapping f esc = fmap (\x -> esc { I.esMapping = x }) $ f (I.esMapping esc)

esLogin :: Lens' I.ElasticSearchConfig (Maybe (EsUsername, EsPassword))
esLogin f esc = fmap (\x -> esc { I.esLogin = x }) $ f (I.esLogin esc)

esLoginInsecure :: Lens' I.ElasticSearchConfig Bool
esLoginInsecure f esc = fmap (\x -> esc { I.esLoginInsecure = x }) $ f (I.esLoginInsecure esc)
