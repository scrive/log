{-# LANGUAGE RankNTypes #-}
-- | Lensified version of "Log.Backend.ElasticSearch".
module Log.Backend.ElasticSearch.Lens (
    I.ElasticSearchConfig
  , esServer
  , esIndex
  , esMapping
  , esLogin
  , esLoginInsecure
  , I.defaultElasticSearchConfig
  , I.withElasticSearchLogger
  ) where

import Database.Bloodhound hiding (Status)
import Prelude
import qualified Data.Text as T
import qualified Log.Backend.ElasticSearch as I
import qualified Log.Backend.ElasticSearch.Internal ()

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- | Elasticsearch server address.
esServer :: Lens' I.ElasticSearchConfig T.Text
esServer f esc = fmap (\x -> esc { I.esServer = x }) $ f (I.esServer esc)

-- | Elasticsearch index name.
esIndex :: Lens' I.ElasticSearchConfig T.Text
esIndex f esc = fmap (\x -> esc { I.esIndex = x }) $ f (I.esIndex esc)

-- | Elasticsearch mapping name.
esMapping :: Lens' I.ElasticSearchConfig T.Text
esMapping f esc = fmap (\x -> esc { I.esMapping = x }) $ f (I.esMapping esc)

-- |  Elasticsearch basic authentication username and password.
esLogin :: Lens' I.ElasticSearchConfig (Maybe (EsUsername, EsPassword))
esLogin f esc = fmap (\x -> esc { I.esLogin = x }) $ f (I.esLogin esc)

-- | Allow basic authentication over non-TLS connections.
esLoginInsecure :: Lens' I.ElasticSearchConfig Bool
esLoginInsecure f esc = fmap (\x -> esc { I.esLoginInsecure = x }) $ f (I.esLoginInsecure esc)
