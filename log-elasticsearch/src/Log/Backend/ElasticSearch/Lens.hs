{-# LANGUAGE RankNTypes #-}
-- | Lensified version of "Log.Backend.ElasticSearch".
module Log.Backend.ElasticSearch.Lens
  ( I.ElasticSearchConfig
  , esServer
  , esIndex
  , esShardCount
  , esReplicaCount
  , esMapping
  , esLogin
  , esLoginInsecure
  , I.defaultElasticSearchConfig
  , I.withElasticSearchLogger
  ) where

import qualified Data.Text as T
import qualified Log.Backend.ElasticSearch as I

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- | Elasticsearch server address.
esServer :: Lens' I.ElasticSearchConfig T.Text
esServer f esc = (\x -> esc { I.esServer = x }) <$> f (I.esServer esc)
{-# INLINE esServer #-}

-- | Elasticsearch index name.
esIndex :: Lens' I.ElasticSearchConfig T.Text
esIndex f esc = (\x -> esc { I.esIndex = x }) <$> f (I.esIndex esc)
{-# INLINE esIndex #-}

-- | Elasticsearch shard count for the named index.
--
-- @since 0.10.0.0
esShardCount :: Lens' I.ElasticSearchConfig Int
esShardCount f esc = (\x -> esc { I.esShardCount = x }) <$> f (I.esShardCount esc)
{-# INLINE esShardCount #-}

-- | Elasticsearch replica count for the named index.
--
-- @since 0.10.0.0
esReplicaCount :: Lens' I.ElasticSearchConfig Int
esReplicaCount f esc = (\x -> esc { I.esReplicaCount = x }) <$> f (I.esReplicaCount esc)
{-# INLINE esReplicaCount #-}

-- | Elasticsearch mapping name.
esMapping :: Lens' I.ElasticSearchConfig T.Text
esMapping f esc = (\x -> esc { I.esMapping = x }) <$> f (I.esMapping esc)
{-# INLINE esMapping #-}

-- |  Elasticsearch basic authentication username and password.
esLogin :: Lens' I.ElasticSearchConfig (Maybe (T.Text, T.Text))
esLogin f esc = (\x -> esc { I.esLogin = x }) <$> f (I.esLogin esc)
{-# INLINE esLogin #-}

-- | Allow basic authentication over non-TLS connections.
esLoginInsecure :: Lens' I.ElasticSearchConfig Bool
esLoginInsecure f esc = (\x -> esc { I.esLoginInsecure = x }) <$> f (I.esLoginInsecure esc)
{-# INLINE esLoginInsecure #-}
