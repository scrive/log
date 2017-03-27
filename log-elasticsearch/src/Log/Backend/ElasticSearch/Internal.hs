module Log.Backend.ElasticSearch.Internal
  (ElasticSearchConfig(..)
  ,defaultElasticSearchConfig
  ,EsUsername(..)
  ,EsPassword(..))
where

import Database.Bloodhound hiding (Status)
import Prelude
import qualified Data.Text as T

-- | Configuration for the Elasticsearch 'Logger'. See
-- <https://www.elastic.co/guide/en/elasticsearch/reference/current/glossary.html>
-- for the explanation of terms.
data ElasticSearchConfig = ElasticSearchConfig {
    esServer        :: !T.Text -- ^ Elasticsearch server address.
  , esIndex         :: !T.Text -- ^ Elasticsearch index name.
  , esMapping       :: !T.Text -- ^ Elasticsearch mapping name.
  , esLogin         :: Maybe (EsUsername, EsPassword) -- ^ Elasticsearch basic authentication username and password.
  , esLoginInsecure :: !Bool   -- ^ Allow basic authentication over non-TLS connections.
  } deriving (Eq, Show)

-- | Sensible defaults for 'ElasticSearchConfig'.
defaultElasticSearchConfig :: ElasticSearchConfig
defaultElasticSearchConfig = ElasticSearchConfig {
  esServer        = "http://localhost:9200",
  esIndex         = "logs",
  esMapping       = "log",
  esLogin         = Nothing,
  esLoginInsecure = False
  }
