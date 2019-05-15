# log-elasticsearch-0.10.1.0 (2019-05-15)
* Add a Generic instance for ElasticSearchConfig.

# log-elasticsearch-0.10.0.1 (2019-01-24)
* Compatibility with ES 6.x by using "text" instead of deprecated "string"

# log-elasticsearch-0.10.0.0 (2018-03-28)
* Expose `checkElasticSearchConnection` and `checkElasticSearchLogin`.
* Add config parameters for number of shards and replicas to
  `ElasticSearchConfig` (and make default of 4 shards and 1 replica
  explicit in 'defaultElasticSearchConfig').

# log-elasticsearch-0.9.1.0 (2017-08-10)
* Add `toEncoding` to `ToJSON LogsMapping` instance

# log-elasticsearch-0.9.0.1 (2017-06-19)
* `withElasticSearchLogger` no longer fails when the Elasticsearch
  server is down.

# log-elasticsearch-0.9.0.0 (2017-05-04)
* Now works with bloodhound-0.14.0.0 (#30).

# log-elasticsearch-0.8.1 (2017-03-27)
* `Log.Backend.ElasticSearch.Internal` now exports `EsUsername` and
  `EsPassword`.

# log-elasticsearch-0.8 (2017-03-16)
* Made `ElasticSearchConfig` an abstract type (#27).
* Added support for HTTPS and basic auth (#26).

# log-elasticsearch-0.7 (2016-11-25)
* Initial release (split from the log package).
