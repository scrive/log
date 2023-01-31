# log-elasticsearch-0.13.0.1 (2023-01-31)
* Add support for OpenSearch.

# log-elasticsearch-0.13.0.0 (2022-09-21)
* Remove deprecated `elasticSearchLogger`.
* Use `http-client-openssl` by default instead of `http-client-tls`.
* Generalize logger related functions to `MonadUnliftIO`.

# log-elasticsearch-0.12.2.0 (2022-04-04)
* Add support for aeson 2.0.1.0.
* Add support for GHC 9.2.
* Drop support for GHC 8.0 and 8.2.

# log-elasticsearch-0.12.1.1 (2021-10-11)
* Make compatible with log-base-0.11.0.0.

# log-elasticsearch-0.12.1.0 (2021-06-23)
* Map `time` as `date_nanos` in ElasticSearch >= 7.0.

# log-elasticsearch-0.12.0.0 (2021-06-09)
* Stop putting `insertion_time` and `insertion_order` in ElasticSearch.

# log-elasticsearch-0.11.0.0 (2020-08-24)
* Drop dependency on bloodhound
* Unify V1 and V5 specific modules
* Add support for ElasticSearch 7.x
* Try harder to insert problematic messages into ElasticSearch
* Support building with GHC 8.8.4 and GHC 8.10.2

# log-elasticsearch-0.10.2.0 (2020-02-03)
* Modify data keys in deterministic manner in case of ES insertion failure

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
