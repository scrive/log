# log-elasticsearch-0.9.1.0 (2017-08-10)
* Add 'toEncoding' to 'ToJSON LogsMapping' instance

# log-elasticsearch-0.9.0.1 (2017-06-19)
* 'withElasticSearchLogger' no longer fails when the Elasticsearch server is down.

# log-elasticsearch-0.9.0.0 (2017-05-04)
* Now works with bloodhound-0.14.0.0 (#30).

# log-elasticsearch-0.8.1 (2017-03-27)
* Log.Backend.ElasticSearch.Internal now exports 'EsUsername' and
  'EsPassword'.

# log-elasticsearch-0.8 (2017-03-16)
* Made ElasticSearchConfig an abstract type (#27).
* Added support for HTTPS and basic auth (#26).

# log-elasticsearch-0.7 (2016-11-25)
* Initial release (split from the log package).
