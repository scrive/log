# log-0.7 (2016-11-25)
* Split into four libraries (log, log-base, log-postgres,
  log-elasticsearch).

# log-0.6 (2016-11-22)
* Moved 'withLogger' to 'Log.Internal.Logger'.

# log-0.5.7 (2016-11-22)
* Remove the dependency on 'cond'.
* Fix formatting in 'mkBulkLogger' haddocks (#16).
* Generalise the types of 'logAttention', 'logInfo' and 'logTrace'
  (#17).

# log-0.5.5 (2016-11-16)
* Add an in-memory logging backend for testing (#13).
* Fix the deprecation message for stdout logger.

# log-0.5.4 (2016-10-21)
* New logger creation API, which is harder to misuse.
* Remove the use of finalisers in favour of the new logger API.
* Fix a JSON serialisation issue affecting the Elasticsearch back-end.
* Make the Elasticsearch back-end compatible with Elasticsearch 1.x.
* Fix a synchronisation issue affecting the Elasticsearch back-end.
* Add a test suite and Travis-based CI.

# log-0.1.0
* Initial version.
