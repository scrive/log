# log-0.5.4 (2016-10-21)
* New logger creation API, which is harder to misuse.
* Remove the use of finalisers in favour of the new logger API.
* Fix a JSON serialisation issue affecting the Elasticsearch back-end.
* Make the Elasticsearch back-end compatible with Elasticsearch 1.x.
* Fix a synchronisation issue affecting the Elasticsearch back-end.
* Add a test suite and Travis-based CI.

# log-0.1.0
* Initial version.
