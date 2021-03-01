# log-base-0.9.1.0 (2021-03-01)
* Add a `LogList` logger

# log-base-0.9.0.0 (2020-09-07)
* Always make data attached to a log message a json object
* Add unliftio-core-0.2 compatiblity
* Tidy up flushing stdout in stdout loggers
* Use `simpleStdoutLogger` in `withSimpleStdOutLogger` instead of `stdoutLogger`
* Remove deprecated functions
* Add JSON loggers
* Make `mkLogger` use bounded queue internally (similar to `mkBulkLogger`)
* Get rid of a space leak in bounded queue used in `mkBulkLogger`

# log-base-0.8.0.1 (2020-05-08)
* Update version bounds.

# log-base-0.8.0.0 (2019-04-09)
* Add `getLoggerEnv` function to `MonadLog` class, add `getLoggerIO`
  utility ([#46](https://github.com/scrive/log/pull/46)).
* Add a `MonadUnliftIO` instance for `LogT`
  ([#47](https://github.com/scrive/log/pull/47)).

# log-base-0.7.4.0 (2017-10-27)
* Add `mkBulkLogger'` ([#40](https://github.com/scrive/log/pull/40)).

# log-base-0.7.3.0 (2017-10-10)
* `BasicStdOutLogger` now flushes stdout on each write. `BulkStdOutLogger`
  now flushes stdout on each bulk write ([#38](https://github.com/scrive/log/issues/38)).

# log-base-0.7.2.0 (2017-08-10)
* Add 'MFunctor LogT' instance ([#35](https://github.com/scrive/log/issues/35) ).

# log-base-0.7.1.1 (2017-06-19)
* mkBulkLogger now uses a bounded queue to interact with the logger thread.

# log-base-0.7.1 (2017-03-16)
* Added a few MTL class instances ([#28](https://github.com/scrive/log/issues/28)).

# log-base-0.7 (2016-11-25)
* Initial release (split from the log package).
* Improved documentation ([#22](https://github.com/scrive/log/issues/22)).
* Implement 'toEncoding' directly in 'ToJSON' instances ([#21](https://github.com/scrive/log/issues/21)).
