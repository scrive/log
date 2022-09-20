# log-postgres-0.9.0.0 (2022-09-??)
* Remove deprecated `pgLogger`.
* Generalize logger related functions to `MonadUnliftIO`.

# log-postgres-0.8.1.0 (2022-04-04)
* Add support for aeson 2.0.1.0.
* Add support for GHC 9.2.
* Drop support for GHC < 8.8.

# log-postgres-0.8.0.2 (2021-10-11)
* Make compatible with log-base-0.11.0.0.

# log-postgres-0.8.0.1 (2021-07-29)
* Fix compilation issues caused by ambiguos occurence of `controlT`.

# log-postgres-0.8.0.0 (2021-06-09)
* Stop putting `insertion_time` and `insertion_order` in PostgreSQL.

# log-postgres-0.7.1.5 (2021-05-28)
* Support GHC 9.0.

# log-postgres-0.7.1.4 (2020-09-07)
* Update bounds of log-base.

# log-postgres-0.7.1.3 (2020-05-08)
* Support latest hpqtypes.

# log-postgres-0.7.1.1 (2019-05-23)
* Support latest hpqtypes.

# log-postgres-0.7.1.0 (2019-05-15)
* Support latest aeson.

# log-postgres-0.7.0.2 (2017-08-10)
* Update package description.

# log-postgres-0.7.0.1 (2017-06-20)
* Update GHC versions in 'tested-with'.
* Add missing extra-source-files (changelog, README, etc.).

# log-postgres-0.7 (2016-11-25)
* Initial release (split from the log package).
