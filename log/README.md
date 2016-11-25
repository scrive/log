# log [![Hackage version](https://img.shields.io/hackage/v/log.svg?label=Hackage)](https://hackage.haskell.org/package/log) [![Build Status](https://secure.travis-ci.org/scrive/log.svg?branch=master)](http://travis-ci.org/scrive/log)

A library that provides a way to record structured log messages with
multiple back ends.

Supported back ends:

* Standard output
* Elasticsearch
* PostgreSQL

The `log` library provides Elasticsearch and PostgreSQL back ends. If
you only need one of those, use `log-base` and `log-elasticsearch` or
`log-postgres`.

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Log
import Log.Backend.ElasticSearch

import System.Random

main :: IO ()
main = do
  let config = ElasticSearchConfig {
        esServer  = "http://localhost:9200",
        esIndex   = "logs",
        esMapping = "log"
        }
  withElasticSearchLogger config randomIO $ \logger ->
    runLogT "main" logger $ do
      logTrace_ "foo"
```
