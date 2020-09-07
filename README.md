# log [![Hackage version](https://img.shields.io/hackage/v/log-base.svg?label=Hackage)](https://hackage.haskell.org/package/log-base) [![Build Status](https://secure.travis-ci.org/scrive/log.svg?branch=master)](http://travis-ci.org/scrive/log)

A set of libraries that provide a way to record structured log
messages with multiple back ends.

Supported back ends:

* Standard output
* Elasticsearch
* PostgreSQL

The `log-base` library provides only the basic scaffolding and the
stdout back end. Additional back ends are provided by
`log-elasticsearch` and `log-postgres`.

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Log
import Log.Backend.ElasticSearch

main :: IO ()
main = do
  let config = defaultElasticSearchConfig {
        esServer  = "http://localhost:9200",
        esIndex   = "logs",
        esMapping = "log"
        }
  withElasticSearchLogger config $ \logger ->
    runLogT "main" logger $ do
      logTrace_ "foo"
```
