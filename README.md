# log

[![Hackage version](https://img.shields.io/hackage/v/log-base.svg?label=Hackage)](https://hackage.haskell.org/package/log-base)
[![Build Status](https://github.com/well-typed/optics/workflows/Haskell-CI/badge.svg?branch=master)](https://github.com/scrive/log/actions?query=branch%3Amaster)

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
    runLogT "main" logger defaultLogLevel $ do
      logTrace_ "foo"
```
