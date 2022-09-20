# log

[![Hackage version](https://img.shields.io/hackage/v/log-base.svg?label=Hackage)](https://hackage.haskell.org/package/log-base)
[![Build Status](https://github.com/scrive/log/workflows/Haskell-CI/badge.svg?branch=master)](https://github.com/scrive/log/actions?query=branch%3Amaster)

A set of libraries that provide a way to record structured log messages with
multiple backends.

Supported backends:

* Standard output via
  [`log-base`](https://hackage.haskell.org/package/log-base).
* Elasticsearch via
  [`log-elasticsearch`](https://hackage.haskell.org/package/log-elasticsearch).
* PostgreSQL via
  [`log-postgres`](https://hackage.haskell.org/package/log-postgres).

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Log
import Log.Backend.ElasticSearch

main :: IO ()
main = do
  let config = defaultElasticSearchConfig
        { esServer  = "http://localhost:9200"
        , esIndex   = "logs"
        }
  withElasticSearchLogger config $ \logger -> do
    runLogT "main" logger defaultLogLevel $ do
      logInfo_ "Hi there"
```
