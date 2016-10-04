# log [![Hackage version](https://img.shields.io/hackage/v/log.svg?label=Hackage)](https://hackage.haskell.org/package/log) [![Build Status](https://secure.travis-ci.org/scrive/log.svg?branch=master)](http://travis-ci.org/scrive/log)

A library that provides a way to record structured log messages with
multiple backends.

Supported backends:

* standard output
* Elasticsearch
* PostgreSQL


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
  logger <- elasticSearchLogger config randomIO
  runLogT "main" logger $ do
    logTrace_ "foo"
```
