#!/bin/sh
set -ex

DIR=log-elasticsearch/src/Log/Backend/ElasticSearch/
FILES="$DIR/V1/Lens.hs $DIR/V1/Internal.hs"

for SRC in $FILES; do
    DEST=$(echo $SRC|sed s/V1/V5/)

    cat $SRC | sed s/V1/V5/ > $DEST
done
