#! /bin/sh

if [ "x$ELASTICSEARCH" = "x5.1.2" ]; then
    ELASTICSEARCH_URL=https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-$ELASTICSEARCH.tar.gz;
else
    ELASTICSEARCH_URL=https://download.elastic.co/elasticsearch/elasticsearch/elasticsearch-$ELASTICSEARCH.tar.gz;
fi

travis_retry wget --no-check-certificate $ELASTICSEARCH_URL
tar xzf elasticsearch-$ELASTICSEARCH.tar.gz
echo "path.repo = [\"/tmp\"]" >> ./elasticsearch-$ELASTICSEARCH/elasticsearch.yml

if [ "x$ELASTICSEARCH" = "x1.5.2" ]; then
    ES_VERSION_ARG=-v; else ES_VERSION_ARG=--version;
fi

./elasticsearch-$ELASTICSEARCH/bin/elasticsearch $ES_VERSION_ARG
./elasticsearch-$ELASTICSEARCH/bin/elasticsearch &
