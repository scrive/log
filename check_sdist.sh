#! /bin/sh

cabal sdist
SRC_BASENAME=$(cabal info . | awk '{print $2;exit}')
SRC_NAME=`echo $SRC_BASENAME | sed 's/-[0-9]\.[0-9]*//'`
tar -C dist/ -xf dist/$SRC_BASENAME.tar.gz
echo 'packages: ., ../../../log-base, ../../../log-elasticsearch, ../../../log-postgres' > "dist/$SRC_BASENAME/cabal.project"
cd dist/$SRC_BASENAME/
cabal new-build --disable-tests --disable-benchmarks $SRC_NAME
rm -rf ./dist-newstyle
cabal new-build ${TEST} ${BENCH} $SRC_NAME
