#! /bin/bash

set -euxo pipefail

SRC_BASENAME=$(cabal info . | awk '{print $2;exit}')

if [ -f configure.ac ]; then autoreconf -i; fi
rm -rf dist/
cabal sdist # test that a source-distribution can be generated
cd dist/
tar -xvf "./$SRC_BASENAME.tar.gz"
cd "$SRC_BASENAME/"
## from here on, CWD is inside the extracted source-tarball
rm -fv cabal.project.local
echo 'packages: ., ../../../log-base, ../../../log-elasticsearch, ../../../log-postgres' > "cabal.project"
# this builds all libraries and executables (without tests/benchmarks)
rm -f cabal.project.freeze
cabal new-build -w ${HC} --disable-tests --disable-benchmarks all
# this builds all libraries and executables (including tests/benchmarks)
# - rm -rf ./dist-newstyle

# build & run tests
cabal new-build -w ${HC} ${TEST} ${BENCH} all
if [ "x$TEST" = "x--enable-tests" ]; then cabal new-test -w ${HC} ${TEST} all; fi
