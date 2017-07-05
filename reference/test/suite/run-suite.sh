#!/bin/bash
source ./configuration

if [ -z "$1" ] ; then
    echo "Usage: ./run-suite.sh <suite> <gcc-args>.." >&2
    exit 1
fi
TEST_SUITE=$1
shift
bash clear_test_suite $TEST_SUITE
source $CTEST_BINDIR/set_test_suite $TEST_SUITE
export CTEST_DRIVER=CRoundTrip

pushd $TEST_SUITE
for cf in `find . -name '*.c'`; do
    echo "[INFO] Running Test $TEST_SUITE::$cf"
    bash run-test $@ $cf
done
