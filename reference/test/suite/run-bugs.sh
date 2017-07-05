#!/bin/bash
source ./configuration

source $CTEST_BINDIR/setup_test_suite bugs

# export CTEST_DEBUG=1
export CTEST_DRIVER=CRoundTrip

cd bugs
export CTEST_DRIVER=CRoundTrip
# TODO: NonCompile test driver
for f in `ls *.c | grep -v non_compile | grep -v concat | grep -v intconst | grep -v elseif`;  do
        bash run-test $f
done
export CTEST_DRIVER=CParse
export CTEST_NON_PARSE=1
for f in `ls *.c | grep non_compile`;  do
	echo "Checking if $f does NOT compile"
    bash run-test $f
done

