#!/bin/bash
source ./configuration

source $CTEST_BINDIR/setup_test_suite smoke

# export CTEST_DEBUG=1

cd smoke

export CTEST_DRIVER=CParse
# sh run-test doesnotexist.c
export CTEST_NON_PARSE=1
bash run-test test_non_parse.c
export CTEST_NON_PARSE=0
bash run-test test.c

export CTEST_DRIVER=CRoundTrip
for f in `ls *.c | grep -v non_parse | grep -v equiv`; do bash run-test $f; done;

export CTEST_DRIVER=CEquiv
export CTEST_NON_EQUIV=1
bash run-test test.c test1.c
bash run-test test_attr.non_equiv_1.c test_attr.non_equiv_2.c
unset CTEST_NON_EQUIV
bash run-test test.c test.c

cd ../decls
export CTEST_DRIVER=CRoundTrip
for f in `ls *.c | grep -v non_parse | grep -v equiv`; do bash run-test $f; done;
