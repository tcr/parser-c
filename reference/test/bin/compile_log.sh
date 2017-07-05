#!/bin/bash
if [ -z "${BINDIR}" ] ; then
    echo "$0: BINDIR not set. Exit." >&2
    exit 1
fi
if [ -z "${1}" ] ; then
    echo "$0: Exactly one argument is required, but none was provided. Exit." >&2
    exit 1
fi

# replace position information
REPLACE1='$_ = $_.gsub(/[\w_-]+\.c:[\d:]*/,"@POS")'
REPLACE2='$_ = $_.gsub(/<stdin>:[\d:]*/,"@POS")'
gcc -fsyntax-only $1.c  2>&1 1>/dev/null | ruby -pe "${REPLACE1}" | grep '@POS'  > $1.log
$BINDIR/CTest $1.c | gcc -x c -fsyntax-only - 2>&1 1>/dev/null | ruby -pe "${REPLACE2}" | grep '@POS'  > $1_test.log
diff -u $1.log $1_test.log
