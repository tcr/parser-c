#!/bin/bash

set -e
set -x

cd parser-c-tests

if [ ! -d gcc_pre ]; then
    echo "Unpacking GCC test suite files..."
    # TODO: update this location at some point...
    tar xjf ../reference/test/harness/parse_dg/gcc_dg_pre.tar.bz2
    cp ../reference/test/harness/parse_dg/expect_fail.txt gcc_expect_fail.txt
fi

# Using release because the gcc_dg tests are too slow in dev mode...
cargo test --release -- "$@"
