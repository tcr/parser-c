#!/bin/bash

set -e
set -x

cd parser-c-tests

cargo test --release -- "$@"
