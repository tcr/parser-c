#!/bin/bash

set -e
set -x

if [ "$1" = "-u" ]; then
    git submodule update --recursive --remote
fi

cd deps/happy-rust
if [ ! -f stack.yaml ]; then stack init; fi
stack build
stack exec -- happy ../../src/parser/Parser.y -o ../../src/parser/parser.rs
cd ../..

cd deps/alex-rust
if [ ! -f stack.yaml ]; then stack init; fi
stack build
stack exec -- alex ../../src/parser/Lexer.x -o ../../src/parser/lexer.rs
cd ../..
