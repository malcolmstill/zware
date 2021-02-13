#!/bin/bash

function cleanup {
    rm *.json
    rm *.wat
    rm *.wasm
}

trap cleanup EXIT

wast2json test/testsuite/i32.wast || exit 1
bin/testrunner i32.json || exit 1
wast2json test/testsuite/block.wast || exit 1
bin/testrunner block.json || exit 1
