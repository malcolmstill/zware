#!/bin/bash

function cleanup {
    rm *.json
    rm *.wat
    rm *.wasm
}

trap cleanup EXIT

wast2json test/testsuite/block.wast || exit 1
bin/testrunner block.json || exit 1
