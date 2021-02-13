#!/bin/bash
wast2json test/testsuite/block.wast || exit 1
bin/testrunner block.json || exit 1
rm *.json
rm *.wat
rm *.wasm
