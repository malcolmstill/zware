#!/bin/bash
wast2json test/testsuite/block.wast
bin/testrunner test/testsuite/ block.json
rm *.json
rm *.wat
rm *.wasm
