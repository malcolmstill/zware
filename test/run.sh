#!/bin/bash
wast2json test/testsuite/block.wast
ls 
bin/testrunner block.json
rm *.json
rm *.wat
rm *.wasm
