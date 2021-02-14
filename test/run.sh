#!/bin/bash

function cleanup {
    rm *.json
    rm *.wat
    rm *.wasm
}

trap cleanup EXIT

wast2json test/testsuite/i32.wast || exit 1
bin/testrunner i32.json || exit 1

wast2json test/testsuite/i64.wast || exit 1
bin/testrunner i64.json || exit 1

wast2json test/testsuite/memory.wast || exit 1
bin/testrunner memory.json || exit 1

wast2json test/testsuite/const.wast || exit 1
bin/testrunner const.json || exit 1

wast2json test/testsuite/f32.wast || exit 1
bin/testrunner f32.json || exit 1

wast2json test/testsuite/f64.wast || exit 1
bin/testrunner f64.json || exit 1

wast2json test/testsuite/local_get.wast || exit 1
bin/testrunner local_get.json || exit 1