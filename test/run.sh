#!/bin/bash

function cleanup {
    rm *.json
    rm *.wat
    rm *.wasm
}

trap cleanup EXIT

wast2json test/testsuite/br_if.wast || exit 1
bin/testrunner br_if.json || exit 1

wast2json test/testsuite/br.wast || exit 1
bin/testrunner br.json || exit 1

wast2json test/testsuite/br_table.wast || exit 1
bin/testrunner br_table.json || exit 1

wast2json test/testsuite/call_indirect.wast || exit 1
bin/testrunner call_indirect.json || exit 1

wast2json test/testsuite/call.wast || exit 1
bin/testrunner call.json || exit 1

wast2json test/testsuite/const.wast || exit 1
bin/testrunner const.json || exit 1

wast2json test/testsuite/f32.wast || exit 1
bin/testrunner f32.json || exit 1

wast2json test/testsuite/f64.wast || exit 1
bin/testrunner f64.json || exit 1

wast2json test/testsuite/i32.wast || exit 1
bin/testrunner i32.json || exit 1

wast2json test/testsuite/i64.wast || exit 1
bin/testrunner i64.json || exit 1


wast2json test/testsuite/local_get.wast || exit 1
bin/testrunner local_get.json || exit 1

wast2json test/testsuite/local_set.wast || exit 1
bin/testrunner local_set.json || exit 1

wast2json test/testsuite/local_tee.wast || exit 1
bin/testrunner local_tee.json || exit 1

wast2json test/testsuite/memory.wast || exit 1
bin/testrunner memory.json || exit 1