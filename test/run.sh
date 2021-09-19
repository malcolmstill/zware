#!/bin/bash

set -e

function cleanup {
    rm *.json
    rm *.wat
    rm *.wasm
}

trap cleanup EXIT

wast2json --version

wast2json test/testsuite/address.wast
bin/testrunner address.json

wast2json test/testsuite/align.wast
bin/testrunner align.json

wast2json test/testsuite/binary-leb128.wast
bin/testrunner binary-leb128.json

wast2json test/testsuite/binary.wast
bin/testrunner binary.json

wast2json test/testsuite/block.wast
bin/testrunner block.json

wast2json test/testsuite/br_if.wast
bin/testrunner br_if.json

wast2json test/testsuite/br.wast
bin/testrunner br.json

wast2json test/testsuite/br_table.wast
bin/testrunner br_table.json

wast2json test/testsuite/call_indirect.wast
bin/testrunner call_indirect.json

wast2json test/testsuite/call.wast
bin/testrunner call.json

wast2json test/testsuite/comments.wast
bin/testrunner comments.json

wast2json test/testsuite/const.wast
bin/testrunner const.json

wast2json test/testsuite/conversions.wast
bin/testrunner conversions.json

wast2json test/testsuite/custom.wast
bin/testrunner custom.json

wast2json test/testsuite/data.wast
bin/testrunner data.json

wast2json test/testsuite/elem.wast
bin/testrunner elem.json

wast2json test/testsuite/endianness.wast
bin/testrunner endianness.json

wast2json test/testsuite/exports.wast
bin/testrunner exports.json

wast2json test/testsuite/f32_bitwise.wast
bin/testrunner f32_bitwise.json

wast2json test/testsuite/f32_cmp.wast
bin/testrunner f32_cmp.json

wast2json test/testsuite/f32.wast
bin/testrunner f32.json

wast2json test/testsuite/f64_bitwise.wast
bin/testrunner f64_bitwise.json

wast2json test/testsuite/f64_cmp.wast
bin/testrunner f64_cmp.json

wast2json test/testsuite/f64.wast
bin/testrunner f64.json

wast2json test/testsuite/fac.wast
bin/testrunner fac.json

wast2json test/testsuite/float_exprs.wast
bin/testrunner float_exprs.json

wast2json test/testsuite/float_literals.wast
bin/testrunner float_literals.json

wast2json test/testsuite/float_memory.wast
bin/testrunner float_memory.json

wast2json test/testsuite/float_misc.wast
bin/testrunner float_misc.json

wast2json test/testsuite/forward.wast
bin/testrunner forward.json

wast2json test/testsuite/func_ptrs.wast
bin/testrunner func_ptrs.json

wast2json test/testsuite/func.wast
bin/testrunner func.json

wast2json test/testsuite/global.wast
bin/testrunner global.json

wast2json test/testsuite/i32.wast
bin/testrunner i32.json

wast2json test/testsuite/i64.wast
bin/testrunner i64.json

wast2json test/testsuite/if.wast
bin/testrunner if.json

wast2json test/testsuite/imports.wast
bin/testrunner imports.json

wast2json test/testsuite/inline-module.wast
bin/testrunner inline-module.json

wast2json test/testsuite/int_exprs.wast
bin/testrunner int_exprs.json

wast2json test/testsuite/int_literals.wast
bin/testrunner int_literals.json

wast2json test/testsuite/labels.wast
bin/testrunner labels.json

wast2json test/testsuite/left-to-right.wast
bin/testrunner left-to-right.json

wast2json test/testsuite/linking.wast
bin/testrunner linking.json

wast2json test/testsuite/load.wast
bin/testrunner load.json

wast2json test/testsuite/local_get.wast
bin/testrunner local_get.json

wast2json test/testsuite/local_set.wast
bin/testrunner local_set.json

wast2json test/testsuite/local_tee.wast
bin/testrunner local_tee.json

wast2json test/testsuite/loop.wast
bin/testrunner loop.json

wast2json test/testsuite/memory_grow.wast
bin/testrunner memory_grow.json

wast2json test/testsuite/memory_redundancy.wast
bin/testrunner memory_redundancy.json

wast2json test/testsuite/memory_size.wast
bin/testrunner memory_size.json

wast2json test/testsuite/memory_trap.wast
bin/testrunner memory_trap.json

wast2json test/testsuite/memory.wast
bin/testrunner memory.json

wast2json test/testsuite/names.wast
bin/testrunner names.json

wast2json test/testsuite/nop.wast
bin/testrunner nop.json

wast2json test/testsuite/return.wast
bin/testrunner return.json

wast2json test/testsuite/select.wast
bin/testrunner select.json

wast2json test/testsuite/skip-stack-guard-page.wast
bin/testrunner skip-stack-guard-page.json

wast2json test/testsuite/stack.wast
bin/testrunner stack.json

wast2json test/testsuite/start.wast
bin/testrunner start.json

wast2json test/testsuite/store.wast
bin/testrunner store.json

wast2json test/testsuite/switch.wast
bin/testrunner switch.json

wast2json test/testsuite/table.wast
bin/testrunner table.json

wast2json test/testsuite/token.wast
bin/testrunner token.json

wast2json test/testsuite/traps.wast
bin/testrunner traps.json

wast2json test/testsuite/type.wast
bin/testrunner type.json

wast2json test/testsuite/unreachable.wast
bin/testrunner unreachable.json

wast2json test/testsuite/unreached-invalid.wast
bin/testrunner unreached-invalid.json

wast2json test/testsuite/unwind.wast
bin/testrunner unwind.json

wast2json test/testsuite/utf8-custom-section-id.wast
bin/testrunner utf8-custom-section-id.json

wast2json test/testsuite/utf8-import-field.wast
bin/testrunner utf8-import-field.json

wast2json test/testsuite/utf8-import-module.wast
bin/testrunner utf8-import-module.json

wast2json test/testsuite/utf8-invalid-encoding.wast
bin/testrunner utf8-invalid-encoding.json