#!/bin/bash

function cleanup {
    rm *.json
    rm *.wat
    rm *.wasm
}

trap cleanup EXIT

wast2json test/testsuite/address.wast || exit 1
bin/testrunner address.json || exit 1

wast2json test/testsuite/align.wast || exit 1
bin/testrunner align.json || exit 1

# wast2json test/testsuite/binary-leb128.wast || exit 1
# bin/testrunner binary-leb128.json || exit 1

wast2json test/testsuite/binary.wast || exit 1
bin/testrunner binary.json || exit 1

wast2json test/testsuite/block.wast || exit 1
bin/testrunner block.json || exit 1

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

wast2json test/testsuite/comments.wast || exit 1
bin/testrunner comments.json || exit 1

wast2json test/testsuite/const.wast || exit 1
bin/testrunner const.json || exit 1

wast2json test/testsuite/conversions.wast || exit 1
bin/testrunner conversions.json || exit 1

wast2json test/testsuite/custom.wast || exit 1
bin/testrunner custom.json || exit 1

wast2json test/testsuite/data.wast || exit 1
bin/testrunner data.json || exit 1

wast2json test/testsuite/elem.wast || exit 1
bin/testrunner elem.json || exit 1

wast2json test/testsuite/endianness.wast || exit 1
bin/testrunner endianness.json || exit 1

wast2json test/testsuite/exports.wast || exit 1
bin/testrunner exports.json || exit 1

wast2json test/testsuite/f32_bitwise.wast || exit 1
bin/testrunner f32_bitwise.json || exit 1

wast2json test/testsuite/f32_cmp.wast || exit 1
bin/testrunner f32_cmp.json || exit 1

wast2json test/testsuite/f32.wast || exit 1
bin/testrunner f32.json || exit 1

wast2json test/testsuite/f64_bitwise.wast || exit 1
bin/testrunner f64_bitwise.json || exit 1

wast2json test/testsuite/f64_cmp.wast || exit 1
bin/testrunner f64_cmp.json || exit 1

wast2json test/testsuite/f64.wast || exit 1
bin/testrunner f64.json || exit 1

wast2json test/testsuite/fac.wast || exit 1
bin/testrunner fac.json || exit 1

wast2json test/testsuite/float_exprs.wast || exit 1
bin/testrunner float_exprs.json || exit 1

wast2json test/testsuite/float_literals.wast || exit 1
bin/testrunner float_literals.json || exit 1

wast2json test/testsuite/float_memory.wast || exit 1
bin/testrunner float_memory.json || exit 1

wast2json test/testsuite/float_misc.wast || exit 1
bin/testrunner float_misc.json || exit 1

wast2json test/testsuite/forward.wast || exit 1
bin/testrunner forward.json || exit 1

wast2json test/testsuite/func_ptrs.wast || exit 1
bin/testrunner func_ptrs.json || exit 1

wast2json test/testsuite/func.wast || exit 1
bin/testrunner func.json || exit 1

wast2json test/testsuite/global.wast || exit 1
bin/testrunner global.json || exit 1

wast2json test/testsuite/i32.wast || exit 1
bin/testrunner i32.json || exit 1

wast2json test/testsuite/i64.wast || exit 1
bin/testrunner i64.json || exit 1

wast2json test/testsuite/if.wast || exit 1
bin/testrunner if.json || exit 1

wast2json test/testsuite/imports.wast || exit 1
bin/testrunner imports.json || exit 1

wast2json test/testsuite/inline-module.wast || exit 1
bin/testrunner inline-module.json || exit 1

wast2json test/testsuite/int_exprs.wast || exit 1
bin/testrunner int_exprs.json || exit 1

wast2json test/testsuite/int_literals.wast || exit 1
bin/testrunner int_literals.json || exit 1

wast2json test/testsuite/labels.wast || exit 1
bin/testrunner labels.json || exit 1

wast2json test/testsuite/left-to-right.wast || exit 1
bin/testrunner left-to-right.json || exit 1

wast2json test/testsuite/linking.wast || exit 1
bin/testrunner linking.json || exit 1

wast2json test/testsuite/load.wast || exit 1
bin/testrunner load.json || exit 1

wast2json test/testsuite/local_get.wast || exit 1
bin/testrunner local_get.json || exit 1

wast2json test/testsuite/local_set.wast || exit 1
bin/testrunner local_set.json || exit 1

wast2json test/testsuite/local_tee.wast || exit 1
bin/testrunner local_tee.json || exit 1

wast2json test/testsuite/loop.wast || exit 1
bin/testrunner loop.json || exit 1

wast2json test/testsuite/memory_grow.wast || exit 1
bin/testrunner memory_grow.json || exit 1

wast2json test/testsuite/memory_redundancy.wast || exit 1
bin/testrunner memory_redundancy.json || exit 1

wast2json test/testsuite/memory_size.wast || exit 1
bin/testrunner memory_size.json || exit 1

wast2json test/testsuite/memory_trap.wast || exit 1
bin/testrunner memory_trap.json || exit 1

wast2json test/testsuite/memory.wast || exit 1
bin/testrunner memory.json || exit 1

wast2json test/testsuite/names.wast || exit 1
bin/testrunner names.json || exit 1

wast2json test/testsuite/nop.wast || exit 1
bin/testrunner nop.json || exit 1

wast2json test/testsuite/return.wast || exit 1
bin/testrunner return.json || exit 1

wast2json test/testsuite/select.wast || exit 1
bin/testrunner select.json || exit 1

wast2json test/testsuite/skip-stack-guard-page.wast || exit 1
bin/testrunner skip-stack-guard-page.json || exit 1

wast2json test/testsuite/stack.wast || exit 1
bin/testrunner stack.json || exit 1

wast2json test/testsuite/start.wast || exit 1
bin/testrunner start.json || exit 1

wast2json test/testsuite/store.wast || exit 1
bin/testrunner store.json || exit 1

wast2json test/testsuite/switch.wast || exit 1
bin/testrunner switch.json || exit 1

wast2json test/testsuite/table.wast || exit 1
bin/testrunner table.json || exit 1

wast2json test/testsuite/token.wast || exit 1
bin/testrunner token.json || exit 1

wast2json test/testsuite/traps.wast || exit 1
bin/testrunner traps.json || exit 1

wast2json test/testsuite/type.wast || exit 1
bin/testrunner type.json || exit 1

wast2json test/testsuite/unreachable.wast || exit 1
bin/testrunner unreachable.json || exit 1

wast2json test/testsuite/unreached-invalid.wast || exit 1
bin/testrunner unreached-invalid.json || exit 1

wast2json test/testsuite/unwind.wast || exit 1
bin/testrunner unwind.json || exit 1

wast2json test/testsuite/utf8-custom-section-id.wast || exit 1
bin/testrunner utf8-custom-section-id.json || exit 1

wast2json test/testsuite/utf8-import-field.wast || exit 1
bin/testrunner utf8-import-field.json || exit 1

wast2json test/testsuite/utf8-import-module.wast || exit 1
bin/testrunner utf8-import-module.json || exit 1

wast2json test/testsuite/utf8-invalid-encoding.wast || exit 1
bin/testrunner utf8-invalid-encoding.json || exit 1