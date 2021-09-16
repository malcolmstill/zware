#!/bin/bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
TESTSUITE_DIR=$SCRIPT_DIR/testsuite
GENERATED_DIR=$SCRIPT_DIR/testsuite-generated

pushd $GENERATED_DIR

wast2json --version > $GENERATED_DIR/wast2json.version

wast2json $TESTSUITE_DIR/address.wast
wast2json $TESTSUITE_DIR/align.wast
wast2json $TESTSUITE_DIR/binary-leb128.wast
wast2json $TESTSUITE_DIR/binary.wast
wast2json $TESTSUITE_DIR/block.wast
wast2json $TESTSUITE_DIR/br_if.wast
wast2json $TESTSUITE_DIR/br.wast
wast2json $TESTSUITE_DIR/br_table.wast
wast2json $TESTSUITE_DIR/call_indirect.wast
wast2json $TESTSUITE_DIR/call.wast
wast2json $TESTSUITE_DIR/comments.wast
wast2json $TESTSUITE_DIR/const.wast
wast2json $TESTSUITE_DIR/conversions.wast
wast2json $TESTSUITE_DIR/custom.wast
wast2json $TESTSUITE_DIR/data.wast
wast2json $TESTSUITE_DIR/elem.wast
wast2json $TESTSUITE_DIR/endianness.wast
wast2json $TESTSUITE_DIR/exports.wast
wast2json $TESTSUITE_DIR/f32_bitwise.wast
wast2json $TESTSUITE_DIR/f32_cmp.wast
wast2json $TESTSUITE_DIR/f32.wast
wast2json $TESTSUITE_DIR/f64_bitwise.wast
wast2json $TESTSUITE_DIR/f64_cmp.wast
wast2json $TESTSUITE_DIR/f64.wast
wast2json $TESTSUITE_DIR/fac.wast
wast2json $TESTSUITE_DIR/float_exprs.wast
wast2json $TESTSUITE_DIR/float_literals.wast
wast2json $TESTSUITE_DIR/float_memory.wast
wast2json $TESTSUITE_DIR/float_misc.wast
wast2json $TESTSUITE_DIR/forward.wast
wast2json $TESTSUITE_DIR/func_ptrs.wast
wast2json $TESTSUITE_DIR/func.wast
wast2json $TESTSUITE_DIR/global.wast
wast2json $TESTSUITE_DIR/i32.wast
wast2json $TESTSUITE_DIR/i64.wast
wast2json $TESTSUITE_DIR/if.wast
wast2json $TESTSUITE_DIR/imports.wast
wast2json $TESTSUITE_DIR/inline-module.wast
wast2json $TESTSUITE_DIR/int_exprs.wast
wast2json $TESTSUITE_DIR/int_literals.wast
wast2json $TESTSUITE_DIR/labels.wast
wast2json $TESTSUITE_DIR/left-to-right.wast
wast2json $TESTSUITE_DIR/linking.wast
wast2json $TESTSUITE_DIR/load.wast
wast2json $TESTSUITE_DIR/local_get.wast
wast2json $TESTSUITE_DIR/local_set.wast
wast2json $TESTSUITE_DIR/local_tee.wast
wast2json $TESTSUITE_DIR/loop.wast
wast2json $TESTSUITE_DIR/memory_grow.wast
wast2json $TESTSUITE_DIR/memory_redundancy.wast
wast2json $TESTSUITE_DIR/memory_size.wast
wast2json $TESTSUITE_DIR/memory_trap.wast
wast2json $TESTSUITE_DIR/memory.wast
wast2json $TESTSUITE_DIR/names.wast
wast2json $TESTSUITE_DIR/nop.wast
wast2json $TESTSUITE_DIR/return.wast
wast2json $TESTSUITE_DIR/select.wast
wast2json $TESTSUITE_DIR/skip-stack-guard-page.wast
wast2json $TESTSUITE_DIR/stack.wast
wast2json $TESTSUITE_DIR/start.wast
wast2json $TESTSUITE_DIR/store.wast
wast2json $TESTSUITE_DIR/switch.wast
wast2json $TESTSUITE_DIR/table.wast
wast2json $TESTSUITE_DIR/token.wast
wast2json $TESTSUITE_DIR/traps.wast
wast2json $TESTSUITE_DIR/type.wast
wast2json $TESTSUITE_DIR/unreachable.wast
wast2json $TESTSUITE_DIR/unreached-invalid.wast
wast2json $TESTSUITE_DIR/unwind.wast
wast2json $TESTSUITE_DIR/utf8-custom-section-id.wast
wast2json $TESTSUITE_DIR/utf8-import-field.wast
wast2json $TESTSUITE_DIR/utf8-import-module.wast
wast2json $TESTSUITE_DIR/utf8-invalid-encoding.wast

popd