#!/bin/bash
set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
BIN_DIR=$SCRIPT_DIR/../bin
GENERATED_DIR=$SCRIPT_DIR/testsuite-generated

pushd $GENERATED_DIR

$BIN_DIR/testrunner address.json
$BIN_DIR/testrunner align.json
# $BIN_DIR/testrunner binary-leb128.json
$BIN_DIR/testrunner binary.json
$BIN_DIR/testrunner block.json
$BIN_DIR/testrunner br_if.json
$BIN_DIR/testrunner br.json
$BIN_DIR/testrunner br_table.json
$BIN_DIR/testrunner call_indirect.json
$BIN_DIR/testrunner call.json
$BIN_DIR/testrunner comments.json
$BIN_DIR/testrunner const.json
$BIN_DIR/testrunner conversions.json
$BIN_DIR/testrunner custom.json
$BIN_DIR/testrunner data.json
$BIN_DIR/testrunner elem.json
$BIN_DIR/testrunner endianness.json
$BIN_DIR/testrunner exports.json
$BIN_DIR/testrunner f32_bitwise.json
$BIN_DIR/testrunner f32_cmp.json
$BIN_DIR/testrunner f32.json
$BIN_DIR/testrunner f64_bitwise.json
$BIN_DIR/testrunner f64_cmp.json
$BIN_DIR/testrunner f64.json
$BIN_DIR/testrunner fac.json
$BIN_DIR/testrunner float_exprs.json
$BIN_DIR/testrunner float_literals.json
$BIN_DIR/testrunner float_memory.json
$BIN_DIR/testrunner float_misc.json
$BIN_DIR/testrunner forward.json
$BIN_DIR/testrunner func_ptrs.json
$BIN_DIR/testrunner func.json
$BIN_DIR/testrunner global.json
$BIN_DIR/testrunner i32.json
$BIN_DIR/testrunner i64.json
$BIN_DIR/testrunner if.json
$BIN_DIR/testrunner imports.json
$BIN_DIR/testrunner inline-module.json
$BIN_DIR/testrunner int_exprs.json
$BIN_DIR/testrunner int_literals.json
$BIN_DIR/testrunner labels.json
$BIN_DIR/testrunner left-to-right.json
$BIN_DIR/testrunner linking.json
$BIN_DIR/testrunner load.json
$BIN_DIR/testrunner local_get.json
$BIN_DIR/testrunner local_set.json
$BIN_DIR/testrunner local_tee.json
$BIN_DIR/testrunner loop.json
$BIN_DIR/testrunner memory_grow.json
$BIN_DIR/testrunner memory_redundancy.json
$BIN_DIR/testrunner memory_size.json
$BIN_DIR/testrunner memory_trap.json
$BIN_DIR/testrunner memory.json
$BIN_DIR/testrunner names.json
$BIN_DIR/testrunner nop.json
$BIN_DIR/testrunner return.json
$BIN_DIR/testrunner select.json
$BIN_DIR/testrunner skip-stack-guard-page.json
$BIN_DIR/testrunner stack.json
$BIN_DIR/testrunner start.json
$BIN_DIR/testrunner store.json
$BIN_DIR/testrunner switch.json
$BIN_DIR/testrunner table.json
$BIN_DIR/testrunner token.json
$BIN_DIR/testrunner traps.json
$BIN_DIR/testrunner type.json
$BIN_DIR/testrunner unreachable.json
$BIN_DIR/testrunner unreached-invalid.json
$BIN_DIR/testrunner unwind.json
$BIN_DIR/testrunner utf8-custom-section-id.json
$BIN_DIR/testrunner utf8-import-field.json
$BIN_DIR/testrunner utf8-import-module.json
$BIN_DIR/testrunner utf8-invalid-encoding.json

popd