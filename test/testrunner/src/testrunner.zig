const std = @import("std");
const fs = std.fs;
const fmt = std.fmt;
const mem = std.mem;
const math = std.math;
const process = std.process;
const json = std.json;
const zware = @import("zware");
const ValType = zware.ValType;
const Module = zware.Module;
const Instance = zware.Instance;
const Store = zware.Store;
const Memory = zware.Memory;
const Function = zware.Function;
const Global = zware.Global;
const VirtualMachine = zware.VirtualMachine;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const StringHashMap = std.hash_map.StringHashMap;
const ArrayList = std.ArrayList;
const WasmError = zware.WasmError;

// testrunner
//
// testrunner is an a program that consumes the zware library
// and runs the WebAssembly testsuite.
//
// This allows us to separate out the library code from these
// tests but still include the testsuite as part of, say, Github
// Actions.
//
// See https://github.com/WebAssembly/spec/blob/master/interpreter/README.md#s-expression-syntax
// for information on the format of the .wast files.

var gpa = GeneralPurposeAllocator(.{}){};

fn print(_: *VirtualMachine) WasmError!void {
    std.debug.print("print\n", .{});
}

fn print_i32(vm: *VirtualMachine) WasmError!void {
    const value = vm.popOperand(i32);
    std.debug.print("print_i32: {}\n", .{value});
}

fn print_i64(vm: *VirtualMachine) WasmError!void {
    const value = vm.popOperand(i64);
    std.debug.print("print_i64: {}\n", .{value});
}

fn print_f32(vm: *VirtualMachine) WasmError!void {
    const value = vm.popOperand(f32);
    std.debug.print("print_f32: {}\n", .{value});
}

fn print_f64(vm: *VirtualMachine) WasmError!void {
    const value = vm.popOperand(f64);
    std.debug.print("print_f64: {}\n", .{value});
}

fn print_i32_f32(vm: *VirtualMachine) WasmError!void {
    const value_f32 = vm.popOperand(f32);
    const value_i32 = vm.popOperand(i32);
    std.debug.print("print_i32_f32: {}, {}\n", .{ value_i32, value_f32 });
}

fn print_f64_f64(vm: *VirtualMachine) WasmError!void {
    const value_f64_2 = vm.popOperand(f64);
    const value_f64_1 = vm.popOperand(f64);
    std.debug.print("print_f64_f64: {}, {}\n", .{ value_f64_1, value_f64_2 });
}

pub fn main() anyerror!void {
    defer _ = gpa.deinit();

    // 1. Get .json file from command line
    var args = try process.argsWithAllocator(gpa.allocator());
    defer args.deinit();
    _ = args.skip();
    const filename = args.next() orelse return error.NoFilename;
    std.log.info("testing: {s}", .{filename});

    var arena = ArenaAllocator.init(gpa.allocator());
    defer _ = arena.deinit();

    const alloc = arena.allocator();

    // 2. Parse json and find .wasm file
    const json_string = try fs.cwd().readFileAlloc(alloc, filename, 0xFFFFFFF);

    const dynamic_tree = try std.json.parseFromSliceLeaky(std.json.Value, alloc, json_string, .{});
    const r = try std.json.parseFromValueLeaky(Wast, alloc, dynamic_tree, .{});

    // 2.a. Find the wasm file
    var wasm_filename: []const u8 = undefined;
    var program: []const u8 = undefined;
    var module: Module = undefined;

    // Initialise a store
    var store: Store = Store.init(alloc);
    const spectest = "spectest";

    // Init spectest memory
    try store.exposeMemory(spectest, "memory", 1, 2);

    // Init spectest table
    try store.exposeTable(spectest, "table", .FuncRef, 10, 20);

    // Expose spectest globals
    try store.exposeGlobal(spectest, "global_i32", 666, .I32, .Immutable);
    try store.exposeGlobal(spectest, "global_i64", 666, .I64, .Immutable);
    try store.exposeGlobal(spectest, "global_f32", 666, .F32, .Immutable);
    try store.exposeGlobal(spectest, "global_f64", 666, .F64, .Immutable);

    // Expose host functions
    try store.exposeHostFunction(spectest, "print", print, &[_]ValType{}, &[_]ValType{});
    try store.exposeHostFunction(spectest, "print_i32", print_i32, &[_]ValType{.I32}, &[_]ValType{});
    try store.exposeHostFunction(spectest, "print_i64", print_i64, &[_]ValType{.I64}, &[_]ValType{});
    try store.exposeHostFunction(spectest, "print_f32", print_f32, &[_]ValType{.F32}, &[_]ValType{});
    try store.exposeHostFunction(spectest, "print_f64", print_f64, &[_]ValType{.F64}, &[_]ValType{});
    try store.exposeHostFunction(spectest, "print_i32_f32", print_i32_f32, &[_]ValType{.I32, .F32}, &[_]ValType{});
    try store.exposeHostFunction(spectest, "print_f64_f64", print_f64_f64, &[_]ValType{.F64, .F64}, &[_]ValType{});

    var current_instance: *Instance = undefined;
    var registered_names = StringHashMap(*Instance).init(alloc);

    for (r.commands) |command| {
        switch (command) {
            .module => {
                wasm_filename = command.module.filename;

                std.debug.print("(module): {s}:{} ({s})\n", .{ r.source_filename, command.module.line, wasm_filename });
                program = try fs.cwd().readFileAlloc(alloc, wasm_filename, 0xFFFFFFF);

                errdefer {
                    std.debug.print("(module): {s} at {}:{s}\n", .{ r.source_filename, command.module.line, wasm_filename });
                }

                // 4. Initialise our module
                module = Module.init(alloc, program);
                try module.decode();

                current_instance = try alloc.create(Instance);
                current_instance.* = Instance.init(alloc, &store, module);
                try current_instance.instantiate();

                if (command.module.name) |name| {
                    try registered_names.put(name, current_instance);
                }
            },
            .assert_return => {
                const action = command.assert_return.action;
                const expected = command.assert_return.expected;
                switch (action) {
                    .invoke => {
                        const field = action.invoke.field;
                        std.debug.print("(return): {s}:{}\n", .{ r.source_filename, command.assert_return.line });

                        var instance = current_instance;
                        if (command.assert_return.action.invoke.module) |name| {
                            if (registered_names.get(name)) |registered_instance| {
                                instance = registered_instance;
                            }
                        }

                        // Allocate input parameters and output results
                        var in = try alloc.alloc(u64, action.invoke.args.len);
                        var out = try alloc.alloc(u64, expected.len);

                        // Initialise input parameters
                        for (action.invoke.args, 0..) |value, i| {
                            if (mem.eql(u8, value.value, "null")) {
                                in[i] = VirtualMachine.REF_NULL;
                            } else {
                                const arg = try fmt.parseInt(u64, value.value, 10);
                                in[i] = arg;
                            }
                        }

                        // Invoke the function
                        instance.invoke(field, in, out, .{}) catch |err| {
                            std.debug.print("(result) invoke = {s}\n", .{field});
                            std.debug.print("Testsuite failure: {s} at {s}:{}\n", .{ field, r.source_filename, command.assert_return.line });
                            return err;
                        };

                        for (expected, 0..) |result, i| {
                            const valtype = try valueTypeFromString(result.type);
                            switch (valtype) {
                                .I32, .I64, .F32, .F64 => {
                                    if (mem.startsWith(u8, result.value, "nan:")) {
                                        if (valtype == .F32 and math.isNan(@as(f32, @bitCast(@as(u32, @truncate(out[i])))))) {
                                            continue;
                                        }
                                        if (valtype == .F64 and math.isNan(@as(f64, @bitCast(out[i])))) {
                                            continue;
                                        }

                                        std.debug.print("(result) invoke = {s}\n", .{field});
                                        std.debug.print("Testsuite failure: {s} at {s}:{}\n", .{ field, r.source_filename, command.assert_return.line });
                                        std.debug.print("result[{}], expected: {s}, result: {} ({x})\n", .{ i, "nan", out[i], out[i] });
                                        return error.TestsuiteTestFailureTrapResult;
                                    }

                                    const result_value = try fmt.parseInt(u64, result.value, 10);
                                    // Otherwise
                                    errdefer {
                                        std.debug.print("(result) invoke = {s}\n", .{field});
                                        std.debug.print("Testsuite failure: {s} at {s}:{}\n", .{ field, r.source_filename, command.assert_return.line });
                                        std.debug.print("result[{}], expected: {s} ({x}), result: {} ({x})\n", .{ i, result.value, result_value, out[i], out[i] });
                                    }
                                    if (result_value != out[expected.len - i - 1]) {
                                        return error.TestsuiteTestFailureTrapResult;
                                    }
                                },
                                .FuncRef, .ExternRef => {
                                    if (mem.eql(u8, result.value, "null")) {
                                        if (out[expected.len - i - 1] != VirtualMachine.REF_NULL) {
                                            return error.TestsuiteTestFailureTrapResult;
                                        }
                                    } else {
                                        const result_value = try fmt.parseInt(u64, result.value, 10);
                                        // Otherwise
                                        errdefer {
                                            std.debug.print("(result) invoke = {s}\n", .{field});
                                            std.debug.print("Testsuite failure: {s} at {s}:{}\n", .{ field, r.source_filename, command.assert_return.line });
                                            std.debug.print("result[{}], expected: {s} ({x}), result: {} ({x})\n", .{ i, result.value, result_value, out[i], out[i] });
                                        }
                                        if (result_value != out[expected.len - i - 1]) {
                                            return error.TestsuiteTestFailureTrapResult;
                                        }
                                    }
                                },
                                .V128 => return error.SIMDNotImplemented,
                            }
                        }
                    },
                    .get => {
                        const field = action.get.field;
                        std.debug.print("(return): get {s}:{} ({s})\n", .{ r.source_filename, command.assert_return.line, wasm_filename });
                        std.debug.print("(result) get \"{s}\"\n", .{field});
                        if (action.get.module) |m| {
                            const registered_inst = registered_names.get(m) orelse return error.NotRegistered;

                            for (registered_inst.module.exports.list.items) |exprt| {
                                if (mem.eql(u8, exprt.name, field)) {
                                    const global = try registered_inst.getGlobal(exprt.index);

                                    for (expected, 0..) |result, j| {
                                        if (j > 0) return error.ExpectedOneResult;
                                        const result_value = try fmt.parseInt(u64, result.value, 10);

                                        if (global.value != result_value) {
                                            return error.GlobalUnexpectedValue;
                                        }
                                    }
                                }
                            }
                        } else {
                            for (current_instance.module.exports.list.items, 0..) |exprt, i| {
                                if (mem.eql(u8, exprt.name, field)) {
                                    const global = try current_instance.getGlobal(i);

                                    for (expected, 0..) |result, j| {
                                        if (j > 0) return error.ExpectedOneResult;
                                        const result_value = try fmt.parseInt(u64, result.value, 10);
                                        if (global.value != result_value) {
                                            return error.GlobalUnexpectedValue;
                                        }
                                    }
                                }
                            }
                        }
                    },
                }
            },
            .assert_trap => {
                const action = command.assert_trap.action;
                const expected = command.assert_trap.expected;
                const trap = command.assert_trap.text;

                switch (action) {
                    .invoke => {
                        const field = action.invoke.field;
                        std.debug.print("(trap): {s}:{}\n", .{ r.source_filename, command.assert_trap.line });

                        errdefer {
                            std.debug.print("(trap) invoke = {s} at {s}:{}\n", .{ field, r.source_filename, command.assert_trap.line });
                        }

                        var instance = current_instance;
                        if (command.assert_trap.action.invoke.module) |name| {
                            if (registered_names.get(name)) |registered_instance| {
                                instance = registered_instance;
                            }
                        }

                        // Allocate input parameters and output results
                        var in = try alloc.alloc(u64, action.invoke.args.len);
                        var out = try alloc.alloc(u64, expected.len);

                        // Initialise input parameters
                        for (action.invoke.args, 0..) |value, i| {
                            if (mem.eql(u8, value.value, "null")) {
                                in[i] = VirtualMachine.REF_NULL;
                            } else {
                                const arg = try fmt.parseInt(u64, value.value, 10);
                                in[i] = arg;
                            }
                        }

                        if (instance.invoke(field, in, out, .{})) |_| {
                            return error.TestsuiteExpectedTrap;
                        } else |err| {
                            // Test the result
                            if (mem.eql(u8, trap, "integer divide by zero")) {
                                switch (err) {
                                    error.DivisionByZero => continue,
                                    else => return error.TestsuiteExpectedDivideByZero,
                                }
                            }

                            if (mem.eql(u8, trap, "integer overflow")) {
                                switch (err) {
                                    error.Overflow => continue,
                                    else => return error.TestsuiteExpectedOverflow,
                                }
                            }

                            if (mem.eql(u8, trap, "invalid conversion to integer")) {
                                switch (err) {
                                    error.InvalidConversion => continue,
                                    else => return error.TestsuiteExpectedInvalidConversion,
                                }
                            }

                            if (mem.eql(u8, trap, "out of bounds memory access")) {
                                switch (err) {
                                    error.OutOfBoundsMemoryAccess => continue,
                                    else => return error.TestsuiteExpectedOutOfBoundsMemoryAccess,
                                }
                            }

                            if (mem.eql(u8, trap, "indirect call type mismatch")) {
                                switch (err) {
                                    error.MismatchedSignatures => continue,
                                    else => return error.TestsuiteExpectedIndirectCallTypeMismatch,
                                }
                            }

                            if (mem.eql(u8, trap, "undefined element") or mem.eql(u8, trap, "uninitialized element") or mem.eql(u8, trap, "uninitialized element 2")) {
                                switch (err) {
                                    error.UndefinedElement => continue,
                                    error.OutOfBoundsMemoryAccess => continue,
                                    else => {
                                        std.debug.print("Unexpected error: {}\n", .{err});
                                        return error.TestsuiteExpectedUndefinedElement;
                                    },
                                }
                            }

                            if (mem.eql(u8, trap, "uninitialized") or mem.eql(u8, trap, "undefined") or mem.eql(u8, trap, "indirect call")) {
                                switch (err) {
                                    error.UndefinedElement => continue,
                                    error.OutOfBoundsMemoryAccess => continue,
                                    error.MismatchedSignatures => continue,
                                    else => {
                                        std.debug.print("Unexpected error: {}\n", .{err});
                                        return error.TestsuiteExpectedUnitialized;
                                    },
                                }
                            }

                            if (mem.eql(u8, trap, "out of bounds table access")) {
                                switch (err) {
                                    error.Trap => continue,
                                    error.OutOfBoundsMemoryAccess => continue,
                                    else => {
                                        std.debug.print("Unexpected error: {}\n", .{err});
                                        return error.TestsuiteExpectedUnreachable;
                                    },
                                }
                            }

                            if (mem.eql(u8, trap, "unreachable")) {
                                switch (err) {
                                    error.TrapUnreachable => continue,
                                    else => {
                                        std.debug.print("Unexpected error: {}\n", .{err});
                                        return error.TestsuiteExpectedUnreachable;
                                    },
                                }
                            }
                        }
                    },
                    .get => {
                        std.debug.print("(trap) get\n", .{});
                        return error.TrapGetNotImplemented;
                    },
                }

                return error.ExpectedTrapDidntOccur;
            },
            .assert_invalid => {
                wasm_filename = command.assert_invalid.filename;
                std.debug.print("(invalid): {s}:{} ({s})\n", .{ r.source_filename, command.assert_invalid.line, wasm_filename });

                program = try fs.cwd().readFileAlloc(alloc, wasm_filename, 0xFFFFFFF);
                module = Module.init(alloc, program);

                errdefer {
                    std.debug.print("ERROR (invalid): {s}:{}\n", .{ r.source_filename, command.assert_invalid.line });
                }

                if (module.decode()) |_| {
                    return error.TestsuiteExpectedInvalid;
                } else |err| switch (err) {
                    error.InvalidAlignment => continue,
                    error.ValidatorPopOperandError => continue,
                    error.MismatchedTypes => continue,
                    error.ValidatorPopControlFrameControlStackEmpty => continue,
                    error.ValidatorPopControlFrameMismatchedSizes => continue,
                    error.ControlStackEmpty => continue,
                    error.ValidatorAttemptToMutateImmutableGlobal => continue,
                    error.ValidatorConstantExpressionRequired => continue,
                    error.ValidatorInvalidTypeIndex => continue,
                    error.ValidatorMultipleMemories => continue,
                    error.LocalGetIndexOutOfBounds => continue,
                    error.LocalSetIndexOutOfBounds => continue,
                    error.LocalTeeIndexOutOfBounds => continue,
                    error.ValidatorDataMemoryReferenceInvalid => continue,
                    error.ValidatorUnknownMemory => continue,
                    error.ValidatorMemoryMinGreaterThanMax => continue,
                    error.ValidatorMemoryMinTooLarge => continue,
                    error.ValidatorMemoryMaxTooLarge => continue,
                    error.ValidatorSelect => continue,
                    error.ValidateBrInvalidLabel => continue,
                    error.ValidateBrIfInvalidLabel => continue,
                    error.ValidateBrTableInvalidLabel => continue,
                    error.ValidateBrTableInvalidLabelWrongArity => continue,
                    error.ValidateBrTableInvalidLabelN => continue,
                    error.ValidatorCallIndirectNoTable => continue,
                    error.ValidatorElemUnknownTable => continue,
                    error.ValidatorTableMinGreaterThanMax => continue,
                    error.ValidatorExportUnknownFunction => continue,
                    error.ValidatorExportUnknownTable => continue,
                    error.ValidatorExportUnknownMemory => continue,
                    error.ValidatorExportUnknownGlobal => continue,
                    error.ValidatorDuplicateExportName => continue,
                    error.ValidatorInvalidIndex => continue,
                    error.ValidatorNotStartFunctionType => continue,
                    error.ValidatorElemUnknownFunctionIndex => continue,
                    error.ValidatorElseBranchExpected => continue,
                    error.ValidatorMutableGlobalInConstantExpr => continue,
                    error.OnlyOneSelectTTypeSupported => continue,
                    error.ExpectingBothNum => continue,
                    error.InstructionRequiresDataCountSection => continue,
                    error.InvalidDataIndex => continue,
                    error.ValidatorInvalidFunction => continue,
                    error.ValidatorUnreferencedFunction => continue,
                    error.ValidatorInvalidElementIndex => continue,
                    else => {
                        std.debug.print("Unexpected error: {}\n", .{err});
                        return error.TestsuiteExpectedInvalidUnexpectedError;
                    },
                }
            },
            .assert_malformed => {
                if (mem.endsWith(u8, command.assert_malformed.filename, ".wat")) continue;
                wasm_filename = command.assert_malformed.filename;
                std.debug.print("(malformed): {s}:{} ({s})\n", .{ r.source_filename, command.assert_malformed.line, wasm_filename });
                program = try fs.cwd().readFileAlloc(alloc, wasm_filename, 0xFFFFFFF);
                module = Module.init(alloc, program);

                const trap = command.assert_malformed.text;

                errdefer {
                    std.debug.print("ERROR (malformed): {s}:{}\n", .{ r.source_filename, command.assert_malformed.line });
                }

                if (module.decode()) |_| {
                    return error.TestsuiteMalformedExpectedButDecodedOk;
                } else |err| {
                    if (mem.eql(u8, trap, "unexpected end") or mem.eql(u8, trap, "length out of bounds")) {
                        switch (err) {
                            error.Overflow => continue,
                            error.FunctionCodeSectionsInconsistent => continue,
                            error.EndOfStream => continue,
                            error.ElementsCountMismatch => continue,
                            error.ValidatorInvalidIndex => continue,
                            error.CouldntFindEnd => continue, // test/testsuite/binary.wast:910 bad br_table means we don't find end
                            else => {},
                        }
                    }

                    if (mem.eql(u8, trap, "illegal opcode")) {
                        switch (err) {
                            error.IllegalOpcode => continue,
                            else => {},
                        }
                    }

                    if (mem.eql(u8, trap, "malformed reference type")) {
                        switch (err) {
                            error.InvalidValue => continue,
                            else => {},
                        }
                    }

                    if (mem.eql(u8, trap, "magic header not detected")) {
                        switch (err) {
                            error.MagicNumberNotFound => continue,
                            else => {},
                        }
                    }

                    if (mem.eql(u8, trap, "unknown binary version")) {
                        switch (err) {
                            error.UnknownBinaryVersion => continue,
                            else => {},
                        }
                    }

                    if (mem.eql(u8, trap, "malformed section id")) {
                        switch (err) {
                            error.InvalidValue => continue,
                            else => {},
                        }
                    }

                    if (mem.eql(u8, trap, "data count and data section have inconsistent lengths")) {
                        switch (err) {
                            error.DataCountSectionDataSectionCountMismatch => continue,
                            else => {},
                        }
                    }

                    if (mem.eql(u8, trap, "data count section required")) {
                        switch (err) {
                            error.InstructionRequiresDataCountSection => continue,
                            else => {},
                        }
                    }

                    if (mem.eql(u8, trap, "integer representation too long")) {
                        switch (err) {
                            error.InvalidValue => continue,
                            error.ExpectedFuncTypeTag => continue,
                            error.Overflow => continue,
                            else => {},
                        }
                    }

                    if (mem.eql(u8, trap, "zero byte expected")) {
                        switch (err) {
                            error.MalformedMemoryReserved => continue,
                            else => {},
                        }
                    }

                    if (mem.eql(u8, trap, "too many locals")) {
                        switch (err) {
                            error.TooManyLocals => continue,
                            else => {},
                        }
                    }

                    if (mem.eql(u8, trap, "function and code section have inconsistent lengths")) {
                        switch (err) {
                            error.FunctionCodeSectionsInconsistent => continue,
                            else => {},
                        }
                    }

                    if (mem.eql(u8, trap, "unexpected end of section or function") or mem.eql(u8, trap, "section size mismatch") or mem.eql(u8, trap, "END opcode expected")) {
                        switch (err) {
                            error.EndOfStream => continue,
                            error.TypeCountMismatch => continue,
                            error.ImportsCountMismatch => continue,
                            error.TablesCountMismatch => continue,
                            error.MemoriesCountMismatch => continue,
                            error.GlobalsCountMismatch => continue,
                            error.ElementsCountMismatch => continue,
                            error.FunctionsCountMismatch => continue,
                            error.CodesCountMismatch => continue,
                            error.DatasCountMismatch => continue,
                            error.InvalidValue => continue,
                            error.MalformedSectionMismatchedSize => continue,
                            error.ContinuationStackUnderflow => continue,
                            error.CouldntFindEnd => continue,
                            else => {},
                        }
                    }

                    if (mem.eql(u8, trap, "malformed import kind")) {
                        switch (err) {
                            error.InvalidValue => continue,
                            else => {},
                        }
                    }

                    if (mem.eql(u8, trap, "integer too large")) {
                        switch (err) {
                            error.Overflow => continue,
                            error.InvalidValue => continue, // test/testsuite/binary.wast:601 I think the test is wrong
                            else => {},
                        }
                    }

                    if (mem.eql(u8, trap, "unexpected content after last section")) {
                        switch (err) {
                            error.MultipleStartSections => continue,
                            else => {},
                        }
                    }

                    if (mem.eql(u8, trap, "malformed mutability")) {
                        switch (err) {
                            error.InvalidValue => continue,
                            else => {},
                        }
                    }

                    if (mem.eql(u8, trap, "malformed UTF-8 encoding")) {
                        switch (err) {
                            error.NameNotUTF8 => continue,
                            else => {},
                        }
                    }

                    std.debug.print("Trap: {s}, got error error: {}\n", .{ trap, err });
                    return error.ErrorDoesNotMatchTrap;
                }
            },
            .action => {
                const action = command.action.action;
                const expected = command.action.expected;
                switch (action) {
                    .invoke => {
                        const field = action.invoke.field;
                        std.debug.print("(return): {s}:{}\n", .{ r.source_filename, command.action.line });

                        // Allocate input parameters and output results
                        var in = try alloc.alloc(u64, action.invoke.args.len);
                        var out = try alloc.alloc(u64, expected.len);

                        // Initialise input parameters
                        for (action.invoke.args, 0..) |value, i| {
                            const arg = try fmt.parseInt(u64, value.value, 10);
                            in[i] = arg;
                        }

                        // Invoke the function
                        current_instance.invoke(field, in, out, .{}) catch |err| {
                            std.debug.print("(result) invoke = {s}\n", .{field});
                            std.debug.print("Testsuite failure: {s} at {s}:{}\n", .{ field, r.source_filename, command.action.line });
                            return err;
                        };
                    },
                    .get => {
                        std.debug.print("(action) get\n", .{});
                        return error.ActionGetNotImplemented;
                    },
                }
            },
            .assert_unlinkable => {
                wasm_filename = command.assert_unlinkable.filename;
                std.debug.print("(unlinkable): {s}:{} ({s})\n", .{ r.source_filename, command.assert_unlinkable.line, wasm_filename });
                program = try fs.cwd().readFileAlloc(alloc, wasm_filename, 0xFFFFFFF);

                module = Module.init(alloc, program);
                try module.decode();

                var instance = try alloc.create(Instance);
                instance.* = Instance.init(alloc, &store, module);
                if (instance.instantiate()) |_| {
                    return error.ExpectedUnlinkable;
                } else |err| switch (err) {
                    error.LimitMismatch => continue,
                    error.ImportedTableRefTypeMismatch => continue,
                    error.ImportNotFound => continue,
                    error.MismatchedSignatures => continue,
                    error.Overflow => continue,
                    error.OutOfBoundsMemoryAccess => continue,
                    error.MismatchedMutability => continue,
                    error.MismatchedGlobalType => continue,
                    else => {
                        std.debug.print("(unlinkable) Unexpected error: {}\n", .{err});
                        return error.UnexpectedError;
                    },
                }
            },
            .assert_uninstantiable => {
                wasm_filename = command.assert_uninstantiable.filename;
                std.debug.print("(uninstantiable): {s}:{} ({s})\n", .{ r.source_filename, command.assert_uninstantiable.line, wasm_filename });
                program = try fs.cwd().readFileAlloc(alloc, wasm_filename, 0xFFFFFFF);

                module = Module.init(alloc, program);
                try module.decode();

                var instance = try alloc.create(Instance);
                instance.* = Instance.init(alloc, &store, module);
                if (instance.instantiate()) |_| {
                    return error.ExpectedUninstantiable;
                } else |err| switch (err) {
                    error.TrapUnreachable => continue,
                    error.OutOfBoundsMemoryAccess => continue, // Why did this work before? data.wast 174
                    else => {
                        std.debug.print("(uninstantiable) Unexpected error: {}\n", .{err});
                        return error.UnexpectedError;
                    },
                }
            },
            .register => {
                std.debug.print("(register): {s}:{}\n", .{ r.source_filename, command.register.line });
                if (command.register.name) |name| {
                    const registered_inst = registered_names.get(name) orelse return error.NotRegistered;

                    for (registered_inst.module.exports.list.items) |exprt| {
                        switch (exprt.tag) {
                            .Table => {
                                const handle = registered_inst.tableaddrs.items[exprt.index];
                                try store.@"export"(command.register.as, exprt.name, .Table, handle);
                            },
                            .Func => {
                                const handle = registered_inst.funcaddrs.items[exprt.index];
                                try store.@"export"(command.register.as, exprt.name, .Func, handle);
                            },
                            .Global => {
                                const handle = registered_inst.globaladdrs.items[exprt.index];
                                try store.@"export"(command.register.as, exprt.name, .Global, handle);
                            },
                            .Mem => {
                                const handle = registered_inst.memaddrs.items[exprt.index];
                                try store.@"export"(command.register.as, exprt.name, .Mem, handle);
                            },
                        }
                    }
                } else {
                    for (current_instance.module.exports.list.items) |exprt| {
                        switch (exprt.tag) {
                            .Table => {
                                const handle = current_instance.tableaddrs.items[exprt.index];
                                try store.@"export"(command.register.as, exprt.name, .Table, handle);
                            },
                            .Func => {
                                const handle = current_instance.funcaddrs.items[exprt.index];
                                try store.@"export"(command.register.as, exprt.name, .Func, handle);
                            },
                            .Global => {
                                const handle = current_instance.globaladdrs.items[exprt.index];
                                try store.@"export"(command.register.as, exprt.name, .Global, handle);
                            },
                            .Mem => {
                                const handle = current_instance.memaddrs.items[exprt.index];
                                try store.@"export"(command.register.as, exprt.name, .Mem, handle);
                            },
                        }
                    }
                }
            },
            else => continue,
        }
    }
}

fn valueTypeFromString(s: []const u8) !ValType {
    if (mem.eql(u8, s, "i32")) return ValType.I32;
    if (mem.eql(u8, s, "i64")) return ValType.I64;
    if (mem.eql(u8, s, "f32")) return ValType.F32;
    if (mem.eql(u8, s, "f64")) return ValType.F64;
    if (mem.eql(u8, s, "funcref")) return ValType.FuncRef;
    if (mem.eql(u8, s, "externref")) return ValType.ExternRef;
    return error.UnknownType;
}

const Wast = struct {
    source_filename: []const u8,
    commands: []const Command,
};

const Command = union(enum) {
    module: CommandModule,
    assert_return: CommandAssertReturn,
    assert_trap: CommandAssertTrap,
    assert_malformed: CommandAssertMalformed,
    assert_invalid: CommandAssertInvalid,
    assert_exhaustion: CommandAssertExhaustion,
    assert_unlinkable: CommandAssertUnlinkable,
    assert_uninstantiable: CommandAssertUninstantiable,
    action: CommandAction,
    register: CommandRegister,

    pub fn jsonParseFromValue(allocator: mem.Allocator, source: std.json.Value, options: std.json.ParseOptions) !@This() {
        if (source != .object) return error.UnexpectedToken;

        const type_value = source.object.get("type") orelse return error.UnexpectedToken;
        if (type_value != .string) return error.UnexpectedToken;

        const type_str = type_value.string;
        var child_options = options;

        child_options.ignore_unknown_fields = true;

        if (std.mem.eql(u8, type_str, "module")) return .{ .module = try std.json.parseFromValueLeaky(CommandModule, allocator, source, child_options) };
        if (std.mem.eql(u8, type_str, "assert_return")) return .{ .assert_return = try std.json.parseFromValueLeaky(CommandAssertReturn, allocator, source, child_options) };
        if (std.mem.eql(u8, type_str, "assert_trap")) return .{ .assert_trap = try std.json.parseFromValueLeaky(CommandAssertTrap, allocator, source, child_options) };
        if (std.mem.eql(u8, type_str, "assert_malformed")) return .{ .assert_malformed = try std.json.parseFromValueLeaky(CommandAssertMalformed, allocator, source, child_options) };
        if (std.mem.eql(u8, type_str, "assert_invalid")) return .{ .assert_invalid = try std.json.parseFromValueLeaky(CommandAssertInvalid, allocator, source, child_options) };
        if (std.mem.eql(u8, type_str, "assert_exhaustion")) return .{ .assert_exhaustion = try std.json.parseFromValueLeaky(CommandAssertExhaustion, allocator, source, child_options) };
        if (std.mem.eql(u8, type_str, "assert_unlinkable")) return .{ .assert_unlinkable = try std.json.parseFromValueLeaky(CommandAssertUnlinkable, allocator, source, child_options) };
        if (std.mem.eql(u8, type_str, "assert_uninstantiable")) return .{ .assert_uninstantiable = try std.json.parseFromValueLeaky(CommandAssertUninstantiable, allocator, source, child_options) };
        if (std.mem.eql(u8, type_str, "action")) return .{ .action = try std.json.parseFromValueLeaky(CommandAction, allocator, source, child_options) };
        if (std.mem.eql(u8, type_str, "register")) return .{ .register = try std.json.parseFromValueLeaky(CommandRegister, allocator, source, child_options) };

        return error.UnexpectedToken;
    }
};

const CommandModule = struct {
    // type: "module"
    line: usize,
    name: ?[]const u8 = null,
    filename: []const u8,
};

const CommandAssertReturn = struct {
    // type: "assert_return"
    line: usize,
    action: Action,
    expected: []const Value,
};

const CommandAssertTrap = struct {
    // type: "assert_trap"
    line: usize,
    action: Action,
    text: []const u8,
    expected: []const ValueTrap,
};

const CommandAssertMalformed = struct {
    // type: "assert_malformed"
    line: usize,
    filename: []const u8,
    text: []const u8,
    module_type: []const u8,
};

const CommandAssertInvalid = struct {
    // type: "assert_invalid"
    line: usize,
    filename: []const u8,
    text: []const u8,
    module_type: []const u8,
};

const CommandAssertExhaustion = struct {
    // type: "assert_exhaustion"
    line: usize,
    action: Action,
    text: []const u8,
    expected: []const ValueTrap,
};

const CommandAssertUnlinkable = struct {
    // type: "assert_unlinkable"
    line: usize,
    filename: []const u8,
    text: []const u8,
    module_type: []const u8,
};

const CommandAssertUninstantiable = struct {
    // type: "assert_uninstantiable"
    line: usize,
    filename: []const u8,
    text: []const u8,
    module_type: []const u8,
};

const CommandAction = struct {
    // type: "action"
    line: usize,
    action: Action,
    expected: []const ValueTrap,
};

const CommandRegister = struct {
    // type: "register"
    line: usize,
    name: ?[]const u8 = null,
    as: []const u8,
};


const Action = union(enum) {
    invoke: ActionInvoke,
    get: ActionGet,

    pub fn jsonParseFromValue(allocator: mem.Allocator, source: std.json.Value, options: std.json.ParseOptions) !@This() {
        if (source != .object) return error.UnexpectedToken;

        const type_value = source.object.get("type") orelse return error.UnexpectedToken;
        if (type_value != .string) return error.UnexpectedToken;

        const type_str = type_value.string;
        var child_options = options;

        child_options.ignore_unknown_fields = true;

        if (std.mem.eql(u8, type_str, "invoke")) return .{ .invoke = try std.json.parseFromValueLeaky(ActionInvoke, allocator, source, child_options) };
        if (std.mem.eql(u8, type_str, "get")) return .{ .get = try std.json.parseFromValueLeaky(ActionGet, allocator, source, child_options) };

        return error.UnexpectedToken;
    }
};

const ActionInvoke = struct {
    // type: "invoke"
    field: []const u8,
    module: ?[]const u8 = null,
    args: []const Value,
};

const ActionGet = struct {
    // type: "get"
    field: []const u8,
    module: ?[]const u8 = null,
};


const Value = struct {
    type: []const u8,
    value: []const u8,
};

const ValueTrap = struct {
    type: []const u8,
};
