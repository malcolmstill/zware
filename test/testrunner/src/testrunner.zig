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
    var args = process.args();
    _ = args.skip();
    const filename = args.next() orelse return error.NoFilename;
    std.log.info("testing: {s}", .{filename});

    var arena = ArenaAllocator.init(gpa.allocator());
    defer _ = arena.deinit();

    const alloc = arena.allocator();

    // 2. Parse json and find .wasm file
    const json_string = try fs.cwd().readFileAlloc(alloc, filename, 0xFFFFFFF);

    // See https://github.com/ziglang/zig/issues/12624
    comptime {
        @setEvalBranchQuota(100000);
        _ = json.ParseError([]const Command);
        _ = json.ParseError(Wast);
    }

    var ts = json.TokenStream.init(json_string);
    const r = try json.parse(Wast, &ts, json.ParseOptions{ .allocator = alloc });

    // 2.a. Find the wasm file
    var wasm_filename: []const u8 = undefined;
    var program: []const u8 = undefined;
    var module: Module = undefined;

    // Initialise a store
    var store: Store = Store.init(alloc);
    const spectest_module = "spectest";

    // Init spectest memory
    const mem_handle = try store.addMemory(1, 2);
    _ = try store.memory(mem_handle);

    const spectest_memory_name = "memory";
    try store.@"export"(spectest_module[0..], spectest_memory_name[0..], .Mem, mem_handle);

    // Init spec test table
    const table_handle = try store.addTable(.FuncRef, 10, 20);
    const spectest_table_name = "table";

    try store.@"export"(spectest_module[0..], spectest_table_name[0..], .Table, table_handle);

    // Initiliase spectest globals
    const i32_handle = try store.addGlobal(Global{
        .value = 666,
        .valtype = .I32,
        .mutability = .Immutable,
    });
    const i32_name = "global_i32";
    try store.@"export"(spectest_module[0..], i32_name[0..], .Global, i32_handle);

    const i64_handle = try store.addGlobal(Global{
        .value = 666,
        .valtype = .I64,
        .mutability = .Immutable,
    });
    const i64_name = "global_i64";
    try store.@"export"(spectest_module[0..], i64_name[0..], .Global, i64_handle);

    const f32_handle = try store.addGlobal(Global{
        .value = 666,
        .valtype = .F32,
        .mutability = .Immutable,
    });
    const f32_name = "global_f32";
    try store.@"export"(spectest_module[0..], f32_name[0..], .Global, f32_handle);

    const f64_handle = try store.addGlobal(Global{
        .value = 666,
        .valtype = .F64,
        .mutability = .Immutable,
    });
    const f64_name = "global_f64";
    try store.@"export"(spectest_module[0..], f64_name[0..], .Global, f64_handle);

    var print_params = [_]ValType{.I32} ** 0;
    var print_results = [_]ValType{.I32} ** 0;
    const print_handle = try store.addFunction(Function{
        .params = print_params[0..],
        .results = print_results[0..],
        .subtype = .{
            .host_function = .{
                .func = print,
            },
        },
    });
    const print_name = "print";
    try store.@"export"(spectest_module[0..], print_name[0..], .Func, print_handle);

    var print_i32_params = [_]ValType{.I32} ** 1;
    var print_i32_results = [_]ValType{.I32} ** 0;
    const print_i32_handle = try store.addFunction(Function{
        .params = print_i32_params[0..],
        .results = print_i32_results[0..],
        .subtype = .{
            .host_function = .{
                .func = print_i32,
            },
        },
    });
    const print_i32_name = "print_i32";
    try store.@"export"(spectest_module[0..], print_i32_name[0..], .Func, print_i32_handle);

    // print_i64
    var print_i64_params = [_]ValType{.I64} ** 1;
    var print_i64_results = [_]ValType{.I64} ** 0;
    const print_i64_handle = try store.addFunction(Function{
        .params = print_i64_params[0..],
        .results = print_i64_results[0..],
        .subtype = .{
            .host_function = .{
                .func = print_i64,
            },
        },
    });
    const print_i64_name = "print_i64";
    try store.@"export"(spectest_module[0..], print_i64_name[0..], .Func, print_i64_handle);

    // export print_f32
    var print_f32_params = [_]ValType{.F32} ** 1;
    var print_f32_results = [_]ValType{.F32} ** 0;
    const print_f32_handle = try store.addFunction(Function{
        .params = print_f32_params[0..],
        .results = print_f32_results[0..],
        .subtype = .{
            .host_function = .{
                .func = print_f32,
            },
        },
    });
    const print_f32_name = "print_f32";
    try store.@"export"(spectest_module[0..], print_f32_name[0..], .Func, print_f32_handle);

    // export print_f64
    var print_f64_params = [_]ValType{.F64} ** 1;
    var print_f64_results = [_]ValType{.F64} ** 0;
    const print_f64_handle = try store.addFunction(Function{
        .params = print_f64_params[0..],
        .results = print_f64_results[0..],
        .subtype = .{
            .host_function = .{
                .func = print_f64,
            },
        },
    });
    const print_f64_name = "print_f64";
    try store.@"export"(spectest_module[0..], print_f64_name[0..], .Func, print_f64_handle);

    // export print_i32_f32
    var print_i32_f32_params: [2]ValType = [_]ValType{ .I32, .F32 };
    var print_i32_f32_results = [_]ValType{.F32} ** 0;
    const print_i32_f32_handle = try store.addFunction(Function{
        .params = print_i32_f32_params[0..],
        .results = print_i32_f32_results[0..],
        .subtype = .{
            .host_function = .{
                .func = print_i32_f32,
            },
        },
    });
    const print_i32_f32_name = "print_i32_f32";
    try store.@"export"(spectest_module[0..], print_i32_f32_name[0..], .Func, print_i32_f32_handle);

    // export print_i64_f64
    var print_f64_f64_params: [2]ValType = [_]ValType{ .F64, .F64 };
    var print_f64_f64_results: [0]ValType = [_]ValType{};
    const print_f64_f64_handle = try store.addFunction(Function{
        .params = print_f64_f64_params[0..],
        .results = print_f64_f64_results[0..],
        .subtype = .{
            .host_function = .{
                .func = print_f64_f64,
            },
        },
    });
    const print_f64_f64_name = "print_f64_f64";
    try store.@"export"(spectest_module[0..], print_f64_f64_name[0..], .Func, print_f64_f64_handle);

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
                        for (action.invoke.args) |value, i| {
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

                        for (expected) |result, i| {
                            const valtype = try valueTypeFromString(result.@"type");
                            switch (valtype) {
                                .I32, .I64, .F32, .F64 => {
                                    if (mem.startsWith(u8, result.value, "nan:")) {
                                        if (valtype == .F32 and math.isNan(@bitCast(f32, @truncate(u32, out[i])))) {
                                            continue;
                                        }
                                        if (valtype == .F64 and math.isNan(@bitCast(f64, out[i]))) {
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

                                    for (expected) |result, j| {
                                        if (j > 0) return error.ExpectedOneResult;
                                        const result_value = try fmt.parseInt(u64, result.value, 10);

                                        if (global.value != result_value) {
                                            return error.GlobalUnexpectedValue;
                                        }
                                    }
                                }
                            }
                        } else {
                            for (current_instance.module.exports.list.items) |exprt, i| {
                                if (mem.eql(u8, exprt.name, field)) {
                                    const global = try current_instance.getGlobal(i);

                                    for (expected) |result, j| {
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
                        for (action.invoke.args) |value, i| {
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
                        for (action.invoke.args) |value, i| {
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

const Command = union(enum) { module: struct {
    comptime @"type": []const u8 = "module",
    line: usize,
    name: ?[]const u8 = null,
    filename: []const u8,
}, assert_return: struct {
    comptime @"type": []const u8 = "assert_return",
    line: usize,
    action: Action,
    expected: []const Value,
}, assert_trap: struct {
    comptime @"type": []const u8 = "assert_trap",
    line: usize,
    action: Action,
    text: []const u8,
    expected: []const ValueTrap,
}, assert_malformed: struct {
    comptime @"type": []const u8 = "assert_malformed",
    line: usize,
    filename: []const u8,
    text: []const u8,
    module_type: []const u8,
}, assert_invalid: struct {
    comptime @"type": []const u8 = "assert_invalid",
    line: usize,
    filename: []const u8,
    text: []const u8,
    module_type: []const u8,
}, assert_exhaustion: struct {
    comptime @"type": []const u8 = "assert_exhaustion",
    line: usize,
    action: Action,
    text: []const u8,
    expected: []const ValueTrap,
}, assert_unlinkable: struct {
    comptime @"type": []const u8 = "assert_unlinkable",
    line: usize,
    filename: []const u8,
    text: []const u8,
    module_type: []const u8,
}, assert_uninstantiable: struct {
    comptime @"type": []const u8 = "assert_uninstantiable",
    line: usize,
    filename: []const u8,
    text: []const u8,
    module_type: []const u8,
}, action: struct {
    comptime @"type": []const u8 = "action",
    line: usize,
    action: Action,
    expected: []const ValueTrap,
}, register: struct {
    comptime @"type": []const u8 = "register",
    line: usize,
    name: ?[]const u8 = null,
    as: []const u8,
} };

const Action = union(enum) {
    invoke: struct {
        comptime @"type": []const u8 = "invoke",
        field: []const u8,
        module: ?[]const u8 = null,
        args: []const Value,
    },
    get: struct {
        comptime @"type": []const u8 = "get",
        field: []const u8,
        module: ?[]const u8 = null,
    },
};

// const Action = ;

const Value = struct {
    @"type": []const u8,
    value: []const u8,
};

const ValueTrap = struct {
    @"type": []const u8,
};
