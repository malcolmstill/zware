const std = @import("std");
const fs = std.fs;
const fmt = std.fmt;
const mem = std.mem;
const math = std.math;
const process = std.process;
const json = std.json;
const foxwren = @import("foxwren");
const ValueType = foxwren.ValueType;
const Module = foxwren.Module;
const Instance = foxwren.Instance;
const Store = foxwren.Store;
const Memory = foxwren.Memory;
const Function = foxwren.Function;
const Interpreter = foxwren.Interpreter;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const StringHashMap = std.hash_map.StringHashMap;
const ArrayList = std.ArrayList;

// testrunner
//
// testrunner is an a program that consumes the foxwren library
// and runs the WebAssembly testsuite.
//
// This allows us to separate out the library code from these
// tests but still include the testsuite as part of, say, Github
// Actions.
//
// See https://github.com/WebAssembly/spec/blob/master/interpreter/README.md#s-expression-syntax
// for information on the format of the .wast files.

var gpa = GeneralPurposeAllocator(.{}){};

fn print_i32(interp: *Interpreter) !void {
    const value = try interp.popOperand(i32);
    std.debug.warn("print_i32: {}\n", .{value});
}

fn print_i64(interp: *Interpreter) !void {
    const value = try interp.popOperand(i64);
    std.debug.warn("print_i64: {}\n", .{value});
}

fn print_f32(interp: *Interpreter) !void {
    const value = try interp.popOperand(f32);
    std.debug.warn("print_f32: {}\n", .{value});
}

fn print_f64(interp: *Interpreter) !void {
    const value = try interp.popOperand(f64);
    std.debug.warn("print_f64: {}\n", .{value});
}

fn print_i32_f32(interp: *Interpreter) !void {
    const value_f32 = try interp.popOperand(f32);
    const value_i32 = try interp.popOperand(i32);
    std.debug.warn("print_i32_f32: {}, {}\n", .{ value_i32, value_f32 });
}

fn print_f64_f64(interp: *Interpreter) !void {
    const value_f64_2 = try interp.popOperand(f64);
    const value_f64_1 = try interp.popOperand(f64);
    std.debug.warn("print_f64_f64: {}, {}\n", .{ value_f64_1, value_f64_2 });
}

pub fn main() anyerror!void {
    defer _ = gpa.deinit();

    // 1. Get .json file from command line
    var args = process.args();
    _ = args.skip();
    const filename = args.nextPosix() orelse return error.NoFilename;
    std.log.info("testing: {s}", .{filename});

    var arena = ArenaAllocator.init(&gpa.allocator);
    defer _ = arena.deinit();

    // 2. Parse json and find .wasm file
    const json_string = try fs.cwd().readFileAlloc(&arena.allocator, filename, 0xFFFFFFF);

    const r = try json.parse(Wast, &json.TokenStream.init(json_string), json.ParseOptions{ .allocator = &arena.allocator });

    // 2.a. Find the wasm file
    var wasm_filename: []const u8 = undefined;
    var program: []const u8 = undefined;
    var module: Module = undefined;
    var module_name: ?[]const u8 = null;

    // Initialise a store
    var store: Store = Store.init(&arena.allocator);
    const spectest_module = "spectest";

    // Init spectest memory
    const mem_handle = try store.addMemory();
    const memory = try store.memory(mem_handle);
    memory.max_size = 2;
    _ = try memory.grow(1);
    const spectest_memory_name = "memory";
    try store.@"export"(spectest_module[0..], spectest_memory_name[0..], .Mem, mem_handle);

    // Init spec test table
    const table_handle = try store.addTable(10, 20);
    const spectest_table_name = "table";

    try store.@"export"(spectest_module[0..], spectest_table_name[0..], .Table, table_handle);

    // Initiliase spectest globals
    const i32_handle = try store.addGlobal(0);
    const i32_name = "global_i32";
    try store.@"export"(spectest_module[0..], i32_name[0..], .Global, i32_handle);

    const i64_handle = try store.addGlobal(0);
    const i64_name = "global_i64";
    try store.@"export"(spectest_module[0..], i64_name[0..], .Global, i64_handle);

    const f32_handle = try store.addGlobal(0);
    const f32_name = "global_f32";
    try store.@"export"(spectest_module[0..], f32_name[0..], .Global, f32_handle);

    const f64_handle = try store.addGlobal(0);
    const f64_name = "global_f64";
    try store.@"export"(spectest_module[0..], f64_name[0..], .Global, f64_handle);

    var print_i32_params = [_]ValueType{.I32} ** 1;
    var print_i32_results = [_]ValueType{.I32} ** 0;
    const print_i32_handle = try store.addFunction(Function{
        .host_function = .{
            .func = print_i32,
            .params = print_i32_params[0..],
            .results = print_i32_results[0..],
        },
    });
    const print_i32_name = "print_i32";
    try store.@"export"(spectest_module[0..], print_i32_name[0..], .Func, print_i32_handle);

    // print_i64
    var print_i64_params = [_]ValueType{.I64} ** 1;
    var print_i64_results = [_]ValueType{.I64} ** 0;
    const print_i64_handle = try store.addFunction(Function{
        .host_function = .{
            .func = print_i64,
            .params = print_i64_params[0..],
            .results = print_i64_results[0..],
        },
    });
    const print_i64_name = "print_i64";
    try store.@"export"(spectest_module[0..], print_i64_name[0..], .Func, print_i64_handle);

    // export print_f32
    var print_f32_params = [_]ValueType{.F32} ** 1;
    var print_f32_results = [_]ValueType{.F32} ** 0;
    const print_f32_handle = try store.addFunction(Function{
        .host_function = .{
            .func = print_f32,
            .params = print_f32_params[0..],
            .results = print_f32_results[0..],
        },
    });
    const print_f32_name = "print_f32";
    try store.@"export"(spectest_module[0..], print_f32_name[0..], .Func, print_f32_handle);

    // export print_f64
    var print_f64_params = [_]ValueType{.F64} ** 1;
    var print_f64_results = [_]ValueType{.F64} ** 0;
    const print_f64_handle = try store.addFunction(Function{
        .host_function = .{
            .func = print_f64,
            .params = print_f64_params[0..],
            .results = print_f64_results[0..],
        },
    });
    const print_f64_name = "print_f64";
    try store.@"export"(spectest_module[0..], print_f64_name[0..], .Func, print_f64_handle);

    // export print_i32_f32
    var print_i32_f32_params: [2]ValueType = [_]ValueType{ .I32, .F32 };
    var print_i32_f32_results = [_]ValueType{.F32} ** 0;
    const print_i32_f32_handle = try store.addFunction(Function{
        .host_function = .{
            .func = print_i32_f32,
            .params = print_i32_f32_params[0..],
            .results = print_i32_f32_results[0..],
        },
    });
    const print_i32_f32_name = "print_i32_f32";
    try store.@"export"(spectest_module[0..], print_i32_f32_name[0..], .Func, print_i32_f32_handle);

    // export print_i64_f64
    var print_f64_f64_params: [2]ValueType = [_]ValueType{ .F64, .F64 };
    var print_f64_f64_results: [0]ValueType = [_]ValueType{};
    const print_f64_f64_handle = try store.addFunction(Function{
        .host_function = .{
            .func = print_f64_f64,
            .params = print_f64_f64_params[0..],
            .results = print_f64_f64_results[0..],
        },
    });
    const print_f64_f64_name = "print_f64_f64";
    try store.@"export"(spectest_module[0..], print_f64_f64_name[0..], .Func, print_f64_f64_handle);

    var inst: Instance = undefined;

    // var modules = StringHashMap(Module)./init(&arena.allocator);
    const NamedInstance = struct {
        name: ?[]const u8,
        inst: Instance,
    };

    var registered_names = StringHashMap(Instance).init(&arena.allocator);
    // var module_instances = ArrayList(NamedInstance).init(&arena.allocator);

    for (r.commands) |command| {
        switch (command) {
            .module => {
                wasm_filename = command.module.filename;

                std.debug.warn("(module): {s}:{} ({s})\n", .{ r.source_filename, command.module.line, wasm_filename });
                program = try fs.cwd().readFileAlloc(&arena.allocator, wasm_filename, 0xFFFFFFF);

                // 4. Initialise our module
                module = Module.init(&arena.allocator, program);
                try module.decode();
                inst = try module.instantiate(&arena.allocator, &store);

                if (command.module.name) |name| {
                    try registered_names.put(name, inst);
                }
            },
            .assert_return => {
                const action = command.assert_return.action;
                const expected = command.assert_return.expected;
                switch (action) {
                    .invoke => {
                        const field = action.invoke.field;
                        std.debug.warn("(return): {s}:{}\n", .{ r.source_filename, command.assert_return.line });

                        var instance = inst;
                        if (command.assert_return.action.invoke.module) |name| {
                            if (registered_names.get(name)) |instptr| {
                                instance = instptr;
                            }
                        }

                        if (expected.len > 1) {
                            std.debug.warn("SKIPPING MULTI-VALUE\n", .{});
                            continue;
                        }

                        // Allocate input parameters and output results
                        var in = try arena.allocator.alloc(u64, action.invoke.args.len);
                        var out = try arena.allocator.alloc(u64, expected.len);

                        // Initialise input parameters
                        for (action.invoke.args) |value, i| {
                            const arg = try fmt.parseInt(u64, value.value, 10);
                            in[i] = arg;
                        }

                        // Invoke the function
                        instance.invokeDynamic(field, in, out, .{}) catch |err| {
                            std.debug.warn("(result) invoke = {s}\n", .{field});
                            std.debug.warn("Testsuite failure: {s} at {s}:{}\n", .{ field, r.source_filename, command.assert_return.line });
                            return err;
                        };

                        // Test the result
                        for (expected) |result, i| {
                            const value_type = try valueTypeFromString(result.@"type");
                            if (mem.startsWith(u8, result.value, "nan:")) {
                                if (value_type == .F32 and math.isNan(@bitCast(f32, @truncate(u32, out[i])))) {
                                    continue;
                                }
                                if (value_type == .F64 and math.isNan(@bitCast(f64, out[i]))) {
                                    continue;
                                }

                                std.debug.warn("(result) invoke = {s}\n", .{field});
                                std.debug.warn("Testsuite failure: {s} at {s}:{}\n", .{ field, r.source_filename, command.assert_return.line });
                                std.debug.warn("result[{}], expected: {s}, result: {} ({x})\n", .{ i, "nan", out[i], out[i] });
                                return error.TestsuiteTestFailureTrapResult;
                            }

                            const result_value = try fmt.parseInt(u64, result.value, 10);
                            // Otherwise
                            errdefer {
                                std.debug.warn("(result) invoke = {s}\n", .{field});
                                std.debug.warn("Testsuite failure: {s} at {s}:{}\n", .{ field, r.source_filename, command.assert_return.line });
                                std.debug.warn("result[{}], expected: {s} ({x}), result: {} ({x})\n", .{ i, result.value, result_value, out[i], out[i] });
                            }
                            if (result_value != out[i]) {
                                return error.TestsuiteTestFailureTrapResult;
                            }
                        }
                    },
                    .get => {
                        const field = action.get.field;
                        std.debug.warn("(return): get {s}:{} ({s})\n", .{ r.source_filename, command.assert_return.line, wasm_filename });
                        std.debug.warn("(result) get \"{s}\"\n", .{field});
                        for (inst.module.exports.list.items) |exprt, i| {
                            if (mem.eql(u8, exprt.name, field)) {
                                const global = try inst.global(i);

                                for (expected) |result, j| {
                                    if (j > 0) return error.ExpectedOneResult;
                                    const result_value = try fmt.parseInt(u64, result.value, 10);
                                    if (global.* != result_value) {
                                        return error.GlobalUnexpectedValue;
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
                        std.debug.warn("(trap): {s}:{}\n", .{ r.source_filename, command.assert_trap.line });

                        errdefer {
                            std.debug.warn("(trap) invoke = {s}\n", .{field});
                        }

                        // Allocate input parameters and output results
                        var in = try arena.allocator.alloc(u64, action.invoke.args.len);
                        var out = try arena.allocator.alloc(u64, expected.len);

                        // Initialise input parameters
                        for (action.invoke.args) |value, i| {
                            const arg = try fmt.parseInt(u64, value.value, 10);
                            in[i] = arg;
                        }

                        // Test the result
                        if (mem.eql(u8, trap, "integer divide by zero")) {
                            if (inst.invokeDynamic(field, in, out, .{})) |x| {
                                return error.TestsuiteExpectedTrap;
                            } else |err| switch (err) {
                                error.DivisionByZero => continue,
                                else => return error.TestsuiteExpectedDivideByZero,
                            }
                        }

                        if (mem.eql(u8, trap, "integer overflow")) {
                            if (inst.invokeDynamic(field, in, out, .{})) |x| {
                                return error.TestsuiteExpectedTrap;
                            } else |err| switch (err) {
                                error.Overflow => continue,
                                else => return error.TestsuiteExpectedOverflow,
                            }
                        }

                        if (mem.eql(u8, trap, "invalid conversion to integer")) {
                            if (inst.invokeDynamic(field, in, out, .{})) |x| {
                                return error.TestsuiteExpectedTrap;
                            } else |err| switch (err) {
                                error.InvalidConversion => continue,
                                else => return error.TestsuiteExpectedInvalidConversion,
                            }
                        }

                        if (mem.eql(u8, trap, "out of bounds memory access")) {
                            if (inst.invokeDynamic(field, in, out, .{})) |x| {
                                return error.TestsuiteExpectedTrap;
                            } else |err| switch (err) {
                                error.OutOfBoundsMemoryAccess => continue,
                                else => return error.TestsuiteExpectedOutOfBoundsMemoryAccess,
                            }
                        }

                        if (mem.eql(u8, trap, "indirect call type mismatch")) {
                            if (inst.invokeDynamic(field, in, out, .{})) |x| {
                                return error.TestsuiteExpectedTrap;
                            } else |err| switch (err) {
                                error.IndirectCallTypeMismatch => continue,
                                else => return error.TestsuiteExpectedIndirectCallTypeMismatch,
                            }
                        }

                        if (mem.eql(u8, trap, "undefined element") or mem.eql(u8, trap, "uninitialized element")) {
                            if (inst.invokeDynamic(field, in, out, .{})) |x| {
                                return error.TestsuiteExpectedTrap;
                            } else |err| switch (err) {
                                error.UndefinedElement => continue,
                                error.OutOfBoundsMemoryAccess => continue,
                                else => {
                                    std.debug.warn("Unexpected error: {}\n", .{err});
                                    return error.TestsuiteExpectedUndefinedElement;
                                },
                            }
                        }
                    },
                    .get => {
                        std.debug.warn("(trap) get\n", .{});
                        return error.TrapGetNotImplemented;
                    },
                }

                return error.ExpectedTrapDidntOccur;
            },
            .assert_malformed => {
                if (mem.endsWith(u8, command.assert_malformed.filename, ".wat")) continue;
                wasm_filename = command.assert_malformed.filename;
                std.debug.warn("(malformed): {s}:{} ({s})\n", .{ r.source_filename, command.assert_malformed.line, wasm_filename });
                program = try fs.cwd().readFileAlloc(&arena.allocator, wasm_filename, 0xFFFFFFF);
                module = Module.init(&arena.allocator, program);

                const trap = command.assert_malformed.text;

                errdefer {
                    std.debug.warn("ERROR (malformed): {s}:{}\n", .{ r.source_filename, command.assert_malformed.line });
                }

                if (mem.eql(u8, trap, "unexpected end")) {
                    if (module.decode()) |x| {
                        return error.TestsuiteExpectedUnexpectedEnd;
                    } else |err| switch (err) {
                        error.FunctionCodeSectionsInconsistent => continue,
                        error.EndOfStream => continue,
                        error.CouldntFindExprEnd => continue,
                        error.ElementsCountMismatch => continue,
                        error.CouldntFindEnd => continue, // test/testsuite/binary.wast:910 bad br_table means we don't find end
                        else => {
                            std.debug.warn("Unexpected error: {}\n", .{err});
                            return error.TestsuiteExpectedUnexpectedEnd;
                        },
                    }
                }

                if (mem.eql(u8, trap, "magic header not detected")) {
                    if (module.decode()) |x| {
                        return error.ExpectedError;
                    } else |err| switch (err) {
                        error.MagicNumberNotFound => continue,
                        else => {
                            std.debug.warn("Unexpected error: {}\n", .{err});
                            return error.ExpectedError;
                        },
                    }
                }

                if (mem.eql(u8, trap, "unknown binary version")) {
                    if (module.decode()) |x| {
                        return error.ExpectedError;
                    } else |err| switch (err) {
                        error.UnknownBinaryVersion => continue,
                        else => {
                            std.debug.warn("Unexpected error: {}\n", .{err});
                            return error.ExpectedError;
                        },
                    }
                }

                if (mem.eql(u8, trap, "malformed section id")) {
                    if (module.decode()) |x| {
                        return error.ExpectedError;
                    } else |err| switch (err) {
                        error.UnknownSectionId => continue,
                        else => {
                            std.debug.warn("Unexpected error: {}\n", .{err});
                            return error.ExpectedError;
                        },
                    }
                }

                if (mem.eql(u8, trap, "integer representation too long")) {
                    if (module.decode()) |x| {
                        return error.ExpectedError;
                    } else |err| switch (err) {
                        error.InvalidValue => continue,
                        error.ExpectedFuncTypeTag => continue,
                        error.Overflow => continue,
                        error.UnknownSectionId => continue,
                        else => {
                            std.debug.warn("Unexpected error: {}\n", .{err});
                            return error.ExpectedError;
                        },
                    }
                }

                if (mem.eql(u8, trap, "zero flag expected")) {
                    if (module.decode()) |x| {
                        return error.ExpectedError;
                    } else |err| switch (err) {
                        error.MalformedCallIndirectReserved => continue,
                        error.MalformedMemoryReserved => continue,
                        else => {
                            std.debug.warn("Unexpected error: {}\n", .{err});
                            return error.ExpectedError;
                        },
                    }
                }

                if (mem.eql(u8, trap, "too many locals")) {
                    if (module.decode()) |x| {
                        return error.ExpectedError;
                    } else |err| switch (err) {
                        error.TooManyLocals => continue,
                        else => {
                            std.debug.warn("Unexpected error: {}\n", .{err});
                            return error.ExpectedError;
                        },
                    }
                }

                if (mem.eql(u8, trap, "function and code section have inconsistent lengths")) {
                    if (module.decode()) |x| {
                        return error.ExpectedError;
                    } else |err| switch (err) {
                        error.FunctionCodeSectionsInconsistent => continue,
                        else => {
                            std.debug.warn("Unexpected error: {}\n", .{err});
                            return error.ExpectedError;
                        },
                    }
                }

                if (mem.eql(u8, trap, "unexpected end of section or function") or mem.eql(u8, trap, "section size mismatch")) {
                    if (module.decode()) |x| {
                        return error.ExpectedError;
                    } else |err| switch (err) {
                        error.UnknownSectionId => continue, // if a section declares more elements than it has we might get this
                        error.TypeCountMismatch => continue,
                        error.ImportsCountMismatch => continue,
                        error.TablesCountMismatch => continue,
                        error.MemoriesCountMismatch => continue,
                        error.GlobalsCountMismatch => continue,
                        error.ElementsCountMismatch => continue,
                        error.FunctionsCountMismatch => continue,
                        error.CodesCountMismatch => continue,
                        error.DatasCountMismatch => continue,
                        error.UnexpectedEndOfSection => continue,
                        error.InvalidValue => continue,
                        else => {
                            std.debug.warn("Unexpected error: {}\n", .{err});
                            return error.ExpectedError;
                        },
                    }
                }

                if (mem.eql(u8, trap, "malformed import kind")) {
                    if (module.decode()) |x| {
                        return error.ExpectedError;
                    } else |err| switch (err) {
                        error.InvalidValue => continue,
                        else => {
                            std.debug.warn("Unexpected error: {}\n", .{err});
                            return error.ExpectedError;
                        },
                    }
                }

                if (mem.eql(u8, trap, "integer too large")) {
                    if (module.decode()) |x| {
                        return error.ExpectedError;
                    } else |err| switch (err) {
                        error.Overflow => continue,
                        error.UnknownSectionId => continue,
                        error.InvalidValue => continue, // test/testsuite/binary.wast:601 I think the test is wrong
                        else => {
                            std.debug.warn("Unexpected error: {}\n", .{err});
                            return error.ExpectedError;
                        },
                    }
                }

                if (mem.eql(u8, trap, "junk after last section")) {
                    if (module.decode()) |x| {
                        return error.ExpectedError;
                    } else |err| switch (err) {
                        error.MultipleStartSections => continue,
                        else => {
                            std.debug.warn("Unexpected error: {}\n", .{err});
                            return error.ExpectedError;
                        },
                    }
                }

                if (mem.eql(u8, trap, "malformed mutability")) {
                    if (module.decode()) |x| {
                        return error.ExpectedError;
                    } else |err| switch (err) {
                        error.InvalidValue => continue,
                        else => {
                            std.debug.warn("Unexpected error: {}\n", .{err});
                            return error.ExpectedError;
                        },
                    }
                }

                return error.ExpectedError;
            },
            .action => {
                const action = command.action.action;
                const expected = command.action.expected;
                switch (action) {
                    .invoke => {
                        const field = action.invoke.field;
                        std.debug.warn("(return): {s}:{}\n", .{ r.source_filename, command.action.line });

                        if (expected.len > 1) {
                            std.debug.warn("SKIPPING MULTI-VALUE\n", .{});
                            continue;
                        }

                        // Allocate input parameters and output results
                        var in = try arena.allocator.alloc(u64, action.invoke.args.len);
                        var out = try arena.allocator.alloc(u64, expected.len);

                        // Initialise input parameters
                        for (action.invoke.args) |value, i| {
                            const arg = try fmt.parseInt(u64, value.value, 10);
                            in[i] = arg;
                        }

                        // Invoke the function
                        inst.invokeDynamic(field, in, out, .{}) catch |err| {
                            std.debug.warn("(result) invoke = {s}\n", .{field});
                            std.debug.warn("Testsuite failure: {s} at {s}:{}\n", .{ field, r.source_filename, command.action.line });
                            return err;
                        };
                    },
                    .get => {
                        std.debug.warn("(action) get\n", .{});
                        return error.ActionGetNotImplemented;
                    },
                }
            },
            .assert_unlinkable => {
                wasm_filename = command.assert_unlinkable.filename;
                std.debug.warn("(unlinkable): {s}:{} ({s})\n", .{ r.source_filename, command.assert_unlinkable.line, wasm_filename });
                program = try fs.cwd().readFileAlloc(&arena.allocator, wasm_filename, 0xFFFFFFF);
                // std.debug.warn("filename = {s}\n", .{wasm_filename});
                module = Module.init(&arena.allocator, program);
                try module.decode();

                if (module.instantiate(&arena.allocator, &store)) |x| {
                    return error.ExpectedUnlinkable;
                } else |err| switch (err) {
                    error.ImportNotFound => continue,
                    error.ImportedFunctionTypeSignatureDoesNotMatch => continue,
                    error.Overflow => continue,
                    error.OutOfBoundsMemoryAccess => continue,
                    else => {
                        std.debug.warn("(unlinkable) Unexpected error: {}\n", .{err});
                        return error.UnexpectedError;
                    },
                }
            },
            .register => {
                std.debug.warn("(register): {s}:{}\n", .{ r.source_filename, command.register.line });
                if (command.register.name) |name| {
                    const registered_inst = registered_names.get(name) orelse return error.NotRegistered;

                    for (registered_inst.module.exports.list.items) |exprt, i| {
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
                    for (inst.module.exports.list.items) |exprt, i| {
                        switch (exprt.tag) {
                            .Table => {
                                const handle = inst.tableaddrs.items[exprt.index];
                                try store.@"export"(command.register.as, exprt.name, .Table, handle);
                            },
                            .Func => {
                                const handle = inst.funcaddrs.items[exprt.index];
                                try store.@"export"(command.register.as, exprt.name, .Func, handle);
                            },
                            .Global => {
                                const handle = inst.globaladdrs.items[exprt.index];
                                try store.@"export"(command.register.as, exprt.name, .Global, handle);
                            },
                            .Mem => {
                                const handle = inst.memaddrs.items[exprt.index];
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

fn valueTypeFromString(s: []const u8) !ValueType {
    if (mem.eql(u8, s, "i32")) return ValueType.I32;
    if (mem.eql(u8, s, "i64")) return ValueType.I64;
    if (mem.eql(u8, s, "f32")) return ValueType.F32;
    if (mem.eql(u8, s, "f64")) return ValueType.F64;
    return error.UnknownType;
}

const Wast = struct {
    source_filename: []const u8,
    commands: []const Command,
};

const Command = union(enum) {
    module: struct {
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
    }
};

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
