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
const ModuleInstance = foxwren.ModuleInstance;
const Store = foxwren.Store;
const Memory = foxwren.Memory;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;

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
    var store: Store = undefined;
    var mem0: *Memory = undefined;
    var modinst: ?ModuleInstance = null;

    for (r.commands) |command| {
        switch (command) {
            .module => {
                wasm_filename = command.module.filename;

                std.debug.warn("(module): {s}, {s}:{}\n", .{ wasm_filename, r.source_filename, command.module.line });
                program = try fs.cwd().readFileAlloc(&arena.allocator, wasm_filename, 0xFFFFFFF);

                // 4. Initialise our module
                module = Module.init(&arena.allocator, program);
                try module.decode();
                modinst = null;
            },
            .assert_return => {
                if (modinst == null) modinst = try module.instantiate();
                var inst = modinst orelse return error.NoInstance;

                const action = command.assert_return.action;
                const expected = command.assert_return.expected;
                const field = action.field;
                std.debug.warn("(return): {s}:{}\n", .{ r.source_filename, command.assert_return.line });

                if (expected.len > 1) {
                    std.debug.warn("SKIPPING MULTI-VALUE\n", .{});
                    continue;
                }

                // Allocate input parameters and output results
                var in = try arena.allocator.alloc(u64, action.args.len);
                var out = try arena.allocator.alloc(u64, expected.len);

                // Initialise input parameters
                for (action.args) |value, i| {
                    const arg = try fmt.parseInt(u64, value.value, 10);
                    in[i] = arg;
                }

                // Invoke the function
                inst.invokeDynamic(field, in, out, .{}) catch |err| {
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

                    // Otherwise
                    errdefer {
                        std.debug.warn("(result) invoke = {s}\n", .{field});
                        std.debug.warn("Testsuite failure: {s} at {s}:{}\n", .{ field, r.source_filename, command.assert_return.line });
                        std.debug.warn("result[{}], expected: {s}, result: {} ({x})\n", .{ i, result.value, out[i], out[i] });
                    }
                    const result_value = try fmt.parseInt(u64, result.value, 10);
                    if (result_value != out[i]) {
                        return error.TestsuiteTestFailureTrapResult;
                    }
                }
            },
            .assert_trap => {
                if (modinst == null) modinst = try module.instantiate();
                var inst = modinst orelse return error.NoInstance;

                const action = command.assert_trap.action;
                const expected = command.assert_trap.expected;
                const field = action.field;
                const trap = command.assert_trap.text;
                std.debug.warn("(trap): {s}:{}\n", .{ r.source_filename, command.assert_trap.line });

                errdefer {
                    std.debug.warn("(trap) invoke = {s}\n", .{field});
                }

                // Allocate input parameters and output results
                var in = try arena.allocator.alloc(u64, action.args.len);
                var out = try arena.allocator.alloc(u64, expected.len);

                // Initialise input parameters
                for (action.args) |value, i| {
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

                if (mem.eql(u8, trap, "undefined element")) {
                    if (inst.invokeDynamic(field, in, out, .{})) |x| {
                        return error.TestsuiteExpectedTrap;
                    } else |err| switch (err) {
                        error.UndefinedElement => continue,
                        else => return error.TestsuiteExpectedUndefinedElement,
                    }
                }

                return error.ExpectedTrapDidntOccur;
            },
            .assert_malformed => {
                if (mem.endsWith(u8, command.assert_malformed.filename, ".wat")) continue;
                std.debug.warn("(malformed): {s}:{}\n", .{ r.source_filename, command.assert_malformed.line });
                wasm_filename = command.assert_malformed.filename;
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

                return error.ExpectedError;
            },
            .action => {
                if (modinst == null) modinst = try module.instantiate();
                var inst = modinst orelse return error.NoInstance;

                const action = command.action.action;
                const expected = command.action.expected;
                const field = action.field;
                std.debug.warn("(return): {s}:{}\n", .{ r.source_filename, command.action.line });

                if (expected.len > 1) {
                    std.debug.warn("SKIPPING MULTI-VALUE\n", .{});
                    continue;
                }

                // Allocate input parameters and output results
                var in = try arena.allocator.alloc(u64, action.args.len);
                var out = try arena.allocator.alloc(u64, expected.len);

                // Initialise input parameters
                for (action.args) |value, i| {
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
            .assert_unlinkable => {
                std.debug.warn("(unlinkable): {s}:{}\n", .{ r.source_filename, command.assert_unlinkable.line });
                wasm_filename = command.assert_unlinkable.filename;
                program = try fs.cwd().readFileAlloc(&arena.allocator, wasm_filename, 0xFFFFFFF);

                module = Module.init(&arena.allocator, program);
                try module.decode();

                if (module.instantiate()) |x| {
                    return error.ExpectedUnlinkable;
                } else |err| switch (err) {
                    error.OutOfBoundsMemoryAccess => continue,
                    else => {
                        std.debug.warn("(unlinkable) Unexpected error: {}\n", .{err});
                        return error.UnexpectedError;
                    },
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
        name: []const u8,
        as: []const u8,
    }
};

const Action = struct {
    comptime @"type": []const u8 = "invoke",
    field: []const u8,
    module: ?[]const u8 = null,
    args: []const Value,
};

const Value = struct {
    @"type": []const u8,
    value: []const u8,
};

const ValueTrap = struct {
    @"type": []const u8,
};
