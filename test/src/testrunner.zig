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
    var modinst: ModuleInstance = undefined;

    for (r.commands) |command| {
        switch (command) {
            .module => {
                wasm_filename = command.module.filename;

                // 3. Load .wasm from file
                program = try fs.cwd().readFileAlloc(&arena.allocator, wasm_filename, 0xFFFFFFF);

                // 4. Initialise our module
                module = Module.init(&arena.allocator, program);
                try module.decode();

                modinst = try module.instantiate();
            },
            .assert_return => {
                const action = command.assert_return.action;
                const expected = command.assert_return.expected;
                const field = action.field;
                std.debug.warn("test: {s}:{s}\n", .{ wasm_filename, field });

                // Allocate input parameters and output results
                var in = try arena.allocator.alloc(u64, action.args.len);
                var out = try arena.allocator.alloc(u64, expected.len);

                // Initialise input parameters
                for (action.args) |value, i| {
                    const arg = try fmt.parseInt(u64, value.value, 10);
                    in[i] = arg;
                }

                // Invoke the function
                modinst.invokeDynamic(field, in, out, .{}) catch |err| {
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
                const action = command.assert_trap.action;
                const expected = command.assert_trap.expected;
                const field = action.field;
                const trap = command.assert_trap.text;

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
                    if (modinst.invokeDynamic(field, in, out, .{})) |x| {
                        return error.TestsuiteExpectedTrap;
                    } else |err| switch (err) {
                        error.DivisionByZero => continue,
                        else => return error.TestsuiteExpectedDivideByZero,
                    }
                }

                if (mem.eql(u8, trap, "integer overflow")) {
                    if (modinst.invokeDynamic(field, in, out, .{})) |x| {
                        return error.TestsuiteExpectedTrap;
                    } else |err| switch (err) {
                        error.Overflow => continue,
                        else => return error.TestsuiteExpectedOverflow,
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
        filename: []const u8,
    },
    assert_return: struct {
        comptime @"type": []const u8 = "assert_return",
        line: usize,
        action: Action,
        expected: []const Value,
    },
    assert_trap: struct {
        comptime @"type": []const u8 = "assert_trap",
        line: usize,
        action: Action,
        text: []const u8,
        expected: []const ValueTrap,
    },
    assert_malformed: struct {
        comptime @"type": []const u8 = "assert_malformed",
        line: usize,
        filename: []const u8,
        text: []const u8,
        module_type: []const u8,
    },
    assert_invalid: struct {
        comptime @"type": []const u8 = "assert_invalid",
        line: usize,
        filename: []const u8,
        text: []const u8,
        module_type: []const u8,
    },
};

const Action = struct {
    comptime @"type": []const u8 = "invoke",
    field: []const u8,
    args: []const Value,
};

const Value = struct {
    @"type": []const u8,
    value: []const u8,
};

const ValueTrap = struct {
    @"type": []const u8,
};