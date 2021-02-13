const std = @import("std");
const fs = std.fs;
const process = std.process;
const json = std.json;
const foxwren = @import("foxwren");
const Module = foxwren.Module;
const Store = foxwren.Store;
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
    std.log.info("Beginning roll program...", .{});

    defer _ = gpa.deinit();

    // 1. Get .json file from command line
    var args = process.args();
    _ = args.skip();
    const filename = args.nextPosix() orelse return error.NoFilename;

    var arena = ArenaAllocator.init(&gpa.allocator);
    defer _ = arena.deinit();

    // 2. Parse json and find .wasm file
    const json_string = try fs.cwd().readFileAlloc(&arena.allocator, filename, 0xFFFFFFF);

    const r = try json.parse(Wast, &json.TokenStream.init(json_string), json.ParseOptions{ .allocator = &arena.allocator });

    // 2.a. Find the wasm file
    var wasm_filename: []const u8 = undefined;
    for (r.commands) |command| {
        switch (command) {
            .module => {
                wasm_filename = command.module.filename;
                break;
            },
            else => continue,
        }
    }

    // 3. Load .wasm from file
    const program = try fs.cwd().readFileAlloc(&arena.allocator, wasm_filename, 0xFFFFFFF);

    // 4. Initialise our module
    var module = Module.init(&arena.allocator, program);
    try module.decode();

    var store = Store.init(&arena.allocator);
    var mem0 = try store.addMemory();
    _ = try mem0.grow(1);

    var modinst = try module.instantiate(&store);

    // 5. Loop over assert_return's and assert_trap's
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
