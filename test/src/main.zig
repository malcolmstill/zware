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
    std.debug.warn("json_string = {}\n", .{json_string});
    const r = try json.parse(Wast, &json.TokenStream.init(json_string), json.ParseOptions{ .allocator = &arena.allocator });
    std.debug.warn("r = {}\n", .{r});
    const wasm = "test.wasm";

    // 3. Load .wasm from file
    const program = try fs.cwd().readFileAlloc(&arena.allocator, wasm, 0xFFFFFFF);

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
        line: []const u8,
        filename: []const u8,
    },
    assert_return: struct {
        comptime @"type": []const u8 = "assert_return",
        line: []const u8,
        action: Action,
        expected: []const Value,
    },
    assert_malformed: struct {
        comptime @"type": []const u8 = "assert_malformed",
        line: []const u8,
        filename: []const u8,
        text: []const u8,
        module_type: []const u8,
    },
    assert_invalid: struct {
        comptime @"type": []const u8 = "assert_invalid",
        line: []const u8,
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
