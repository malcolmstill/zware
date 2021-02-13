const std = @import("std");
const fs = std.fs;
const process = std.process;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const foxwren = @import("foxwren");
const Module = foxwren.Module;
const Store = foxwren.Store;

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

    // 1. Get wasm file to test from command line
    var args = process.args();
    _ = args.skip();
    const filename = args.nextPosix() orelse return error.NoFilename;

    var arena = ArenaAllocator.init(&gpa.allocator);
    defer _ = arena.deinit();

    const program = try fs.cwd().readFileAlloc(&arena.allocator, filename, 0xFFFFFFF);

    // 2. Initialise our module
    var module = Module.init(&arena.allocator, program);
    try module.decode();

    var store = Store.init(&arena.allocator);
    var mem0 = try store.addMemory();
    _ = try mem0.grow(1);

    var modinst = try module.instantiate(&store);

    // 3. Run test
}
