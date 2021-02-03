const std = @import("std");
const mem = std.mem;
const process = std.process;
const fs = std.fs;
const Module = @import("module.zig").Module;
const ValueType = @import("module.zig").ValueType;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;

var gpa = GeneralPurposeAllocator(.{}){};

pub fn main() anyerror!void {
    defer _ = gpa.deinit();

    // We will allocate the module with an arena allocator and free it all once
    var arena = ArenaAllocator.init(&gpa.allocator);
    defer _ = arena.deinit();

    const program = try fs.cwd().readFileAlloc(&arena.allocator, "export.wasm", 0xFFFFFFF);

    // Load and parse a module
    var module = Module.init(&arena.allocator, program);
    try module.parse();
    module.print();

    // Get index of add function
    // const idx = try module.getExport(.Func, "add");
    const add = try module.getFunction(2, "add", [2]ValueType{ .I32, .I32 }, .I32);
}
