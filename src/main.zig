const std = @import("std");
const mem = std.mem;
const process = std.process;
const fs = std.fs;
const Module = @import("module.zig").Module;
const ValueType = @import("common.zig").ValueType;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;

var gpa = GeneralPurposeAllocator(.{}){};

pub fn main() anyerror!void {
    defer _ = gpa.deinit();

    // We will allocate the module with an arena allocator and free it all once
    var arena = ArenaAllocator.init(&gpa.allocator);
    defer _ = arena.deinit();

    const program = try fs.cwd().readFileAlloc(&arena.allocator, "test/test.wasm", 0xFFFFFFF);

    // Load and parse a module
    var module = Module.init(&arena.allocator, program);
    try module.parse();
    module.print();

    const result = try module.callFunction("add", .{ @as(i32, 22), @as(i32, 23) }, i32);
    std.debug.warn("result: {}\n", .{result});
}

test "" {
    _ = @import("validator.zig");
    _ = @import("interpreter.zig");
    _ = @import("module.zig");
}
