const std = @import("std");
const mem = std.mem;
const process = std.process;
const fs = std.fs;
const fw = @import("foxwren");
const Module = fw.Module;
const Store = fw.Store;
const ValueType = fw.ValueType;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;

var gpa = GeneralPurposeAllocator(.{}){};

pub fn main() anyerror!void {
    defer _ = gpa.deinit();

    // We will allocate the module with an arena allocator and free it all once
    var arena = ArenaAllocator.init(&gpa.allocator);
    defer _ = arena.deinit();

    const program = try fs.cwd().readFileAlloc(&arena.allocator, "../../test/fib.wasm", 0xFFFFFFF);

    // Load and parse a module
    var module = Module.init(&arena.allocator, program);
    try module.decode();
    module.print();

    var module_instance = try module.instantiate();

    const result = try module_instance.invoke("fib", .{@as(i32, 30)}, i32, .{});
    std.debug.warn("result: {}\n", .{result});
}
