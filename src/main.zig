const std = @import("std");
const mem = std.mem;
const process = std.process;
const fs = std.fs;
const Engine = @import("engine.zig").Engine;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;

var gpa = GeneralPurposeAllocator(.{}){};

pub fn main() anyerror!void {
    defer _ = gpa.deinit();
    std.log.info("Booting wasm runtime...", .{});

    var arena = ArenaAllocator.init(&gpa.allocator);
    defer _ = arena.deinit();

    const program = try fs.cwd().readFileAlloc(&arena.allocator, "main.wasm", 0xFFFFFFF);

    var e = Engine.init();
    _ = try e.loadModule(program);
}
