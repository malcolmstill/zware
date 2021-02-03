const std = @import("std");
const mem = std.mem;
const process = std.process;
const fs = std.fs;
const Module = @import("module.zig").Module;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;

var gpa = GeneralPurposeAllocator(.{}){};

pub fn main() anyerror!void {
    defer _ = gpa.deinit();
    std.log.info("Booting wasm runtime...", .{});

    var arena = ArenaAllocator.init(&gpa.allocator);
    defer _ = arena.deinit();

    const program = try fs.cwd().readFileAlloc(&arena.allocator, "export.wasm", 0xFFFFFFF);

    // var e = Engine.init();
    // var module = try e.loadModule(&arena.allocator, program);
    var module = Module.init(&arena.allocator, program);
    try module.parse();

    std.debug.warn("Functypes: {}\n", .{module.types.items.len});
    std.debug.warn("Functions: {}\n", .{module.functions.items.len});
    std.debug.warn("Tables: {}\n", .{module.tables.items.len});
    std.debug.warn("Memories: {}\n", .{module.memories.items.len});
    std.debug.warn("Globals: {}\n", .{module.globals.items.len});
    std.debug.warn("Exports: {}\n", .{module.exports.items.len});
    std.debug.warn("Imports: {}\n", .{module.imports.items.len});
    std.debug.warn("Codes: {}\n", .{module.codes.items.len});
    std.debug.warn("Datas: {}\n", .{module.datas.items.len});
    std.debug.warn("Customs: {}\n", .{module.customs.items.len});

    const idx = try module.getExport(.Func, "add");
    std.debug.warn("func index: {}\n", .{idx});

    const add = try module.getFunction("add", .{ i32, i32 }, i32);
    // std.debug.warn("typeof add: {}\n", .{@typeInfo(@TypeOf(add))});
    // const r = add(0, 0);
    // const r = add.call(.{ 0, 0 });
    // const r = @call(.{}, add, .{ 43, 34 });
    // const module.wasmFuncall(add, .{0, 0});
}
