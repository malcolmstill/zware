const std = @import("std");
const mem = std.mem;
const fs = std.fs;
const fmt = std.fmt;
const process = std.process;
const zware = @import("zware");
const ArrayList = std.ArrayList;
const Module = zware.Module;
const Store = zware.Store;
const Instance = zware.Instance;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;
var gpa = GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    var args = process.args();
    _ = args.skip();
    const filename = args.next() orelse return error.NoFilename;

    defer _ = gpa.deinit();

    var arena = ArenaAllocator.init(gpa.allocator());
    defer _ = arena.deinit();
    const alloc = arena.allocator();

    const program = try fs.cwd().readFileAlloc(alloc, filename, 0xFFFFFFF);

    var store: Store = Store.init(alloc);

    var module = Module.init(alloc, program);
    try module.decode();

    var instance = Instance.init(alloc, &store, module);
    try instance.instantiate();
}
