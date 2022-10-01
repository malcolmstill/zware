const std = @import("std");
const zware = @import("zware");
const Store = zware.Store;
const Module = zware.Module;
const Instance = zware.Instance;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
var gpa = GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    defer _ = gpa.deinit();

    var arena = ArenaAllocator.init(gpa.allocator());
    defer _ = arena.deinit();

    const alloc = arena.allocator();

    const bytes = @embedFile("fib.wasm");

    var store = Store.init(alloc);

    var module = Module.init(alloc, bytes);
    try module.decode();

    var instance = Instance.init(alloc, &store, module);
    try instance.instantiate();

    const n = 39;
    var in = [1]u64{n};
    var out = [1]u64{0};
    try instance.invoke("fib", in[0..], out[0..], .{});
    std.debug.print("fib({}) = {}\n", .{ n, @bitCast(i32, @truncate(u32, out[0])) });
}
