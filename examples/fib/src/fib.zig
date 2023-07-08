const std = @import("std");
const zware = @import("zware");
const Store = zware.Store;
const Module = zware.Module;
const Instance = zware.Instance;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
var gpa = GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const bytes = @embedFile("fib.wasm");

    var store = Store.init(alloc);
    defer store.deinit();

    var module = Module.init(alloc, bytes);
    defer module.deinit();
    try module.decode();

    var instance = Instance.init(alloc, &store, module);
    try instance.instantiate();
    defer instance.deinit();

    const n = 39;
    var in = [1]u64{n};
    var out = [1]u64{0};

    try instance.invoke("fib", in[0..], out[0..], .{});

    const result: i32 = @bitCast(@as(u32, @truncate(out[0])));
    std.debug.print("fib({}) = {}\n", .{ n, result });
}
