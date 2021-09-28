const std = @import("std");
const foxwren = @import("main.zig");
const Module = foxwren.Module;
const Store = foxwren.Store;
const Instance = foxwren.Instance;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;
var gpa = GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    defer _ = gpa.deinit();

    var arena = ArenaAllocator.init(&gpa.allocator);
    defer _ = arena.deinit();

    const bytes = @embedFile("../test/fib.wasm");

    var store: Store = Store.init(&arena.allocator);

    var module = Module.init(&arena.allocator, bytes);
    try module.decode();

    var new_inst = Instance.init(&arena.allocator, &store, module);
    const index = try store.addInstance(new_inst);
    var inst = try store.instance(index);
    try inst.instantiate(index);

    const n = 6;
    var in = [1]u64{n};
    var out = [1]u64{0};
    try inst.invoke("fib", in[0..], out[0..], .{});
    std.debug.warn("fib({}) = {}\n", .{ n, @bitCast(i32, @truncate(u32, out[0])) });
}
