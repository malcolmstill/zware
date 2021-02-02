const std = @import("std");
const mem = std.mem;
const Format = @import("format.zig").Format;
const Module = @import("format.zig").Module;

pub const Engine = struct {
    exe: []const u8 = undefined,

    const Self = @This();

    pub fn init() Self {
        return Self{};
    }

    pub fn loadModule(self: *Self, alloc: *mem.Allocator, data: []const u8) !Module {
        var buffer = Format.init(alloc, data);
        var module = try buffer.readModule();

        var i: usize = 0;
        while (true) : (i += 1) {
            var section = buffer.readSection(&module) catch |err| {
                switch (err) {
                    error.EndOfStream => break,
                    else => return err,
                }
            };
        }

        return module;
    }

    pub fn getFunction(self: *Self, function_name: []const u8) usize {}
};
