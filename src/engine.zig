const std = @import("std");
const mem = std.mem;
const Format = @import("format.zig").Format;

pub const Engine = struct {
    exe: []const u8 = undefined,

    const Self = @This();

    pub fn init() Self {
        return Self{};
    }

    pub fn loadModule(self: *Self, module: []const u8) !void {
        var buffer = Format.init(module);
        var header = try buffer.readModule();
    }

    pub fn getFunction(self: *Self, function_name: []const u8) usize {}
};

const Sections = enum(u8) {
    Custom,
    Type,
    Import,
    Function,
    Table,
    Memory,
    Global,
    Export,
    Start,
    Element,
    Code,
    Data,
};
