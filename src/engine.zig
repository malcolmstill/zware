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
        std.log.info("header: {}\n", .{header});

        var first_section = try buffer.readSection();
        std.log.info("section[0]: {}\n", .{first_section});

        var second_section = try buffer.readSection();
        std.log.info("section[1]: {}\n", .{second_section});

        var third_section = try buffer.readSection();
        std.log.info("section[2]: {}\n", .{third_section});

        var fourth_section = try buffer.readSection();
        std.log.info("section[3]: {}\n", .{fourth_section});

        var fifth_section = try buffer.readSection();
        std.log.info("section[4]: {}\n", .{fifth_section});
    }

    pub fn getFunction(self: *Self, function_name: []const u8) usize {}
};
