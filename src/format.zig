const std = @import("std");
const mem = std.mem;

pub const Format = struct {
    module: []const u8 = undefined,
    offset: usize = 0,

    pub fn init(module: []const u8) Format {
        return Format{
            .module = module,
        };
    }

    pub fn readModule(self: *Format) !Module {
        if (!mem.eql(u8, self.module[self.offset .. self.offset + 4], "\x00asm")) return error.MagicNumberNotFound;
        self.offset += 4;
        const version = mem.readInt(u32, @ptrCast(*const [@sizeOf(u32)]u8, &self.module[self.offset]), .Little);
        std.log.info("version: {}\n", .{version});

        return Module{
            .version = version,
        };
    }
};

const Module = packed struct {
    version: u32,
};
