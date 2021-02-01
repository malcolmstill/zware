const std = @import("std");
const mem = std.mem;
const leb = std.debug.leb;

pub const Format = struct {
    module: []const u8 = undefined,
    offset: []const u8 = undefined,

    pub fn init(module: []const u8) Format {
        return Format{
            .module = module,
            .offset = module,
        };
    }

    pub fn readModule(self: *Format) !Module {
        if (!mem.eql(u8, self.module[0..4], "\x00asm")) return error.MagicNumberNotFound;
        self.offset = self.offset[4..];

        const version = mem.readInt(u32, @ptrCast(*const [@sizeOf(u32)]u8, &self.offset[0]), .Little);
        self.offset = self.offset[4..];

        return Module{
            .version = version,
        };
    }

    pub fn readSection(self: *Format) !Section {
        const id: SectionType = @intToEnum(SectionType, self.offset[0]);
        self.offset = self.offset[1..];

        var reader = std.io.fixedBufferStream(self.offset);
        const size = try leb.readULEB128(u32, reader.reader());
        self.offset = self.offset[4..];

        defer self.offset = self.offset[size..];

        return Section{
            .id = id,
            .size = size,
            .contents = self.offset[0..size],
        };
    }
};

const Module = struct {
    version: u32,
};

const Section = struct {
    id: SectionType,
    size: u32,
    contents: []const u8,
};

const SectionType = enum(u8) {
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
