const std = @import("std");
const mem = std.mem;
const leb = std.debug.leb;

pub const Format = struct {
    module: []const u8 = undefined,
    buf: std.io.FixedBufferStream([]const u8) = undefined,

    pub fn init(module: []const u8) Format {
        return Format{
            .module = module,
            .buf = std.io.fixedBufferStream(module),
        };
    }

    pub fn readModule(self: *Format) !Module {
        const rd = self.buf.reader();

        const magic = try rd.readBytesNoEof(4);
        if (!mem.eql(u8, magic[0..], "\x00asm")) return error.MagicNumberNotFound;

        const version = try rd.readIntBig(u32);

        return Module{
            .version = version,
        };
    }

    pub fn readSection(self: *Format) !Section {
        const rd = self.buf.reader();
        const id: SectionType = @intToEnum(SectionType, try rd.readByte());

        const size = try leb.readULEB128(u32, rd);

        try rd.skipBytes(size, .{});
        const offset = rd.context.pos;

        return Section{
            .id = id,
            .size = size,
            .contents = self.module[offset .. offset + size],
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
