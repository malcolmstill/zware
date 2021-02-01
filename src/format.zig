const std = @import("std");
const mem = std.mem;
const leb = std.debug.leb;
const ArrayList = std.ArrayList;

pub const Format = struct {
    alloc: *mem.Allocator,
    module: []const u8 = undefined,
    buf: std.io.FixedBufferStream([]const u8) = undefined,

    pub fn init(alloc: *mem.Allocator, module: []const u8) Format {
        return Format{
            .alloc = alloc,
            .module = module,
            .buf = std.io.fixedBufferStream(module),
        };
    }

    pub fn readModule(self: *Format) !Module {
        const rd = self.buf.reader();

        const magic = try rd.readBytesNoEof(4);
        if (!mem.eql(u8, magic[0..], "\x00asm")) return error.MagicNumberNotFound;

        const version = try rd.readIntLittle(u32);

        return Module{
            .version = version,
            // .customs = Customs.init(self.alloc),
            .types = Types.init(self.alloc),
            .value_types = ValueTypes.init(self.alloc),
            .imports = Imports.init(self.alloc),
            // .functions = Functions.init(self.alloc),
            // .tables = Tables.init(self.alloc),
            // .memories = Memories.init(self.alloc),
            // .globals = Globals.init(self.alloc),
            // .exports = Exports.init(self.alloc),
            // .starts = Starts.init(self.alloc),
            // .elements = Elements.init(self.alloc),
            // .codes = Codes.init(self.alloc),
            // .datas = Datas.init(self.alloc),
        };
    }

    pub fn readSection(self: *Format, module: *Module) !SectionType {
        const rd = self.buf.reader();
        std.debug.warn("pos: {}, byte: {x}\n", .{ rd.context.pos, self.module[rd.context.pos] });
        const id: SectionType = @intToEnum(SectionType, try rd.readByte());

        const size = try leb.readULEB128(u32, rd);
        std.debug.warn("section bytes: {}, next byte {}\n", .{ size, self.module[rd.context.pos] });

        const offset = rd.context.pos;
        // const section_data = self.module[offset .. offset + size];

        switch (id) {
            .Type => {
                const count = try self.readTypeSection(module);
                std.debug.warn("read {} FuncTypes\n", .{count});
            },
            .Import => {
                const count = try self.readImportSection(module);
                std.debug.warn("read {} Import\n", .{count});
            },
            else => {},
        }

        return id;
    }

    fn readTypeSection(self: *Format, module: *Module) !usize {
        const rd = self.buf.reader();
        const functype_count = try leb.readULEB128(u32, rd);

        var f: usize = 0;
        while (f < functype_count) : (f += 1) {
            const tag: u8 = try rd.readByte();
            if (tag != 0x60) return error.ExpectedFuncTypeTag;

            const param_count = try leb.readULEB128(u32, rd);
            const param_offset = module.value_types.items.len;

            {
                var i: usize = 0;
                while (i < param_count) : (i += 1) {
                    const v = try rd.readEnum(ValueType, .Little);
                    try module.value_types.append(v);
                }
            }

            const results_count = try leb.readULEB128(u32, rd);
            const results_offset = module.value_types.items.len;

            {
                var i: usize = 0;
                while (i < results_count) : (i += 1) {
                    const r = try rd.readEnum(ValueType, .Little);
                    try module.value_types.append(r);
                }
            }

            // TODO: rather than count we could store the first element / last element + 1
            // TODO: should we just index into the existing module data?
            try module.types.append(FuncType{
                .params_offset = param_offset,
                .params_count = param_count,
                .results_offset = results_offset,
                .results_count = results_count,
            });
        }

        return functype_count;
    }

    fn readImportSection(self: *Format, module: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);
        std.debug.warn("import count: {}\n", .{count});

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const module_name_length = try leb.readULEB128(u32, rd);
            const module_name = self.module[rd.context.pos .. rd.context.pos + module_name_length];
            try rd.skipBytes(module_name_length, .{});
            std.debug.warn("string: {}\n", .{module_name});

            const name_length = try leb.readULEB128(u32, rd);
            const name = self.module[rd.context.pos .. rd.context.pos + name_length];
            try rd.skipBytes(name_length, .{});
            std.debug.warn("string: {}\n", .{name});

            const desc_tag = try rd.readByte();
            const desc = try rd.readByte(); // TODO: not sure if this is a byte or leb

            try module.imports.append(Import{
                .module = module_name,
                .name = name,
                .desc_tag = desc_tag,
                .desc = desc,
            });
        }

        return count;
    }
};

const Module = struct {
    version: u32,
    // customs: Customs,
    types: Types,
    value_types: ValueTypes,
    imports: Imports,
    // functions: Functions,
    // tables: Tables,
    // memories: Memories,
    // globals: Globals,
    // exports: Exports,
    // starts: Starts,
    // elements: Elements,
    // codes: Codes,
    // datas: Datas,
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

const ValueType = enum(u8) {
    I32 = 0x7F,
    I64 = 0x7E,
    F32 = 0x7D,
    F64 = 0x7C,
};

// TODO: should we define these?
// const Customs = ArrayList(Custom);
const Types = ArrayList(FuncType);
const ValueTypes = ArrayList(ValueType);
const Imports = ArrayList(Import);
// const Functions = ArrayList(Functions);
// const Tables = ArrayList(Table);
// const Memories = ArrayList(Memory);
// const Globals = ArrayList(Global);
// const Exports = ArrayList(Export);
// const Starts = ArrayList(Start);
// const Codes = ArrayList(Code);
// const Datas = ArrayList(Data);

const FuncType = struct {
    params_offset: usize,
    params_count: usize,
    results_offset: usize,
    results_count: usize,
    // params: []const u8,
    // results: []const u8,
};

const Import = struct {
    module: []const u8, name: []const u8, desc_tag: u8, desc: u8
};
