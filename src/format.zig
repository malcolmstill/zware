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
            .functions = Functions.init(self.alloc),
            .tables = Tables.init(self.alloc),
            .memories = Memories.init(self.alloc),
            .globals = Globals.init(self.alloc),
            .exports = Exports.init(self.alloc),
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

        switch (id) {
            .Type => {
                const count = try self.readTypeSection(module);
                std.debug.warn("read {} FuncTypes\n", .{count});
            },
            .Import => {
                const count = try self.readImportSection(module);
                std.debug.warn("read {} Import\n", .{count});
            },
            .Function => {
                const count = try self.readFunctionSection(module);
                std.debug.warn("read {} Function\n", .{count});
            },
            .Table => {
                const count = try self.readTableSection(module);
                std.debug.warn("read {} Table\n", .{count});
            },
            .Memory => {
                const count = try self.readMemorySection(module);
                std.debug.warn("read {} Memory\n", .{count});
            },
            .Global => {
                const count = try self.readGlobalSection(module);
                std.debug.warn("read {} Global\n", .{count});
            },
            .Export => {
                const count = try self.readExportSection(module);
                std.debug.warn("read {} Export\n", .{count});
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

            const desc_tag = try rd.readEnum(DescTag, .Little);
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

    fn readFunctionSection(self: *Format, module: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const type_index = try leb.readULEB128(u32, rd);
            try module.functions.append(type_index);
        }

        return count;
    }

    fn readTableSection(self: *Format, module: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);

        const tag = try rd.readByte();
        if (tag != 0x70) return error.ExpectedTable;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const limit_type = try rd.readEnum(LimitType, .Little);
            switch (limit_type) {
                .Min => {
                    const min = try leb.readULEB128(u32, rd);

                    try module.tables.append(Limit{
                        .min = min,
                        .max = std.math.maxInt(u32),
                    });
                },
                .MinMax => {
                    const min = try leb.readULEB128(u32, rd);
                    const max = try leb.readULEB128(u32, rd);

                    try module.tables.append(Limit{
                        .min = min,
                        .max = max,
                    });
                },
            }
        }

        return count;
    }

    fn readMemorySection(self: *Format, module: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const limit_type = try rd.readEnum(LimitType, .Little);
            switch (limit_type) {
                .Min => {
                    const min = try leb.readULEB128(u32, rd);

                    try module.tables.append(Limit{
                        .min = min,
                        .max = std.math.maxInt(u32),
                    });
                },
                .MinMax => {
                    const min = try leb.readULEB128(u32, rd);
                    const max = try leb.readULEB128(u32, rd);

                    try module.tables.append(Limit{
                        .min = min,
                        .max = max,
                    });
                },
            }
        }

        return count;
    }

    fn readGlobalSection(self: *Format, module: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const global_type = try rd.readEnum(ValueType, .Little);
            const mutability = try rd.readEnum(Mutability, .Little);
            const offset = rd.context.pos;

            var j: usize = 0;
            while (true) : (j += 1) {
                const byte = try rd.readByte();
                if (byte == @enumToInt(Instructions.End)) break;
            }
            const code = self.module[offset .. offset + j + 1];

            try module.globals.append(Global{
                .value_type = global_type,
                .mutability = mutability,
                .code = code,
            });
        }

        return count;
    }

    fn readExportSection(self: *Format, module: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);
        std.debug.warn("export count: {}\n", .{count});

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const name_length = try leb.readULEB128(u32, rd);
            const name = self.module[rd.context.pos .. rd.context.pos + name_length];
            try rd.skipBytes(name_length, .{});
            std.debug.warn("string: {}\n", .{name});

            const tag = try rd.readEnum(DescTag, .Little);
            const index = try leb.readULEB128(u32, rd);

            try module.exports.append(Export{
                .name = name,
                .tag = tag,
                .index = index,
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
    functions: Functions,
    tables: Tables,
    memories: Memories,
    globals: Globals,
    exports: Exports,
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

const LimitType = enum(u8) {
    Min,
    MinMax,
};

const Limit = struct {
    min: u32,
    max: u32,
};

const Mutability = enum(u8) {
    Immutable,
    Mutable,
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
const Functions = ArrayList(u32); // We can't use view into data because values are leb
const Tables = ArrayList(Limit);
const Memories = ArrayList(Limit);
const Globals = ArrayList(Global);
const Exports = ArrayList(Export);
// const Starts = ArrayList(Start);
// const Codes = ArrayList(Code);
// const Datas = ArrayList(Data);

const FuncType = struct {
    params_offset: usize,
    params_count: usize,
    results_offset: usize,
    results_count: usize,
};

const Global = struct {
    value_type: ValueType,
    mutability: Mutability,
    code: []const u8,
};

const Import = struct {
    module: []const u8,
    name: []const u8,
    desc_tag: DescTag,
    desc: u8,
};

const Export = struct {
    name: []const u8,
    tag: DescTag,
    index: u32,
};

const DescTag = enum(u8) {
    Func,
    Table,
    Mem,
    Global,
};

const Instructions = enum(u8) {
    Unreachable = 0x0,
    Nop = 0x01,
    Block = 0x02, // bt
    Loop = 0x03, // bt
    If = 0x04, // bt
    Else = 0x05,
    End = 0x0b,
    Br = 0x0c,
    BrIf = 0x0d,
    BrTable = 0x0e,
    Return = 0x0f,
    Call = 0x10,
    CallIndirect = 0x11,
    Drop = 0x1a,
    Select = 0x1b,
    LocalGet = 0x20,
    LocalSet = 0x21,
    LocalTee = 0x22,
    GlobalGet = 0x23,
    GlobalSet = 0x24,
    I32Load = 0x28,
    I64Load = 0x29,
    F32Load = 0x2a,
    F64Load = 0x2b,
    I32Load8S = 0x2c,
    I32Load8U = 0x2d,
    I32Load16S = 0x2e,
    I32Load16U = 0x2f,
    I64Load8S = 0x30,
    I64Load8U = 0x31,
    I64Load16S = 0x32,
    I64Load16U = 0x33,
    I64Load32S = 0x34,
    I64Load32U = 0x35,
    I32Store = 0x36,
    I64Store = 0x37,
    F32Store = 0x38,
    F64Store = 0x39,
    I32Store8 = 0x3a,
    I32Store16 = 0x3b,
    I64Store8 = 0x3c,
    I64Store16 = 0x3d,
    I64Store32 = 0x3e,
    MemorySize = 0x3f,
    MemoryGrow = 0x40,
    I32Const = 0x41,
    I64Const = 0x42,
    F32Const = 0x43,
    F64Const = 0x44,
    I32Eqz = 0x45,
    I32Eq = 0x46,
    I32Ne = 0x47,
    I32LtS = 0x48,
    I32LtU = 0x49,
    I32GtS = 0x4a,
    I32GtU = 0x4b,
    I32LeS = 0x4c,
    I32LeU = 0x4d,
    I32GeS = 0x4e,
    I32GeU = 0x4f,
    I64Eqz = 0x50,
    I64Eq = 0x51,
    I64Ne = 0x52,
    I64LtS = 0x53,
    I64LtU = 0x54,
    I64GtS = 0x55,
    I64GtU = 0x56,
    I64LeS = 0x57,
    I64LeU = 0x58,
    I64GeS = 0x59,
    I64GeU = 0x5a,
    F32Eq = 0x5b,
    F32Ne = 0x5c,
    F32Lt = 0x5d,
    F32Gt = 0x5e,
    F32Le = 0x5f,
    F32Ge = 0x60,
    F64Eq = 0x61,
    F64Ne = 0x62,
    F64Lt = 0x63,
    F64Gt = 0x64,
    F64Le = 0x65,
    F64Ge = 0x66,
    I32Clz = 0x67,
    I32Ctz = 0x68,
    I32Popcnt = 0x69,
    I32Add = 0x6a,
    I32Sub = 0x6b,
    I32Mul = 0x6c,
    I32DivS = 0x6d,
    I32DivU = 0x6e,
    I32RemS = 0x6f,
    I32RemU = 0x70,
    I32And = 0x71,
    I32Or = 0x72,
    I32Xor = 0x73,
    I32Shl = 0x74,
    I32ShrS = 0x75,
    I32ShrU = 0x76,
    I32Rotl = 0x77,
    I32Rotr = 0x78,
    I64Clz = 0x79,
    I64Ctz = 0x7a,
    I64Popcnt = 0x7b,
    I64Add = 0x7c,
    I64Sub = 0x7d,
    I64Mul = 0x7e,
    I64DivS = 0x7f,
    I64DivU = 0x80,
    I64RemS = 0x81,
    I64RemU = 0x82,
    I64And = 0x83,
    I64Or = 0x84,
    I64Xor = 0x85,
    I64Shl = 0x86,
    I64ShrS = 0x87,
    I64ShrU = 0x88,
    I64Rotl = 0x89,
    I64Rotr = 0x8a,
    F32Abs = 0x8b,
    F32Neg = 0x8c,
    F32Ceil = 0x8d,
    F32Floor = 0x8e,
    F32Trunc = 0x8f,
    F32Nearest = 0x90,
    F32Sqrt = 0x91,
    F32Add = 0x92,
    F32Sub = 0x93,
    F32Mul = 0x94,
    F32Div = 0x95,
    F32Min = 0x96,
    F32Max = 0x97,
    F32CopySign = 0x98,
    F64Abs = 0x99,
    F64Neg = 0x9a,
    F64Ceil = 0x9b,
    F64Floor = 0x9c,
    F64Trunc = 0x9d,
    F64Nearest = 0x9e,
    F64Sqrt = 0x9f,
    F64Add = 0xa0,
    F64Sub = 0xa1,
    F64Mul = 0xa2,
    F64Div = 0xa3,
    F64Min = 0xa4,
    F64Max = 0xa5,
    F64CopySign = 0xa6,
    I32WrapI64 = 0xa7,
    I32TruncF32S = 0xa8,
    I32TruncF32U = 0xa9,
    I32TruncF64S = 0xaa,
    I32TruncF64U = 0xab,
    I64ExtendI32S = 0xac,
    I64ExtendI32U = 0xad,
    I64TruncF32S = 0xae,
    I64TruncF32U = 0xaf,
    I64TruncF64S = 0xb0,
    I64TruncF64U = 0xb1,
    F32ConvertI32S = 0xb2,
    F32ConvertI32U = 0xb3,
    F32ConvertI64S = 0xb4,
    F32ConvertI64U = 0xb5,
    F32DemoteF64 = 0xb6,
    F64ConvertI32S = 0xb7,
    F64ConvertI32U = 0xb8,
    F64ConvertI64S = 0xb9,
    F64ConvertI64U = 0xba,
    F64PromoteF32 = 0xbb,
    I32ReinterpretF32 = 0xbc,
    I64ReinterpretF64 = 0xbd,
    F32ReinterpretI32 = 0xbe,
    F64ReinterpretI64 = 0xbf,
    I32Extend8S = 0xc0,
    I32Extend16S = 0xc1,
    I64Extend8S = 0xc2,
    I64Extend16S = 0xc3,
    I64Extend32S = 0xc4,
    // reserved
    // I32TruncSatF32S = 0xfc,
};
