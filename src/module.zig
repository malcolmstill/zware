const std = @import("std");
const mem = std.mem;
const leb = std.debug.leb;
const ArrayList = std.ArrayList;
const Instruction = @import("instruction.zig").Instruction;

pub const Module = struct {
    alloc: *mem.Allocator,
    module: []const u8 = undefined,
    buf: std.io.FixedBufferStream([]const u8) = undefined,
    version: u32 = 0,
    customs: ArrayList(Custom),
    types: ArrayList(FuncType),
    value_types: ArrayList(ValueType),
    imports: ArrayList(Import),
    functions: ArrayList(u32),
    tables: ArrayList(Limit),
    memories: ArrayList(Limit),
    globals: ArrayList(Global),
    exports: ArrayList(Export),
    start: u32,
    // elements: Elements,
    codes: ArrayList(Code),
    datas: ArrayList(Data),

    pub fn init(alloc: *mem.Allocator, module: []const u8) Module {
        return Module{
            .alloc = alloc,
            .module = module,
            .buf = std.io.fixedBufferStream(module),
            .customs = ArrayList(Custom).init(alloc),
            .types = ArrayList(FuncType).init(alloc),
            .value_types = ArrayList(ValueType).init(alloc),
            .imports = ArrayList(Import).init(alloc),
            .functions = ArrayList(u32).init(alloc),
            .tables = ArrayList(Limit).init(alloc),
            .memories = ArrayList(Limit).init(alloc),
            .globals = ArrayList(Global).init(alloc),
            .exports = ArrayList(Export).init(alloc),
            .start = undefined,
            // .elements = Elements.init(alloc),
            .codes = ArrayList(Code).init(alloc),
            .datas = ArrayList(Data).init(alloc),
        };
    }

    pub fn parse(self: *Module) !void {
        const rd = self.buf.reader();

        const magic = try rd.readBytesNoEof(4);
        if (!mem.eql(u8, magic[0..], "\x00asm")) return error.MagicNumberNotFound;

        const version = try rd.readIntLittle(u32);

        var i: usize = 0;
        while (true) : (i += 1) {
            var section = self.parseSection() catch |err| {
                switch (err) {
                    error.EndOfStream => break,
                    else => return err,
                }
            };
        }
    }

    pub fn parseSection(self: *Module) !SectionType {
        const rd = self.buf.reader();
        const id: SectionType = @intToEnum(SectionType, try rd.readByte());

        const size = try leb.readULEB128(u32, rd);

        _ = switch (id) {
            .Custom => try self.parseCustomSection(size),
            .Type => try self.parseTypeSection(),
            .Import => try self.parseImportSection(),
            .Function => try self.parseFunctionSection(),
            .Table => try self.parseTableSection(),
            .Memory => try self.parseMemorySection(),
            .Global => try self.parseGlobalSection(),
            .Export => try self.parseExportSection(),
            .Start => try self.parseStartSection(),
            .Code => try self.parseCodeSection(),
            .Data => try self.parseDataSection(size),
        };

        return id;
    }

    fn parseTypeSection(self: *Module) !usize {
        const rd = self.buf.reader();
        const functype_count = try leb.readULEB128(u32, rd);

        var f: usize = 0;
        while (f < functype_count) : (f += 1) {
            const tag: u8 = try rd.readByte();
            if (tag != 0x60) return error.ExpectedFuncTypeTag;

            const param_count = try leb.readULEB128(u32, rd);
            const param_offset = self.value_types.items.len;

            {
                var i: usize = 0;
                while (i < param_count) : (i += 1) {
                    const v = try rd.readEnum(ValueType, .Little);
                    try self.value_types.append(v);
                }
            }

            const results_count = try leb.readULEB128(u32, rd);
            const results_offset = self.value_types.items.len;

            {
                var i: usize = 0;
                while (i < results_count) : (i += 1) {
                    const r = try rd.readEnum(ValueType, .Little);
                    try self.value_types.append(r);
                }
            }

            // TODO: rather than count we could store the first element / last element + 1
            // TODO: should we just index into the existing module data?
            try self.types.append(FuncType{
                .params_offset = param_offset,
                .params_count = param_count,
                .results_offset = results_offset,
                .results_count = results_count,
            });
        }

        return functype_count;
    }

    fn parseImportSection(self: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const module_name_length = try leb.readULEB128(u32, rd);
            const module_name = self.module[rd.context.pos .. rd.context.pos + module_name_length];
            try rd.skipBytes(module_name_length, .{});

            const name_length = try leb.readULEB128(u32, rd);
            const name = self.module[rd.context.pos .. rd.context.pos + name_length];
            try rd.skipBytes(name_length, .{});

            const desc_tag = try rd.readEnum(Tag, .Little);
            const desc = try rd.readByte(); // TODO: not sure if this is a byte or leb

            try self.imports.append(Import{
                .module = module_name,
                .name = name,
                .desc_tag = desc_tag,
                .desc = desc,
            });
        }

        return count;
    }

    fn parseFunctionSection(self: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const type_index = try leb.readULEB128(u32, rd);
            try self.functions.append(type_index);
        }

        return count;
    }

    fn parseTableSection(self: *Module) !usize {
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

                    try self.tables.append(Limit{
                        .min = min,
                        .max = std.math.maxInt(u32),
                    });
                },
                .MinMax => {
                    const min = try leb.readULEB128(u32, rd);
                    const max = try leb.readULEB128(u32, rd);

                    try self.tables.append(Limit{
                        .min = min,
                        .max = max,
                    });
                },
            }
        }

        return count;
    }

    fn parseMemorySection(self: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const limit_type = try rd.readEnum(LimitType, .Little);
            switch (limit_type) {
                .Min => {
                    const min = try leb.readULEB128(u32, rd);

                    try self.memories.append(Limit{
                        .min = min,
                        .max = std.math.maxInt(u32),
                    });
                },
                .MinMax => {
                    const min = try leb.readULEB128(u32, rd);
                    const max = try leb.readULEB128(u32, rd);

                    try self.memories.append(Limit{
                        .min = min,
                        .max = max,
                    });
                },
            }
        }

        return count;
    }

    fn parseGlobalSection(self: *Module) !usize {
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
                if (byte == @enumToInt(Instruction.End)) break;
            }
            const code = self.module[offset .. offset + j + 1];

            try self.globals.append(Global{
                .value_type = global_type,
                .mutability = mutability,
                .code = code,
            });
        }

        return count;
    }

    fn parseExportSection(self: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const name_length = try leb.readULEB128(u32, rd);
            const name = self.module[rd.context.pos .. rd.context.pos + name_length];
            try rd.skipBytes(name_length, .{});

            const tag = try rd.readEnum(Tag, .Little);
            const index = try leb.readULEB128(u32, rd);

            try self.exports.append(Export{
                .name = name,
                .tag = tag,
                .index = index,
            });
        }

        return count;
    }

    fn parseStartSection(self: *Module) !usize {
        const rd = self.buf.reader();
        const index = try leb.readULEB128(u32, rd);

        self.start = index;

        return 1;
    }

    fn parseCodeSection(self: *Module) !usize {
        const rd = self.buf.reader();
        var x = rd.context.pos;
        const count = try leb.readULEB128(u32, rd);

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const size = try leb.readULEB128(u32, rd); // includes bytes defining locals
            const offset = rd.context.pos;
            try rd.skipBytes(size, .{});

            // TODO: run some verification on the code
            //      e.g. check last value is End instruction
            const code = self.module[offset..rd.context.pos];

            try self.codes.append(Code{
                .code = code,
            });
        }

        return count;
    }

    fn parseDataSection(self: *Module, size: u32) !usize {
        const rd = self.buf.reader();
        var section_offset = rd.context.pos;
        const count = try leb.readULEB128(u32, rd);

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const mem_idx = try leb.readULEB128(u32, rd);
            const instr = try rd.readEnum(Instruction, .Little);
            const mem_offset = try leb.readULEB128(u32, rd);
            const data_length = try leb.readULEB128(u32, rd);

            const offset = rd.context.pos;
            try rd.skipBytes(data_length, .{});
            const data = self.module[offset..rd.context.pos];

            try self.datas.append(Data{
                .mem_idx = mem_idx,
                .instr = instr,
                .mem_offset = mem_offset,
                .data = data,
            });
        }

        const remaining_data = size - (rd.context.pos - section_offset);
        try rd.skipBytes(remaining_data, .{});

        return count;
    }

    fn parseCustomSection(self: *Module, size: u32) !usize {
        const rd = self.buf.reader();
        const offset = rd.context.pos;

        const name_length = try leb.readULEB128(u32, rd);
        const name = self.module[rd.context.pos .. rd.context.pos + name_length];
        try rd.skipBytes(name_length, .{});

        const remaining_size = size - (rd.context.pos - offset);

        const data = self.module[rd.context.pos .. rd.context.pos + remaining_size];
        try rd.skipBytes(remaining_size, .{});

        try self.customs.append(Custom{
            .name = name,
            .data = data,
        });

        return 1;
    }

    pub fn getExport(self: *Module, tag: Tag, name: []const u8) !usize {
        for (self.exports.items) |exported| {
            if (tag == exported.tag and mem.eql(u8, name, exported.name)) return exported.index;
        }

        return error.ExportNotFound;
    }

    pub fn getFunction(self: *Module, comptime len: usize, name: []const u8, comptime args: [len]ValueType, comptime result: ValueType) !usize {
        const index = try self.getExport(.Func, name);
        if (index >= self.types.items.len) return error.FuncIndexExceedsTypesLength;

        const func_type = self.types.items[index];
        const params = self.value_types.items[func_type.params_offset .. func_type.params_offset + func_type.params_count];
        const results = self.value_types.items[func_type.results_offset .. func_type.results_offset + func_type.results_count];

        if (params.len != args.len) return error.ParamCountMismatch;

        for (args) |arg, i| {
            if (arg != params[i]) return error.ParamTypeMismatch;
        }

        if (results.len > 1) return error.OnlySingleReturnValueSupported;
        if (results.len == 1) {
            if (results[0] != result) return error.ResultTypeMismatch;
        }

        return index;
    }

    pub fn print(module: *Module) void {
        std.debug.warn("Functypes: {}\n", .{module.types.items.len});
        std.debug.warn("Functions: {}\n", .{module.functions.items.len});
        std.debug.warn("Tables: {}\n", .{module.tables.items.len});
        std.debug.warn("Memories: {}\n", .{module.memories.items.len});
        std.debug.warn("Globals: {}\n", .{module.globals.items.len});
        std.debug.warn("Exports: {}\n", .{module.exports.items.len});
        std.debug.warn("Imports: {}\n", .{module.imports.items.len});
        std.debug.warn("Codes: {}\n", .{module.codes.items.len});
        std.debug.warn("Datas: {}\n", .{module.datas.items.len});
        std.debug.warn("Customs: {}\n", .{module.customs.items.len});
    }
};

fn WasmFunctionType(args: anytype, result: anytype) type {
    var fn_args: [65536]std.builtin.TypeInfo.FnArg = undefined;
    if (@typeInfo(@TypeOf(args)) != .Struct) {
        @compileError("Expected tuple or struct argument, found " ++ @typeName(@TypeOf(args)));
    }

    if (!(result == i32 or result == i64 or result == f32 or result == f64)) {
        @compileError("Return type must be in ValueType, found " ++ @typeName(result));
    }

    for (args) |arg, i| {
        if (!(arg == i32 or arg == i64 or arg == f32 or arg == f64)) {
            @compileError("Arg type must be in ValueType, found " ++ @typeName(arg));
        }
        fn_args[i] = std.builtin.TypeInfo.FnArg{
            .is_generic = false,
            .is_noalias = false,
            .arg_type = arg,
        };
    }

    return @Type(std.builtin.TypeInfo{
        .Fn = std.builtin.TypeInfo.Fn{
            .calling_convention = .Unspecified,
            .alignment = 0,
            .is_generic = false,
            .is_var_args = false,
            .return_type = result,
            .args = fn_args[0..args.len],
        },
    });
}

const Section = struct {
    id: SectionType,
    size: u32,
    contents: []const u8,
};

const Custom = struct {
    name: []const u8,
    data: []const u8,
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
    Custom = 0x00,
    Type = 0x01,
    Import = 0x02,
    Function = 0x03,
    Table = 0x04,
    Memory = 0x05,
    Global = 0x06,
    Export = 0x07,
    Start = 0x08,
    // Element = 0x09,
    Code = 0x0a,
    Data = 0x0b,
};

pub const ValueType = enum(u8) {
    I32 = 0x7F,
    I64 = 0x7E,
    F32 = 0x7D,
    F64 = 0x7C,
};

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
    desc_tag: Tag,
    desc: u8,
};

const Export = struct {
    name: []const u8,
    tag: Tag,
    index: u32,
};

const Code = struct {
    code: []const u8,
};

const Data = struct {
    mem_idx: u32,
    instr: Instruction,
    mem_offset: u32,
    data: []const u8,
};

const Tag = enum(u8) {
    Func,
    Table,
    Mem,
    Global,
};
