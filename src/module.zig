const std = @import("std");
const mem = std.mem;
const leb = std.leb;
const math = std.math;
const unicode = std.unicode;
const common = @import("common.zig");
const instruction = @import("instruction.zig");
const Instance = @import("instance.zig").Instance;
const ArrayList = std.ArrayList;
const Instruction = instruction.Instruction;
const FuncType = common.FuncType;
const ValueType = common.ValueType;
const Import = common.Import;
const Export = common.Export;
const Limit = common.Limit;
const LimitType = common.LimitType;
const Mutability = common.Mutability;
const Function = common.Function;
const Global = common.Global;
const Element = common.Element;
const Code = common.Code;
const Segment = common.Segment;
const Tag = common.Tag;
const Interpreter = @import("interpreter.zig").Interpreter;
const Store = @import("store.zig").ArrayListStore;

pub const Module = struct {
    decoded: bool = false,
    alloc: *mem.Allocator,
    module: []const u8 = undefined,
    buf: std.io.FixedBufferStream([]const u8) = undefined,
    version: u32 = 0,
    customs: Section(Custom),
    types: Section(FuncType),
    value_types: Section(ValueType), // Not really a section
    imports: Section(Import),
    functions: Section(Function),
    tables: Section(Limit),
    memories: Section(Limit),
    globals: Section(Global),
    exports: Section(Export),
    start: ?u32,
    elements: Section(Segment),
    codes: Section(Code),
    datas: Section(Segment),

    pub fn init(alloc: *mem.Allocator, module: []const u8) Module {
        return Module{
            .alloc = alloc,
            .module = module,
            .buf = std.io.fixedBufferStream(module),
            .customs = Section(Custom).init(alloc),
            .types = Section(FuncType).init(alloc),
            .value_types = Section(ValueType).init(alloc),
            .imports = Section(Import).init(alloc),
            .functions = Section(Function).init(alloc),
            .tables = Section(Limit).init(alloc),
            .memories = Section(Limit).init(alloc),
            .globals = Section(Global).init(alloc),
            .exports = Section(Export).init(alloc),
            .start = null,
            .elements = Section(Segment).init(alloc),
            .codes = Section(Code).init(alloc),
            .datas = Section(Segment).init(alloc),
        };
    }

    pub fn decode(self: *Module) !void {
        const rd = self.buf.reader();

        const magic = try rd.readBytesNoEof(4);
        if (!mem.eql(u8, magic[0..], "\x00asm")) return error.MagicNumberNotFound;

        const version = try rd.readIntLittle(u32);
        if (version != 1) return error.UnknownBinaryVersion;

        var i: usize = 0;
        while (true) : (i += 1) {
            var section = self.decodeSection() catch |err| {
                switch (err) {
                    error.EndOfStream => {
                        break;
                    },
                    else => return err,
                }
            };
            try self.verify();
        }
        try self.verify();
        if (self.codes.list.items.len != self.functions.list.items.len) return error.FunctionCodeSectionsInconsistent;
        self.decoded = true;
    }

    pub fn verify(self: *Module) !void {
        if (self.types.count != self.types.list.items.len) return error.TypeCountMismatch;
        if (self.imports.count != self.imports.list.items.len) return error.ImportsCountMismatch;
        if (self.functions.count != nonImportCount(self.functions.list.items)) return error.FunctionsCountMismatch;
        if (self.tables.count != nonImportCount(self.tables.list.items)) return error.TablesCountMismatch;
        if (self.memories.count != nonImportCount(self.memories.list.items)) return error.MemoriesCountMismatch;
        if (self.globals.count != nonImportCount(self.globals.list.items)) return error.GlobalsCountMismatch;
        if (self.exports.count != self.exports.list.items.len) return error.ExportsCountMismatch;
        if (self.elements.count != self.elements.list.items.len) return error.ElementsCountMismatch;
        if (self.codes.count != self.codes.list.items.len) return error.CodesCountMismatch;
        if (self.datas.count != self.datas.list.items.len) return error.DatasCountMismatch;
    }

    // Some types can be imported. For validation we want to check the non-imported
    // counts of each type match the what is stated in the binary. This function
    // counts the non-imports.
    fn nonImportCount(imported_type: anytype) usize {
        var count: usize = 0;
        for (imported_type) |import| {
            if (import.import == null) count += 1;
        }
        return count;
    }

    pub fn decodeSection(self: *Module) !SectionType {
        const rd = self.buf.reader();

        const id: SectionType = rd.readEnum(SectionType, .Little) catch |err| switch (err) {
            error.InvalidValue => return error.UnknownSectionId,
            else => return err,
        };

        const size = try leb.readULEB128(u32, rd);

        _ = switch (id) {
            .Custom => try self.decodeCustomSection(size),
            .Type => try self.decodeTypeSection(),
            .Import => try self.decodeImportSection(),
            .Function => try self.decodeFunctionSection(),
            .Table => try self.decodeTableSection(),
            .Memory => try self.decodeMemorySection(),
            .Global => try self.decodeGlobalSection(),
            .Export => try self.decodeExportSection(),
            .Start => try self.decodeStartSection(),
            .Element => try self.decodeElementSection(size),
            .Code => try self.decodeCodeSection(),
            .Data => try self.decodeDataSection(size),
        };

        return id;
    }

    fn decodeTypeSection(self: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);
        self.types.count = count;

        var f: usize = 0;
        while (f < count) : (f += 1) {
            const tag: u8 = try rd.readByte();
            if (tag != 0x60) return error.ExpectedFuncTypeTag;

            const param_count = try leb.readULEB128(u32, rd);
            const param_offset = self.value_types.list.items.len;

            {
                var i: usize = 0;
                while (i < param_count) : (i += 1) {
                    const v = try rd.readEnum(ValueType, .Little);
                    try self.value_types.list.append(v);
                }
            }

            const results_count = try leb.readULEB128(u32, rd);
            const results_offset = self.value_types.list.items.len;

            {
                var i: usize = 0;
                while (i < results_count) : (i += 1) {
                    const r = try rd.readEnum(ValueType, .Little);
                    try self.value_types.list.append(r);
                }
            }

            // TODO: rather than count we could store the first element / last element + 1
            // TODO: should we just index into the existing module data?
            try self.types.list.append(FuncType{
                .params_offset = param_offset,
                .params_count = param_count,
                .results_offset = results_offset,
                .results_count = results_count,
            });
        }

        return count;
    }

    fn decodeImportSection(self: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);
        self.imports.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const module_name_length = try leb.readULEB128(u32, rd);
            const module_name = self.module[rd.context.pos .. rd.context.pos + module_name_length];
            try rd.skipBytes(module_name_length, .{});

            const name_length = try leb.readULEB128(u32, rd);
            const name = self.module[rd.context.pos .. rd.context.pos + name_length];
            try rd.skipBytes(name_length, .{});

            const desc_tag = try rd.readEnum(Tag, .Little);

            if (i > math.maxInt(u32)) return error.ExpectedU32Index;
            const import_index = @truncate(u32, i);
            _ = switch (desc_tag) {
                .Func => try self.decodeFunction(import_index),
                .Table => try self.decodeTable(import_index),
                .Mem => try self.decodeMemory(import_index),
                .Global => try self.decodeGlobal(import_index),
            };

            try self.imports.list.append(Import{
                .module = module_name,
                .name = name,
                .desc_tag = desc_tag,
            });
        }

        return count;
    }

    fn decodeFunctionSection(self: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);
        self.functions.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            try self.decodeFunction(null);
        }

        return count;
    }

    fn decodeFunction(self: *Module, import: ?u32) !void {
        const rd = self.buf.reader();
        const type_index = try leb.readULEB128(u32, rd);
        try self.functions.list.append(Function{
            .typeidx = type_index,
            .import = import,
        });
    }

    fn decodeTableSection(self: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);
        self.tables.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            try self.decodeTable(null);
        }

        return count;
    }

    fn decodeTable(self: *Module, import: ?u32) !void {
        const rd = self.buf.reader();

        const tag = try rd.readByte();
        if (tag != 0x70) return error.ExpectedTable;
        const limit_type = try rd.readEnum(LimitType, .Little);
        switch (limit_type) {
            .Min => {
                const min = try leb.readULEB128(u32, rd);

                try self.tables.list.append(Limit{
                    .min = min,
                    .max = min,
                    .import = import,
                });
            },
            .MinMax => {
                const min = try leb.readULEB128(u32, rd);
                const max = try leb.readULEB128(u32, rd);

                try self.tables.list.append(Limit{
                    .min = min,
                    .max = max,
                    .import = import,
                });
            },
        }
    }

    fn decodeMemorySection(self: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);
        self.memories.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            try self.decodeMemory(null);
        }

        return count;
    }

    fn decodeMemory(self: *Module, import: ?u32) !void {
        const rd = self.buf.reader();
        const limit_type = try rd.readEnum(LimitType, .Little);
        switch (limit_type) {
            .Min => {
                const min = try leb.readULEB128(u32, rd);

                try self.memories.list.append(Limit{
                    .min = min,
                    .max = std.math.maxInt(u32),
                    .import = import,
                });
            },
            .MinMax => {
                const min = try leb.readULEB128(u32, rd);
                const max = try leb.readULEB128(u32, rd);

                try self.memories.list.append(Limit{
                    .min = min,
                    .max = max,
                    .import = import,
                });
            },
        }
    }

    fn decodeGlobalSection(self: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);
        self.globals.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            try self.decodeGlobal(null);
        }

        return count;
    }

    fn decodeGlobal(self: *Module, import: ?u32) !void {
        const rd = self.buf.reader();
        const global_type = try rd.readEnum(ValueType, .Little);
        const mutability = try rd.readEnum(Mutability, .Little);
        const offset = rd.context.pos;

        var code: ?[]const u8 = null;

        // If we're not importing the global we will expect
        // an expression
        if (import == null) {
            // TODO: this isn't right
            var j: usize = 0;
            while (true) : (j += 1) {
                const byte = try rd.readByte();
                if (byte == @enumToInt(Instruction.End)) break;
            }
            code = self.module[offset .. offset + j + 1];

            try decodeCode(code orelse return error.ExpectedCode);
        }

        if (code == null and import == null) return error.ExpectedOneOrTheOther;
        if (code != null and import != null) return error.ExpectedOneOrTheOther;

        try self.globals.list.append(Global{
            .value_type = global_type,
            .mutability = mutability,
            .code = code,
            .import = import,
        });
    }

    fn decodeExportSection(self: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);
        self.exports.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const name_length = try leb.readULEB128(u32, rd);
            if (rd.context.pos + name_length > self.module.len) return error.UnexpectedEndOfSection;
            const name = self.module[rd.context.pos .. rd.context.pos + name_length];
            try rd.skipBytes(name_length, .{});

            const tag = try rd.readEnum(Tag, .Little);
            const index = try leb.readULEB128(u32, rd);

            try self.exports.list.append(Export{
                .name = name,
                .tag = tag,
                .index = index,
            });
        }

        return count;
    }

    fn decodeStartSection(self: *Module) !usize {
        if (self.start != null) return error.MultipleStartSections;
        const rd = self.buf.reader();
        const index = try leb.readULEB128(u32, rd);

        self.start = index;

        return 1;
    }

    fn decodeElementSection(self: *Module, size: u32) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);
        self.elements.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const table_index = try leb.readULEB128(u32, rd);

            const expr_start = rd.context.pos;
            const expr = self.module[expr_start..];
            const meta = try instruction.findExprEnd(true, expr);

            try rd.skipBytes(meta.offset + 1, .{});

            // Number of u32's in our data (not the length in bytes!)
            const data_length = try leb.readULEB128(u32, rd);
            const data_start = rd.context.pos;

            var j: usize = 0;
            while (j < data_length) : (j += 1) {
                // When we pre-process all this data we can just store this
                // but for the moment we're just using it to skip over
                _ = try leb.readULEB128(u32, rd);
            }

            try self.elements.list.append(Segment{
                .index = table_index,
                .offset = self.module[expr_start .. expr_start + meta.offset + 1],
                .count = data_length,
                .data = self.module[data_start..rd.context.pos],
            });
        }

        return 1;
    }

    fn decodeCodeSection(self: *Module) !usize {
        const rd = self.buf.reader();
        var x = rd.context.pos;
        const count = try leb.readULEB128(u32, rd);
        self.codes.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const size = try leb.readULEB128(u32, rd); // includes bytes defining locals
            const offset = rd.context.pos;

            // TODO: run some verification on the code
            //      e.g. check last value is End instruction
            var locals_and_code = self.module[offset .. offset + size];
            const locals_definitions_count = try leb.readULEB128(u32, rd);

            const locals_start = rd.context.pos;

            // Iterate over local definitions counting them
            var j: usize = 0;
            var locals_count: usize = 0;
            while (j < locals_definitions_count) : (j += 1) {
                const type_count = try leb.readULEB128(u32, rd);
                const local_type = try rd.readByte();
                locals_count += type_count;
            }
            if (locals_count > 0x100000000) return error.TooManyLocals;

            const code_start = rd.context.pos;

            const locals = self.module[locals_start..code_start];
            const code = self.module[code_start .. offset + size];

            try decodeCode(code);

            try self.codes.list.append(Code{
                .locals = locals,
                .locals_count = locals_count,
                .code = code,
            });
            try rd.skipBytes(code.len, .{});
        }

        return count;
    }

    fn decodeDataSection(self: *Module, size: u32) !usize {
        const rd = self.buf.reader();
        var section_offset = rd.context.pos;
        const count = try leb.readULEB128(u32, rd);
        self.datas.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const mem_idx = try leb.readULEB128(u32, rd);

            const expr_start = rd.context.pos;
            // TODO: bounds check
            const expr = self.module[expr_start..];
            const meta = try instruction.findExprEnd(true, expr);

            try rd.skipBytes(meta.offset + 1, .{});
            const data_length = try leb.readULEB128(u32, rd);

            const offset = rd.context.pos;

            try rd.skipBytes(data_length, .{});

            try self.datas.list.append(Segment{
                .index = mem_idx,
                .offset = self.module[expr_start .. expr_start + meta.offset + 1],
                .count = data_length,
                .data = self.module[offset..rd.context.pos],
            });
        }

        // const remaining_data = size - (rd.context.pos - section_offset);
        // try rd.skipBytes(remaining_data, .{});

        return count;
    }

    fn decodeCustomSection(self: *Module, size: u32) !usize {
        const rd = self.buf.reader();
        const offset = rd.context.pos;

        const name_length = try leb.readULEB128(u32, rd);
        if (rd.context.pos + name_length > self.module.len) return error.UnexpectedEndOfSection;
        const name = self.module[rd.context.pos .. rd.context.pos + name_length];
        try rd.skipBytes(name_length, .{});

        if (!unicode.utf8ValidateSlice(name)) return error.NameNotUTF8;

        const remaining_size = size - (rd.context.pos - offset);

        if (rd.context.pos + remaining_size > self.module.len) return error.UnexpectedEndOfSection;
        const data = self.module[rd.context.pos .. rd.context.pos + remaining_size];
        try rd.skipBytes(remaining_size, .{});

        try self.customs.list.append(Custom{
            .name = name,
            .data = data,
        });

        return 1;
    }

    // decodeCode
    //
    // Checks the binary representation of a function is well formed
    pub fn decodeCode(code: []const u8) !void {
        var it = instruction.InstructionIterator.init(code);
        while (try it.next(true)) |meta| {
            // try will fail on bad code
        }
        // 2. Make sure we can find the end of every function
        _ = try instruction.findFunctionEnd(true, code);
    }

    pub fn getExport(self: *Module, tag: Tag, name: []const u8) !usize {
        for (self.exports.list.items) |exported| {
            if (tag == exported.tag and mem.eql(u8, name, exported.name)) return exported.index;
        }

        return error.ExportNotFound;
    }

    pub fn signaturesEqual(self: *Module, a: FuncType, b: FuncType) bool {
        if (a.params_count != b.params_count) return false;
        if (a.results_count != b.results_count) return false;

        const params_a = self.value_types.list.items[a.params_offset .. a.params_offset + a.params_count];
        const params_b = self.value_types.list.items[b.params_offset .. b.params_offset + b.params_count];

        for (params_a) |p_a, i| {
            if (p_a != params_b[i]) return false;
        }

        const results_a = self.value_types.list.items[a.results_offset .. a.results_offset + a.results_count];
        const results_b = self.value_types.list.items[b.results_offset .. b.results_offset + b.results_count];

        for (results_a) |r_a, i| {
            if (r_a != results_b[i]) return false;
        }

        return true;
    }

    pub fn instantiate(self: *Module, allocator: *mem.Allocator, store: *Store) !Instance {
        if (self.decoded == false) return error.ModuleNotDecoded;

        var inst = Instance{
            .module = self.*,
            .store = store,
            .memaddrs = ArrayList(usize).init(allocator),
            .tableaddrs = ArrayList(usize).init(allocator),
            .globaladdrs = ArrayList(usize).init(allocator),
        };

        // 1. Initialise imports
        for (self.imports.list.items) |import, i| {
            const import_handle = try store.import(import.module, import.name, import.desc_tag);
            switch (import.desc_tag) {
                .Func => {},
                .Mem => try inst.memaddrs.append(import_handle),
                .Table => try inst.tableaddrs.append(import_handle),
                .Global => try inst.globaladdrs.append(import_handle),
            }
        }

        // 2. Initialise globals
        for (self.globals.list.items) |global_def, i| {
            const handle = try inst.store.addGlobal();
            try inst.globaladdrs.append(handle);
        }

        // 3. Initialise memories
        for (self.memories.list.items) |memory_definition, i| {
            const handle = try inst.store.addMemory();
            const memory = try inst.store.memory(handle);

            try inst.memaddrs.append(handle);
            _ = try memory.grow(memory_definition.min);
            memory.max_size = memory_definition.max;
        }

        // 4. Initialise memories with data
        for (self.datas.list.items) |data, i| {
            const handle = inst.memaddrs.items[data.index];
            const memory = try inst.store.memory(handle);

            const offset = try inst.invokeExpression(data.offset, u32, .{});
            try memory.copy(offset, data.data);
        }

        // 5. Initialise tables
        for (self.tables.list.items) |table_size, i| {
            const handle = try inst.store.addTable(table_size.min, table_size.max);
            try inst.tableaddrs.append(handle);
        }

        // 6. Initialise from elements
        for (self.elements.list.items) |element_def, i| {
            if (element_def.count == 0) continue; // Zero-length is allowed and shouldn't error
            const table = try inst.table(element_def.index);

            const offset = try inst.invokeExpression(element_def.offset, u32, .{});

            // Test that offset is in bounds
            _ = table.lookup(offset) catch |err| switch (err) {
                error.UndefinedElement => {},
                else => return err,
            };

            var data = element_def.data;
            var j: usize = 0;
            while (j < element_def.count) : (j += 1) {
                const value = try instruction.readULEB128Mem(u32, &data);
                try table.set(@intCast(u32, offset + j), value);
            }
        }

        return inst;
    }

    pub fn print(module: *Module) void {
        std.debug.warn("    Types: {}\n", .{module.types.list.items.len});
        std.debug.warn("Functions: {}\n", .{module.functions.list.items.len});
        std.debug.warn("   Tables: {}\n", .{module.tables.list.items.len});
        std.debug.warn(" Memories: {}\n", .{module.memories.list.items.len});
        std.debug.warn("  Globals: {}\n", .{module.globals.list.items.len});
        std.debug.warn("  Exports: {}\n", .{module.exports.list.items.len});
        std.debug.warn("  Imports: {}\n", .{module.imports.list.items.len});
        std.debug.warn("    Codes: {}\n", .{module.codes.list.items.len});
        std.debug.warn("    Datas: {}\n", .{module.datas.list.items.len});
        std.debug.warn("  Customs: {}\n", .{module.customs.list.items.len});
    }
};

fn Section(comptime T: type) type {
    return struct {
        count: usize = 0,
        list: ArrayList(T),

        const Self = @This();

        pub fn init(alloc: *mem.Allocator) Self {
            return Self{
                .list = ArrayList(T).init(alloc),
            };
        }

        pub fn itemsSlice(self: *Self) []T {
            return self.list.items;
        }
    };
}

const Custom = struct {
    name: []const u8,
    data: []const u8,
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
    Element = 0x09,
    Code = 0x0a,
    Data = 0x0b,
};

const testing = std.testing;

test "module loading (simple add function)" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer _ = arena.deinit();

    const bytes = @embedFile("../test/test.wasm");

    var module = Module.init(&arena.allocator, bytes);
    try module.decode();
    var modinst = try module.instantiate();

    const result = try modinst.invoke("add", .{ @as(i32, 22), @as(i32, 23) }, i32, .{});
    testing.expectEqual(@as(i32, 45), result);
}

test "module loading (fib)" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer _ = arena.deinit();

    const bytes = @embedFile("../test/fib.wasm");

    var module = Module.init(&arena.allocator, bytes);
    try module.decode();
    var modinst = try module.instantiate();

    testing.expectEqual(@as(i32, 1), try modinst.invoke("fib", .{@as(i32, 0)}, i32, .{}));
    testing.expectEqual(@as(i32, 1), try modinst.invoke("fib", .{@as(i32, 1)}, i32, .{}));
    testing.expectEqual(@as(i32, 2), try modinst.invoke("fib", .{@as(i32, 2)}, i32, .{}));
    testing.expectEqual(@as(i32, 3), try modinst.invoke("fib", .{@as(i32, 3)}, i32, .{}));
    testing.expectEqual(@as(i32, 5), try modinst.invoke("fib", .{@as(i32, 4)}, i32, .{}));
    testing.expectEqual(@as(i32, 8), try modinst.invoke("fib", .{@as(i32, 5)}, i32, .{}));
    testing.expectEqual(@as(i32, 13), try modinst.invoke("fib", .{@as(i32, 6)}, i32, .{}));
}

test "module loading (fact)" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer _ = arena.deinit();

    const bytes = @embedFile("../test/fact.wasm");

    var module = Module.init(&arena.allocator, bytes);
    try module.decode();
    var modinst = try module.instantiate();

    testing.expectEqual(@as(i32, 1), try modinst.invoke("fact", .{@as(i32, 1)}, i32, .{}));
    testing.expectEqual(@as(i32, 2), try modinst.invoke("fact", .{@as(i32, 2)}, i32, .{}));
    testing.expectEqual(@as(i32, 6), try modinst.invoke("fact", .{@as(i32, 3)}, i32, .{}));
    testing.expectEqual(@as(i32, 24), try modinst.invoke("fact", .{@as(i32, 4)}, i32, .{}));
    testing.expectEqual(@as(i32, 479001600), try modinst.invoke("fact", .{@as(i32, 12)}, i32, .{}));
}
