const std = @import("std");
const mem = std.mem;
const leb = std.leb;
const math = std.math;
const unicode = std.unicode;
const ArrayList = std.ArrayList;
const Rr = @import("rr.zig").Rr;
const RrOpcode = @import("rr.zig").RrOpcode;
const Instance = @import("instance.zig").Instance;
const Parser = @import("module/parser.zig").Parser;
const Parsed = @import("module/parser.zig").Parsed;
const NumType = @import("valtype.zig").NumType;
const ValType = @import("valtype.zig").ValType;
const RefType = @import("valtype.zig").RefType;

pub const Module = struct {
    decoded: bool = false,
    alloc: mem.Allocator,
    wasm_bin: []const u8,
    customs: Section(Custom),
    types: Section(FuncType),
    imports: Section(Import),
    functions: Section(Function),
    tables: Section(TableType),
    memories: Section(MemType),
    globals: Section(GlobalType),
    exports: Section(Export),
    elements: Section(ElementSegment),
    codes: Section(Code),
    datas: Section(DataSegment),
    start: ?u32 = null,
    function_index_start: ?usize = null,
    data_count: ?u32 = null,
    element_init_offsets: ArrayList(usize),
    parsed_code: ArrayList(Rr),
    local_types: ArrayList(LocalType),
    br_table_indices: ArrayList(u32),
    references: ArrayList(u32),

    pub fn init(alloc: mem.Allocator, wasm_bin: []const u8) Module {
        return Module{
            .alloc = alloc,
            .wasm_bin = wasm_bin,
            .customs = Section(Custom).init(alloc),
            .types = Section(FuncType).init(alloc),
            .imports = Section(Import).init(alloc),
            .functions = Section(Function).init(alloc),
            .tables = Section(TableType).init(alloc),
            .memories = Section(MemType).init(alloc),
            .globals = Section(GlobalType).init(alloc),
            .exports = Section(Export).init(alloc),
            .elements = Section(ElementSegment).init(alloc),
            .codes = Section(Code).init(alloc),
            .datas = Section(DataSegment).init(alloc),
            .element_init_offsets = ArrayList(usize).init(alloc),
            .parsed_code = ArrayList(Rr).init(alloc),
            .local_types = ArrayList(LocalType).init(alloc),
            .br_table_indices = ArrayList(u32).init(alloc),
            .references = ArrayList(u32).init(alloc),
        };
    }

    pub fn deinit(self: *Module) void {
        self.customs.deinit();
        self.types.deinit();
        self.imports.deinit();
        self.functions.deinit();
        self.tables.deinit();
        self.memories.deinit();
        self.globals.deinit();
        self.exports.deinit();
        self.elements.deinit();
        self.codes.deinit();
        self.datas.deinit();

        self.element_init_offsets.deinit();
        self.parsed_code.deinit();
        self.local_types.deinit();
        self.br_table_indices.deinit();
        self.references.deinit();
    }

    pub fn decode(self: *Module) !void {
        if (self.decoded) return error.AlreadyDecoded;
        var decoder = Decoder{
            .fbs = .{ .pos = 0, .buffer = self.wasm_bin },
        };
        const rd = decoder.fbs.reader();

        const magic = try rd.readBytesNoEof(4);
        if (!mem.eql(u8, magic[0..], "\x00asm")) return error.MagicNumberNotFound;

        const version = try rd.readInt(u32, .little);
        if (version != 1) return error.UnknownBinaryVersion;

        // FIXME: in hindsight I don't like this:
        // Push an initial return instruction so we don't have to
        // track the end of a function to use its return on invoke
        // See https://github.com/malcolmstill/zware/pull/133
        try self.parsed_code.append(.@"return");

        var i: usize = 0;
        while (true) : (i += 1) {
            decoder.decodeSection(self) catch |err| switch (err) {
                error.WasmFileEnd => break,
                else => return err,
            };
            try self.verify();
        }

        try self.verify();
        if (self.codes.list.items.len != nonImportCount(self.functions.list.items)) return error.FunctionCodeSectionsInconsistent;

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

    pub fn getExport(self: *Module, tag: Tag, name: []const u8) !usize {
        for (self.exports.list.items) |exported| {
            if (tag == exported.tag and mem.eql(u8, name, exported.name)) return exported.index;
        }

        return error.ExportNotFound;
    }
};

pub const Decoder = struct {
    fbs: std.io.FixedBufferStream([]const u8),

    pub fn decodeSection(self: *Decoder, module: *Module) !void {
        const id: SectionType = self.readEnum(SectionType) catch |err| switch (err) {
            error.EndOfStream => return error.WasmFileEnd,
            else => return err,
        };

        const size = try self.readULEB128(u32);

        const section_start = self.fbs.pos;

        switch (id) {
            .Custom => try self.decodeCustomSection(module, size),
            .Type => try self.decodeTypeSection(module),
            .Import => try self.decodeImportSection(module),
            .Function => try self.decodeFunctionSection(module),
            .Table => try self.decodeTableSection(module),
            .Memory => try self.decodeMemorySection(module),
            .Global => try self.decodeGlobalSection(module),
            .Export => try self.decodeExportSection(module),
            .Start => try self.decodeStartSection(module),
            .Element => try self.decodeElementSection(module),
            .Code => try self.decodeCodeSection(module),
            .Data => try self.decodeDataSection(module),
            .DataCount => try self.decodeDataCountSection(module, size),
        }

        const section_end = self.fbs.pos;
        if (section_end - section_start != size) return error.MalformedSectionMismatchedSize;
    }

    fn decodeTypeSection(self: *Decoder, module: *Module) !void {
        const count = try self.readULEB128(u32);
        module.types.count = count;

        var f: usize = 0;
        while (f < count) : (f += 1) {
            const tag = try self.readByte();
            if (tag != 0x60) return error.ExpectedFuncTypeTag;

            const param_count = try self.readULEB128(u32);
            const params_start = self.fbs.pos;
            {
                var i: usize = 0;
                while (i < param_count) : (i += 1) {
                    _ = try self.readEnum(ValType);
                }
            }
            const params_end = self.fbs.pos;

            const results_count = try self.readULEB128(u32);
            const results_start = self.fbs.pos;
            {
                var i: usize = 0;
                while (i < results_count) : (i += 1) {
                    _ = try self.readEnum(ValType);
                }
            }
            const results_end = self.fbs.pos;

            const params = module.wasm_bin[params_start..params_end];
            const results = module.wasm_bin[results_start..results_end];

            var params_valtype: []const ValType = undefined;
            params_valtype.ptr = @ptrCast(params.ptr);
            params_valtype.len = params.len;

            var results_valtype: []const ValType = undefined;
            results_valtype.ptr = @ptrCast(results.ptr);
            results_valtype.len = results.len;

            try module.types.list.append(FuncType{
                .params = params_valtype,
                .results = results_valtype,
            });
        }
    }

    fn decodeImportSection(self: *Decoder, module: *Module) !void {
        const count = try self.readULEB128(u32);
        module.imports.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const module_name_length = try self.readULEB128(u32);
            const module_name = try self.readSlice(module_name_length);

            if (!unicode.utf8ValidateSlice(module_name)) return error.NameNotUTF8;

            const name_length = try self.readULEB128(u32);
            const name = try self.readSlice(name_length);

            if (!unicode.utf8ValidateSlice(name)) return error.NameNotUTF8;

            const tag = try self.readEnum(Tag);

            if (i > math.maxInt(u32)) return error.ExpectedU32Index;
            const import_index: u32 = @truncate(i);
            _ = switch (tag) {
                .Func => try self.decodeFunction(module, import_index),
                .Table => try self.decodeTable(module, import_index),
                .Mem => try self.decodeMemory(module, import_index),
                .Global => try self.decodeGlobal(module, import_index),
            };

            try module.imports.list.append(Import{
                .module = module_name,
                .name = name,
                .desc_tag = tag,
            });
        }
    }

    fn decodeFunctionSection(self: *Decoder, module: *Module) !void {
        const count = try self.readULEB128(u32);
        module.functions.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            try self.decodeFunction(module, null);
        }
    }

    fn decodeFunction(self: *Decoder, module: *Module, import: ?u32) !void {
        const typeidx = try self.readULEB128(u32);

        if (typeidx >= module.types.list.items.len) return error.ValidatorInvalidTypeIndex;

        if (import == null and module.function_index_start == null) {
            module.function_index_start = module.functions.list.items.len;
        }

        try module.functions.list.append(Function{
            .typeidx = typeidx,
            .import = import,
        });
    }

    fn decodeTableSection(self: *Decoder, module: *Module) !void {
        const count = try self.readULEB128(u32);
        module.tables.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            try self.decodeTable(module, null);
        }
    }

    fn decodeTable(self: *Decoder, module: *Module, import: ?u32) !void {
        const reftype = try self.readEnum(RefType);
        const limit_type = try self.readEnum(LimitType);
        const min = try self.readULEB128(u32);
        const max: ?u32 = blk: {
            switch (limit_type) {
                .Min => break :blk null,
                .MinMax => {
                    const max = try self.readULEB128(u32);
                    if (min > max) return error.ValidatorTableMinGreaterThanMax;
                    break :blk max;
                },
            }
        };
        try module.tables.list.append(TableType{
            .import = import,
            .reftype = reftype,
            .limits = Limit{
                .min = min,
                .max = max,
            },
        });
    }

    fn decodeMemorySection(self: *Decoder, module: *Module) !void {
        const count = try self.readULEB128(u32);
        module.memories.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            try self.decodeMemory(module, null);
        }
    }

    fn decodeMemory(self: *Decoder, module: *Module, import: ?u32) !void {
        if (module.memories.list.items.len > 0) return error.ValidatorMultipleMemories;

        const limit_type = try self.readEnum(LimitType);
        const min = try self.readULEB128(u32);
        if (min > 65536) return error.ValidatorMemoryMinTooLarge;
        const max: ?u32 = blk: {
            switch (limit_type) {
                .Min => break :blk null,
                .MinMax => {
                    const max = try self.readULEB128(u32);
                    if (min > max) return error.ValidatorMemoryMinGreaterThanMax;
                    if (max > 65536) return error.ValidatorMemoryMaxTooLarge;
                    break :blk max;
                },
            }
        };
        try module.memories.list.append(MemType{
            .import = import,
            .limits = Limit{
                .min = min,
                .max = max,
            },
        });
    }

    fn decodeGlobalSection(self: *Decoder, module: *Module) !void {
        const count = try self.readULEB128(u32);
        module.globals.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            try self.decodeGlobal(module, null);
        }
    }

    fn decodeGlobal(self: *Decoder, module: *Module, import: ?u32) !void {
        const global_type = try self.readEnum(ValType);
        const mutability = try self.readEnum(Mutability);

        var parsed_code: ?Parsed = null;

        // If we're not importing the global we will expect
        // an expression
        if (import == null) {
            parsed_code = try self.readConstantExpression(module, global_type);
        }

        if (parsed_code == null and import == null) return error.ExpectedOneOrTheOther;
        if (parsed_code != null and import != null) return error.ExpectedOneOrTheOther;

        try module.globals.list.append(GlobalType{
            .valtype = global_type,
            .mutability = mutability,
            .start = if (parsed_code) |pc| pc.start else null,
            .import = import,
        });
    }

    fn decodeExportSection(self: *Decoder, module: *Module) !void {
        const count = try self.readULEB128(u32);
        module.exports.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const name_length = try self.readULEB128(u32);
            const name = try self.readSlice(name_length);

            if (!unicode.utf8ValidateSlice(name)) return error.NameNotUTF8;

            for (module.exports.list.items) |exprt| {
                if (mem.eql(u8, name, exprt.name)) return error.ValidatorDuplicateExportName;
            }

            const tag = try self.readEnum(Tag);
            const index = try self.readULEB128(u32);

            switch (tag) {
                .Func => {
                    if (index >= module.functions.list.items.len) return error.ValidatorExportUnknownFunction;
                    try module.references.append(index);
                },
                .Table => if (index >= module.tables.list.items.len) return error.ValidatorExportUnknownTable,
                .Mem => if (index >= module.memories.list.items.len) return error.ValidatorExportUnknownMemory,
                .Global => if (index >= module.globals.list.items.len) return error.ValidatorExportUnknownGlobal,
            }

            try module.exports.list.append(Export{
                .name = name,
                .tag = tag,
                .index = index,
            });
        }
    }

    fn decodeStartSection(self: *Decoder, module: *Module) !void {
        if (module.start != null) return error.MultipleStartSections;

        const funcidx = try self.readULEB128(u32);
        const func = try module.functions.lookup(funcidx);
        const functype = try module.types.lookup(func.typeidx);

        if (functype.params.len != 0 or functype.results.len != 0) return error.ValidatorNotStartFunctionType;

        module.start = funcidx;
    }

    fn decodeElementSection(self: *Decoder, module: *Module) !void {
        const count = try self.readULEB128(u32);
        module.elements.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const elem_type = try self.readULEB128(u32);

            switch (elem_type) {
                0 => {
                    const tableidx = 0;
                    if (tableidx >= module.tables.list.items.len) return error.ValidatorElemUnknownTable;

                    const parsed_offset_code = try self.readConstantExpression(module, .I32);

                    const data_length = try self.readULEB128(u32);

                    const first_init_offset = module.element_init_offsets.items.len;

                    var j: usize = 0;
                    while (j < data_length) : (j += 1) {
                        const funcidx = try self.readULEB128(u32);

                        if (funcidx >= module.functions.list.items.len) return error.ValidatorElemUnknownFunctionIndex;

                        try module.references.append(funcidx);

                        const init_offset = module.parsed_code.items.len;
                        try module.parsed_code.append(Rr{ .@"ref.func" = funcidx });
                        try module.parsed_code.append(Rr.@"return");
                        try module.element_init_offsets.append(init_offset);
                    }

                    try module.elements.list.append(ElementSegment{
                        .reftype = .FuncRef,
                        .init = first_init_offset,
                        .count = data_length,
                        .mode = ElementSegmentMode{ .Active = .{
                            .tableidx = 0,
                            .offset = parsed_offset_code.start,
                        } },
                    });
                },
                1 => {
                    _ = try self.readEnum(ElemKind);

                    const data_length = try self.readULEB128(u32);

                    const first_init_offset = module.element_init_offsets.items.len;

                    var j: usize = 0;
                    while (j < data_length) : (j += 1) {
                        const funcidx = try self.readULEB128(u32);

                        if (funcidx >= module.functions.list.items.len) return error.ValidatorElemUnknownFunctionIndex;

                        try module.references.append(funcidx);

                        const init_offset = module.parsed_code.items.len;
                        try module.parsed_code.append(Rr{ .@"ref.func" = funcidx });
                        try module.parsed_code.append(Rr.@"return");
                        try module.element_init_offsets.append(init_offset);
                    }

                    try module.elements.list.append(ElementSegment{
                        .reftype = .FuncRef,
                        .init = first_init_offset,
                        .count = data_length,
                        .mode = ElementSegmentMode.Passive,
                    });
                },
                2 => {
                    const tableidx = try self.readULEB128(u32);

                    if (tableidx >= module.tables.list.items.len) return error.ValidatorElemUnknownTable;

                    const parsed_offset_code = try self.readConstantExpression(module, .I32);

                    _ = try self.readEnum(ElemKind);
                    const data_length = try self.readULEB128(u32);

                    const first_init_offset = module.element_init_offsets.items.len;

                    var j: usize = 0;
                    while (j < data_length) : (j += 1) {
                        const funcidx = try self.readULEB128(u32);

                        if (funcidx >= module.functions.list.items.len) return error.ValidatorElemUnknownFunctionIndex;

                        try module.references.append(funcidx);

                        const init_offset = module.parsed_code.items.len;
                        try module.parsed_code.append(Rr{ .@"ref.func" = funcidx });
                        try module.parsed_code.append(Rr.@"return");
                        try module.element_init_offsets.append(init_offset);
                    }

                    try module.elements.list.append(ElementSegment{
                        .reftype = .FuncRef,
                        .init = first_init_offset,
                        .count = data_length,
                        .mode = ElementSegmentMode{ .Active = .{
                            .tableidx = tableidx,
                            .offset = parsed_offset_code.start,
                        } },
                    });
                },
                3 => {
                    _ = try self.readEnum(ElemKind);
                    const data_length = try self.readULEB128(u32);

                    const first_init_offset = module.element_init_offsets.items.len;

                    var j: usize = 0;
                    while (j < data_length) : (j += 1) {
                        const funcidx = try self.readULEB128(u32);

                        if (funcidx >= module.functions.list.items.len) return error.ValidatorElemUnknownFunctionIndex;

                        try module.references.append(funcidx);

                        const init_offset = module.parsed_code.items.len;
                        try module.parsed_code.append(Rr{ .@"ref.func" = funcidx });
                        try module.parsed_code.append(Rr.@"return");
                        try module.element_init_offsets.append(init_offset);
                    }

                    try module.elements.list.append(ElementSegment{
                        .reftype = .FuncRef,
                        .init = first_init_offset,
                        .count = data_length,
                        .mode = ElementSegmentMode.Declarative,
                    });
                },
                4 => {
                    const tableidx = 0;
                    if (tableidx >= module.tables.list.items.len) return error.ValidatorElemUnknownTable;

                    const parsed_offset_code = try self.readConstantExpression(module, .I32);

                    const init_expression_count = try self.readULEB128(u32);

                    const first_init_offset = module.element_init_offsets.items.len;

                    var j: usize = 0;
                    while (j < init_expression_count) : (j += 1) {
                        const parsed_init_code = try self.readConstantExpression(module, .FuncRef);
                        try module.element_init_offsets.append(parsed_init_code.start);
                    }

                    try module.elements.list.append(ElementSegment{
                        .reftype = .FuncRef,
                        .init = first_init_offset,
                        .count = init_expression_count,
                        .mode = ElementSegmentMode{ .Active = .{
                            .tableidx = 0,
                            .offset = parsed_offset_code.start,
                        } },
                    });
                },
                5 => { // Passive
                    const reftype = try self.readEnum(RefType);
                    const expr_count = try self.readULEB128(u32);

                    const first_init_offset = module.element_init_offsets.items.len;

                    var j: usize = 0;
                    while (j < expr_count) : (j += 1) {
                        const init_offset = module.parsed_code.items.len;
                        _ = try self.readConstantExpression(module, .FuncRef);
                        try module.element_init_offsets.append(init_offset);
                    }

                    try module.elements.list.append(ElementSegment{
                        .reftype = reftype,
                        .init = first_init_offset,
                        .count = expr_count,
                        .mode = ElementSegmentMode.Passive,
                    });
                },
                7 => { // Declarative
                    const reftype = try self.readEnum(RefType);
                    const expr_count = try self.readULEB128(u32);

                    const first_init_offset = module.element_init_offsets.items.len;

                    var j: usize = 0;
                    while (j < expr_count) : (j += 1) {
                        const parsed_init_code = try self.readConstantExpression(module, .FuncRef);
                        try module.element_init_offsets.append(parsed_init_code.start);
                    }

                    try module.elements.list.append(ElementSegment{
                        .reftype = reftype,
                        .init = first_init_offset,
                        .count = expr_count,
                        .mode = ElementSegmentMode.Declarative,
                    });
                },
                else => {
                    return error.ValidatorElemTypeNotImplemented;
                },
            }
        }
    }

    fn decodeDataCountSection(self: *Decoder, module: *Module, size: u32) !void {
        if (size == 0) return;
        module.data_count = try self.readULEB128(u32);
    }

    fn decodeCodeSection(self: *Decoder, module: *Module) !void {
        const count = try self.readULEB128(u32);
        module.codes.count = count;

        try module.parsed_code.ensureTotalCapacity(count * 32);

        if (count == 0) return;

        const function_index_start = module.function_index_start orelse return error.FunctionCodeSectionsInconsistent;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            // size: the number of bytes defining the function, includes bytes defining locals
            _ = try self.readULEB128(u32);

            const locals_definitions_count = try self.readULEB128(u32);

            const locals_start = module.local_types.items.len;

            // Iterate over local definitions counting them
            var j: usize = 0;
            var locals_count: usize = 0;
            while (j < locals_definitions_count) : (j += 1) {
                const type_count = try self.readULEB128(u32);
                const local_type = try self.readEnum(ValType);
                locals_count += type_count;

                try module.local_types.append(.{ .count = type_count, .valtype = local_type });
            }
            if (locals_count >= 0x100000000) return error.TooManyLocals;

            const locals = module.local_types.items[locals_start .. locals_start + locals_definitions_count];

            if (function_index_start + i >= module.functions.list.items.len) return error.FunctionCodeSectionsInconsistent;
            const parsed_code = try self.readFunction(module, locals, function_index_start + i);

            try module.codes.list.append(Code{
                .locals_count = locals_count,
                .start = parsed_code.start,
                .required_stack_space = parsed_code.max_depth,
            });
        }
    }

    fn decodeDataSection(self: *Decoder, module: *Module) !void {
        const count = try self.readULEB128(u32);

        if (module.data_count) |data_count| {
            if (count != data_count) return error.DataCountSectionDataSectionCountMismatch;
        }

        module.datas.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const data_section_type = try self.readULEB128(u32);

            switch (data_section_type) {
                0 => {
                    const memidx = 0;

                    if (memidx >= module.memories.list.items.len) return error.ValidatorDataMemoryReferenceInvalid;

                    const parsed_code = try self.readConstantExpression(module, .I32);

                    const data_length = try self.readULEB128(u32);
                    const data = try self.readSlice(data_length);

                    try module.datas.list.append(DataSegment{
                        .count = data_length,
                        .data = data,
                        .mode = DataSegmentMode{ .Active = .{
                            .memidx = 0,
                            .offset = parsed_code.start,
                        } },
                    });
                },
                1 => {
                    const data_length = try self.readULEB128(u32);
                    const data = try self.readSlice(data_length);

                    try module.datas.list.append(DataSegment{
                        .count = data_length,
                        .data = data,
                        .mode = .Passive,
                    });
                },
                2 => {
                    const memidx = try self.readULEB128(u32);

                    if (memidx >= module.memories.list.items.len) return error.ValidatorDataMemoryReferenceInvalid;

                    const parsed_code = try self.readConstantExpression(module, .I32);

                    const data_length = try self.readULEB128(u32);
                    const data = try self.readSlice(data_length);

                    try module.datas.list.append(DataSegment{
                        .count = data_length,
                        .data = data,
                        .mode = DataSegmentMode{ .Active = .{
                            .memidx = 0,
                            .offset = parsed_code.start,
                        } },
                    });
                },
                else => {
                    return error.UnknownDataSectionType;
                },
            }
        }
    }

    fn decodeCustomSection(self: *Decoder, module: *Module, size: u32) !void {
        const offset = self.fbs.pos;

        const name_length = try self.readULEB128(u32);
        const name = try self.readSlice(name_length);

        if (!unicode.utf8ValidateSlice(name)) return error.NameNotUTF8;

        const data_length = try math.sub(usize, size, (self.fbs.pos - offset));
        const data = try self.readSlice(data_length);

        try module.customs.list.append(Custom{
            .name = name,
            .data = data,
        });
    }

    pub fn readConstantExpression(self: *Decoder, module: *Module, valtype: ValType) !Parsed {
        const rd = self.fbs.reader();
        const code = module.wasm_bin[rd.context.pos..];

        var parser = Parser.init(module, self);
        defer parser.deinit();

        return parser.parseConstantExpression(valtype, code);
    }

    pub fn readFunction(self: *Decoder, module: *Module, locals: []LocalType, funcidx: usize) !Parsed {
        const code = module.wasm_bin[self.fbs.pos..];

        var parser = Parser.init(module, self);
        defer parser.deinit();

        return parser.parseFunction(funcidx, locals, code);
    }

    fn readByte(self: *Decoder) !u8 {
        return self.fbs.reader().readByte();
    }

    fn readEnum(self: *Decoder, comptime T: type) !T {
        return self.fbs.reader().readEnum(T, .little);
    }

    fn readULEB128(self: *Decoder, comptime T: type) !T {
        return leb.readULEB128(T, self.fbs.reader());
    }

    pub fn readSlice(self: *Decoder, count: usize) ![]const u8 {
        const start = self.fbs.pos;
        const end = self.fbs.pos + count;
        if (end > self.fbs.buffer.len)
            return error.EndOfStream;
        self.fbs.pos = end;
        return self.fbs.buffer[start..self.fbs.pos];
    }
};

fn Section(comptime T: type) type {
    return struct {
        count: usize = 0,
        list: ArrayList(T),

        const Self = @This();

        pub fn init(alloc: mem.Allocator) Self {
            return Self{
                .list = ArrayList(T).init(alloc),
            };
        }

        pub fn deinit(self: *Self) void {
            self.list.deinit();
        }

        pub fn itemsSlice(self: *Self) []T {
            return self.list.items;
        }

        pub fn lookup(self: *Self, idx: anytype) !T {
            const index = switch (@TypeOf(idx)) {
                u32 => idx,
                usize => math.cast(u32, idx) orelse return error.IndexTooLarge,
                else => @compileError("only u32 / usize supported"),
            };

            if (index >= self.list.items.len) return error.ValidatorInvalidIndex;

            return self.list.items[index];
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
    DataCount = 0x0c,
};

const ElemKind = enum(u8) {
    FuncRef = 0x0,
};

const Code = struct {
    start: usize,
    locals_count: usize,
    required_stack_space: usize,
};

pub const FuncType = struct {
    params: []const ValType,
    results: []const ValType,
};

pub const GlobalType = struct {
    valtype: ValType,
    mutability: Mutability,
    start: ?usize,
    import: ?u32,
};

const MemType = struct {
    import: ?u32,
    limits: Limit,
};

const TableType = struct {
    import: ?u32,
    reftype: RefType,
    limits: Limit,
};

const DataSegment = struct {
    count: u32,
    data: []const u8,
    mode: DataSegmentMode,
};

const DataSegmentType = enum {
    Passive,
    Active,
};

const DataSegmentMode = union(DataSegmentType) {
    Passive: void,
    Active: struct {
        memidx: u32,
        offset: usize, // index of parsed code representing offset
    },
};

pub const ElementSegment = struct {
    reftype: RefType,
    init: usize, // Offset into element_init_offset of first init expression code offset
    count: u32, // Number of element_init_offset values for this segment (we have an array of initialisation functions)
    mode: ElementSegmentMode,
};

pub const ElementSegmentType = enum {
    Passive,
    Active,
    Declarative,
};

pub const ElementSegmentMode = union(ElementSegmentType) {
    Passive: void,
    Active: struct {
        tableidx: u32,
        offset: usize, // index of parsed code representing offset
    },
    Declarative: void,
};

const LimitType = enum(u8) {
    Min,
    MinMax,
};

pub const Limit = struct {
    min: u32,
    max: ?u32,

    pub fn checkMatch(self: Limit, min_imported: u32, max_imported: ?u32) !void {
        if (min_imported < self.min) return error.LimitMismatch;
        if (self.max) |defined_max| {
            if (max_imported) |imported_max| {
                if (!(imported_max <= defined_max)) {
                    return error.LimitMismatch;
                }
            } else {
                return error.LimitMismatch;
            }
        }
    }
};

pub const Mutability = enum(u8) {
    Immutable,
    Mutable,
};

pub const Function = struct {
    typeidx: u32,
    import: ?u32,
};

pub const Import = struct {
    module: []const u8,
    name: []const u8,
    desc_tag: Tag,
    // desc: u8,
};

pub const Export = struct {
    name: []const u8,
    tag: Tag,
    index: u32,
};

pub const Tag = enum(u8) {
    Func,
    Table,
    Mem,
    Global,
};

pub const LocalType = struct {
    count: u32,
    valtype: ValType,
};

const testing = std.testing;

test "module loading (simple add function)" {
    const Store = @import("store.zig").ArrayListStore;
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer _ = arena.deinit();

    const alloc = arena.allocator();

    const bytes = @embedFile("test/test.wasm");

    var store: Store = Store.init(alloc);

    var module = Module.init(alloc, bytes);
    try module.decode();

    var instance = Instance.init(alloc, &store, module);
    try instance.instantiate();

    var in = [2]u64{ 22, 23 };
    var out = [1]u64{0};
    try instance.invoke("add", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 45), @as(i32, @bitCast(@as(u32, @truncate(out[0])))));
}

test "module loading (fib)" {
    const Store = @import("store.zig").ArrayListStore;
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer _ = arena.deinit();

    const alloc = arena.allocator();

    const bytes = @embedFile("test/fib.wasm");

    var store: Store = Store.init(alloc);

    var module = Module.init(alloc, bytes);
    try module.decode();

    var instance = Instance.init(alloc, &store, module);
    try instance.instantiate();

    var in = [1]u64{0};
    var out = [1]u64{0};
    try instance.invoke("fib", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 0), @as(i32, @bitCast(@as(u32, @truncate(out[0])))));

    in[0] = 1;
    try instance.invoke("fib", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 1), @as(i32, @bitCast(@as(u32, @truncate(out[0])))));

    in[0] = 2;
    try instance.invoke("fib", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 1), @as(i32, @bitCast(@as(u32, @truncate(out[0])))));

    in[0] = 3;
    try instance.invoke("fib", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 2), @as(i32, @bitCast(@as(u32, @truncate(out[0])))));

    in[0] = 4;
    try instance.invoke("fib", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 3), @as(i32, @bitCast(@as(u32, @truncate(out[0])))));

    in[0] = 5;
    try instance.invoke("fib", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 5), @as(i32, @bitCast(@as(u32, @truncate(out[0])))));

    in[0] = 6;
    try instance.invoke("fib", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 8), @as(i32, @bitCast(@as(u32, @truncate(out[0])))));
}

test "module loading (fact)" {
    const Store = @import("store.zig").ArrayListStore;
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer _ = arena.deinit();

    const alloc = arena.allocator();

    const bytes = @embedFile("test/fact.wasm");

    var store: Store = Store.init(alloc);

    var module = Module.init(alloc, bytes);
    try module.decode();

    var instance = Instance.init(alloc, &store, module);
    try instance.instantiate();

    var in = [1]u64{1};
    var out = [1]u64{0};
    try instance.invoke("fact", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 1), @as(i32, @bitCast(@as(u32, @truncate(out[0])))));

    in[0] = 2;
    try instance.invoke("fact", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 2), @as(i32, @bitCast(@as(u32, @truncate(out[0])))));

    in[0] = 3;
    try instance.invoke("fact", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 6), @as(i32, @bitCast(@as(u32, @truncate(out[0])))));

    in[0] = 4;
    try instance.invoke("fact", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 24), @as(i32, @bitCast(@as(u32, @truncate(out[0])))));

    in[0] = 12;
    try instance.invoke("fact", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 479001600), @as(i32, @bitCast(@as(u32, @truncate(out[0])))));
}
