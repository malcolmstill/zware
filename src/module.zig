const std = @import("std");
const mem = std.mem;
const leb = std.leb;
const math = std.math;
const unicode = std.unicode;
const ArrayList = std.ArrayList;
const Rr = @import("rr.zig").Rr;
const RrOpcode = @import("rr.zig").RrOpcode;
const Instance = @import("instance.zig").Instance;
const opcode = @import("opcode.zig");
const Opcode = @import("opcode.zig").Opcode;
const Parser = @import("parser.zig").Parser;
const NumType = @import("valtype.zig").NumType;
const ValType = @import("valtype.zig").ValType;
const RefType = @import("valtype.zig").RefType;

pub const Module = struct {
    decoded: bool = false,
    alloc: mem.Allocator,
    module: []const u8 = undefined,
    buf: std.io.FixedBufferStream([]const u8) = undefined,
    version: u32 = 0,
    customs: Section(Custom),
    types: Section(FuncType),
    imports: Section(Import),
    functions: Section(Function),
    tables: Section(TableType),
    memories: Section(MemType),
    globals: Section(GlobalType),
    exports: Section(Export),
    start: ?u32,
    elements: Section(ElementSegment),
    element_init_offsets: ArrayList(usize),
    codes: Section(Code),
    datas: Section(DataSegment),
    parsed_code: ArrayList(Rr),
    local_types: ArrayList(LocalType),
    br_table_indices: ArrayList(u32),
    function_index_start: ?usize,
    dataCount: ?u32 = null,
    references: ArrayList(u32),

    pub fn init(alloc: mem.Allocator, module: []const u8) Module {
        return Module{
            .alloc = alloc,
            .module = module,
            .buf = std.io.fixedBufferStream(module),
            .customs = Section(Custom).init(alloc),
            .types = Section(FuncType).init(alloc),
            .imports = Section(Import).init(alloc),
            .functions = Section(Function).init(alloc),
            .tables = Section(TableType).init(alloc),
            .memories = Section(MemType).init(alloc),
            .globals = Section(GlobalType).init(alloc),
            .exports = Section(Export).init(alloc),
            .start = null,
            .elements = Section(ElementSegment).init(alloc),
            .element_init_offsets = ArrayList(usize).init(alloc),
            .codes = Section(Code).init(alloc),
            .datas = Section(DataSegment).init(alloc),
            .parsed_code = ArrayList(Rr).init(alloc),
            .local_types = ArrayList(LocalType).init(alloc),
            .br_table_indices = ArrayList(u32).init(alloc),
            .function_index_start = null,
            .references = ArrayList(u32).init(alloc),
        };
    }

    // fn deinit?

    pub fn decode(self: *Module) !void {
        const rd = self.buf.reader();

        const magic = try rd.readBytesNoEof(4);
        if (!mem.eql(u8, magic[0..], "\x00asm")) return error.MagicNumberNotFound;

        const version = try rd.readIntLittle(u32);
        if (version != 1) return error.UnknownBinaryVersion;

        // Push an initial return instruction so we don't have to
        // track the end of a function to use its return on invoke
        try self.parsed_code.append(.@"return");

        var i: usize = 0;
        while (true) : (i += 1) {
            self.decodeSection() catch |err| {
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

    pub fn decodeSection(self: *Module) !void {
        const rd = self.buf.reader();

        const id: SectionType = rd.readEnum(SectionType, .Little) catch |err| switch (err) {
            error.InvalidValue => return error.UnknownSectionId,
            else => return err,
        };

        const size = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };

        const section_start = rd.context.pos;

        switch (id) {
            .Custom => try self.decodeCustomSection(size),
            .Type => try self.decodeTypeSection(),
            .Import => try self.decodeImportSection(),
            .Function => try self.decodeFunctionSection(),
            .Table => try self.decodeTableSection(),
            .Memory => try self.decodeMemorySection(),
            .Global => try self.decodeGlobalSection(),
            .Export => try self.decodeExportSection(),
            .Start => try self.decodeStartSection(),
            .Element => try self.decodeElementSection(),
            .Code => try self.decodeCodeSection(),
            .Data => try self.decodeDataSection(),
            .DataCount => try self.decodeDataCountSection(size),
        }

        const section_end = rd.context.pos;
        if (section_end - section_start != size) return error.MalformedSectionMismatchedSize;
    }

    fn decodeTypeSection(self: *Module) !void {
        const rd = self.buf.reader();

        const count = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
        self.types.count = count;

        var f: usize = 0;
        while (f < count) : (f += 1) {
            const tag: u8 = rd.readByte() catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            if (tag != 0x60) return error.ExpectedFuncTypeTag;

            const param_count = leb.readULEB128(u32, rd) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            const params_start = rd.context.pos;
            {
                var i: usize = 0;
                while (i < param_count) : (i += 1) {
                    _ = rd.readEnum(ValType, .Little) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };
                }
            }
            const params_end = rd.context.pos;

            const results_count = leb.readULEB128(u32, rd) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            const results_start = rd.context.pos;
            {
                var i: usize = 0;
                while (i < results_count) : (i += 1) {
                    _ = rd.readEnum(ValType, .Little) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };
                }
            }
            const results_end = rd.context.pos;

            var params = self.module[params_start..params_end];
            var results = self.module[results_start..results_end];

            var params_valtype: []const ValType = undefined;
            params_valtype.ptr = @ptrCast([*]const ValType, params.ptr);
            params_valtype.len = params.len;

            var results_valtype: []const ValType = undefined;
            results_valtype.ptr = @ptrCast([*]const ValType, results.ptr);
            results_valtype.len = results.len;

            try self.types.list.append(FuncType{
                .params = params_valtype,
                .results = results_valtype,
            });
        }
    }

    fn decodeImportSection(self: *Module) !void {
        const rd = self.buf.reader();

        const count = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
        self.imports.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const module_name_length = leb.readULEB128(u32, rd) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            const module_name_start = rd.context.pos;

            rd.skipBytes(module_name_length, .{}) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            const module_name = self.module[module_name_start .. module_name_start + module_name_length];
            if (!unicode.utf8ValidateSlice(module_name)) return error.NameNotUTF8;

            const name_length = leb.readULEB128(u32, rd) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            const name_start = rd.context.pos;

            rd.skipBytes(name_length, .{}) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            const name = self.module[name_start .. name_start + name_length];
            if (!unicode.utf8ValidateSlice(name)) return error.NameNotUTF8;

            const tag = rd.readEnum(Tag, .Little) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            if (i > math.maxInt(u32)) return error.ExpectedU32Index;
            const import_index = @truncate(u32, i);
            _ = switch (tag) {
                .Func => try self.decodeFunction(import_index),
                .Table => try self.decodeTable(import_index),
                .Mem => try self.decodeMemory(import_index),
                .Global => try self.decodeGlobal(import_index),
            };

            try self.imports.list.append(Import{
                .module = module_name,
                .name = name,
                .desc_tag = tag,
            });
        }
    }

    fn decodeFunctionSection(self: *Module) !void {
        const rd = self.buf.reader();

        const count = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
        self.functions.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            try self.decodeFunction(null);
        }
    }

    fn decodeFunction(self: *Module, import: ?u32) !void {
        const rd = self.buf.reader();
        const typeidx = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };

        if (typeidx >= self.types.list.items.len) return error.ValidatorInvalidTypeIndex;

        if (import == null and self.function_index_start == null) {
            self.function_index_start = self.functions.list.items.len;
        }

        try self.functions.list.append(Function{
            .typeidx = typeidx,
            .import = import,
        });
    }

    fn decodeTableSection(self: *Module) !void {
        const rd = self.buf.reader();

        const count = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
        self.tables.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            try self.decodeTable(null);
        }
    }

    fn decodeTable(self: *Module, import: ?u32) !void {
        const rd = self.buf.reader();

        const reftype = rd.readEnum(RefType, .Little) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };

        const limit_type = rd.readEnum(LimitType, .Little) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };

        switch (limit_type) {
            .Min => {
                const min = leb.readULEB128(u32, rd) catch |err| switch (err) {
                    error.EndOfStream => return error.UnexpectedEndOfInput,
                    else => return err,
                };

                try self.tables.list.append(TableType{
                    .import = import,
                    .reftype = reftype,
                    .limits = Limit{
                        .min = min,
                        .max = null,
                    },
                });
            },
            .MinMax => {
                const min = leb.readULEB128(u32, rd) catch |err| switch (err) {
                    error.EndOfStream => return error.UnexpectedEndOfInput,
                    else => return err,
                };

                const max = leb.readULEB128(u32, rd) catch |err| switch (err) {
                    error.EndOfStream => return error.UnexpectedEndOfInput,
                    else => return err,
                };

                if (min > max) return error.ValidatorTableMinGreaterThanMax;

                try self.tables.list.append(TableType{
                    .import = import,
                    .reftype = reftype,
                    .limits = Limit{
                        .min = min,
                        .max = max,
                    },
                });
            },
        }
    }

    fn decodeMemorySection(self: *Module) !void {
        const rd = self.buf.reader();

        const count = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
        self.memories.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            try self.decodeMemory(null);
        }
    }

    fn decodeMemory(self: *Module, import: ?u32) !void {
        if (self.memories.list.items.len > 0) return error.ValidatorMultipleMemories;
        const rd = self.buf.reader();

        const limit_type = rd.readEnum(LimitType, .Little) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };

        switch (limit_type) {
            .Min => {
                const min = leb.readULEB128(u32, rd) catch |err| switch (err) {
                    error.EndOfStream => return error.UnexpectedEndOfInput,
                    else => return err,
                };

                if (min > 65536) return error.ValidatorMemoryMinTooLarge;

                try self.memories.list.append(MemType{
                    .import = import,
                    .limits = Limit{
                        .min = min,
                        .max = null,
                    },
                });
            },
            .MinMax => {
                const min = leb.readULEB128(u32, rd) catch |err| switch (err) {
                    error.EndOfStream => return error.UnexpectedEndOfInput,
                    else => return err,
                };

                const max = leb.readULEB128(u32, rd) catch |err| switch (err) {
                    error.EndOfStream => return error.UnexpectedEndOfInput,
                    else => return err,
                };

                if (min > max) return error.ValidatorMemoryMinGreaterThanMax;
                if (min > 65536) return error.ValidatorMemoryMinTooLarge;
                if (max > 65536) return error.ValidatorMemoryMaxTooLarge;

                try self.memories.list.append(MemType{
                    .import = import,
                    .limits = Limit{
                        .min = min,
                        .max = max,
                    },
                });
            },
        }
    }

    fn decodeGlobalSection(self: *Module) !void {
        const rd = self.buf.reader();

        const count = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
        self.globals.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            try self.decodeGlobal(null);
        }
    }

    fn decodeGlobal(self: *Module, import: ?u32) !void {
        const rd = self.buf.reader();

        const global_type = rd.readEnum(ValType, .Little) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };

        const mutability = rd.readEnum(Mutability, .Little) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
        const offset = rd.context.pos;

        var code: ?[]const u8 = null;
        var parsed_code: ?StartWithDepth = null;

        // If we're not importing the global we will expect
        // an expression
        if (import == null) {
            // TODO: this isn't right because I think we might have more than one end
            var j: usize = 0;
            while (true) : (j += 1) {
                const byte = rd.readByte() catch |err| switch (err) {
                    error.EndOfStream => return error.UnexpectedEndOfInput,
                    else => return err,
                };

                if (byte == @enumToInt(Opcode.end)) break;
            }
            code = self.module[offset .. offset + j + 1];

            parsed_code = try self.parseConstantCode(code orelse return error.NoCode, global_type);
        }

        if (code == null and import == null) return error.ExpectedOneOrTheOther;
        if (code != null and import != null) return error.ExpectedOneOrTheOther;

        try self.globals.list.append(GlobalType{
            .valtype = global_type,
            .mutability = mutability,
            .start = if (parsed_code) |pc| pc.start else null,
            .import = import,
        });
    }

    fn decodeExportSection(self: *Module) !void {
        const rd = self.buf.reader();

        const count = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
        self.exports.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const name_length = leb.readULEB128(u32, rd) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            const name_start = rd.context.pos;

            rd.skipBytes(name_length, .{}) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            const name = self.module[name_start .. name_start + name_length];
            if (!unicode.utf8ValidateSlice(name)) return error.NameNotUTF8;

            for (self.exports.list.items) |exprt| {
                if (mem.eql(u8, name, exprt.name)) return error.ValidatorDuplicateExportName;
            }

            const tag = rd.readEnum(Tag, .Little) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            const index = leb.readULEB128(u32, rd) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            switch (tag) {
                .Func => {
                    if (index >= self.functions.list.items.len) return error.ValidatorExportUnknownFunction;
                    try self.references.append(index);
                },
                .Table => if (index >= self.tables.list.items.len) return error.ValidatorExportUnknownTable,
                .Mem => if (index >= self.memories.list.items.len) return error.ValidatorExportUnknownMemory,
                .Global => if (index >= self.globals.list.items.len) return error.ValidatorExportUnknownGlobal,
            }

            try self.exports.list.append(Export{
                .name = name,
                .tag = tag,
                .index = index,
            });
        }
    }

    fn decodeStartSection(self: *Module) !void {
        if (self.start != null) return error.MultipleStartSections;
        const rd = self.buf.reader();

        const funcidx = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };

        if (funcidx >= self.functions.list.items.len) return error.ValidatorStartFunctionUnknown;
        const func = self.functions.list.items[funcidx];
        const functype = self.types.list.items[func.typeidx];
        if (functype.params.len != 0 or functype.results.len != 0) return error.ValidatorNotStartFunctionType;

        self.start = funcidx;
    }

    fn decodeElementSection(self: *Module) !void {
        const rd = self.buf.reader();

        const count = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
        self.elements.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const elem_type = leb.readULEB128(u32, rd) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            switch (elem_type) {
                0 => {
                    const tableidx = 0;
                    if (tableidx >= self.tables.list.items.len) return error.ValidatorElemUnknownTable;

                    const expr_start = rd.context.pos;
                    const expr = self.module[expr_start..];
                    const meta = try opcode.findExprEnd(expr);

                    rd.skipBytes(meta.offset + 1, .{}) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    // Number of u32's in our data (not the length in bytes!)
                    const data_length = leb.readULEB128(u32, rd) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    const first_init_offset = self.element_init_offsets.items.len;

                    var j: usize = 0;
                    while (j < data_length) : (j += 1) {
                        // When we pre-process all this data we can just store this
                        // but for the moment we're just using it to skip over
                        const funcidx = leb.readULEB128(u32, rd) catch |err| switch (err) {
                            error.EndOfStream => return error.UnexpectedEndOfInput,
                            else => return err,
                        };

                        if (funcidx >= self.functions.list.items.len) return error.ValidatorElemUnknownFunctionIndex;

                        try self.references.append(funcidx);

                        const init_offset = self.parsed_code.items.len;
                        try self.parsed_code.append(Rr{ .@"ref.func" = funcidx });
                        try self.parsed_code.append(Rr.@"return");
                        try self.element_init_offsets.append(init_offset);
                    }

                    // offset code
                    const parsed_offset_code = try self.parseConstantCode(self.module[expr_start .. expr_start + meta.offset + 1], .I32);

                    // The parsed code defines the offset of the data
                    //
                    try self.elements.list.append(ElementSegment{
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
                    // read elemkind (only 0x00 == .FuncRef supported)
                    _ = rd.readByte() catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    // Number of u32's in our data (not the length in bytes!)
                    const data_length = leb.readULEB128(u32, rd) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    const first_init_offset = self.element_init_offsets.items.len;

                    var j: usize = 0;
                    while (j < data_length) : (j += 1) {
                        // When we pre-process all this data we can just store this
                        // but for the moment we're just using it to skip over
                        const funcidx = leb.readULEB128(u32, rd) catch |err| switch (err) {
                            error.EndOfStream => return error.UnexpectedEndOfInput,
                            else => return err,
                        };

                        if (funcidx >= self.functions.list.items.len) return error.ValidatorElemUnknownFunctionIndex;

                        try self.references.append(funcidx);

                        const init_offset = self.parsed_code.items.len;
                        try self.parsed_code.append(Rr{ .@"ref.func" = funcidx });
                        try self.parsed_code.append(Rr.@"return");
                        try self.element_init_offsets.append(init_offset);
                    }

                    try self.elements.list.append(ElementSegment{
                        .reftype = .FuncRef,
                        .init = first_init_offset,
                        .count = data_length,
                        .mode = ElementSegmentMode.Passive,
                    });
                },
                2 => {
                    const tableidx = leb.readULEB128(u32, rd) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    if (tableidx >= self.tables.list.items.len) return error.ValidatorElemUnknownTable;

                    const expr_start = rd.context.pos;
                    const expr = self.module[expr_start..];
                    const meta = try opcode.findExprEnd(expr);

                    rd.skipBytes(meta.offset + 1, .{}) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    // read elemkind (only 0x00 == .FuncRef supported)
                    _ = rd.readByte() catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    // Number of u32's in our data (not the length in bytes!)
                    const data_length = leb.readULEB128(u32, rd) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    const first_init_offset = self.element_init_offsets.items.len;

                    var j: usize = 0;
                    while (j < data_length) : (j += 1) {
                        // When we pre-process all this data we can just store this
                        // but for the moment we're just using it to skip over
                        const funcidx = leb.readULEB128(u32, rd) catch |err| switch (err) {
                            error.EndOfStream => return error.UnexpectedEndOfInput,
                            else => return err,
                        };

                        if (funcidx >= self.functions.list.items.len) return error.ValidatorElemUnknownFunctionIndex;

                        try self.references.append(funcidx);

                        const init_offset = self.parsed_code.items.len;
                        try self.parsed_code.append(Rr{ .@"ref.func" = funcidx });
                        try self.parsed_code.append(Rr.@"return");
                        try self.element_init_offsets.append(init_offset);
                    }

                    const parsed_offset_code = try self.parseConstantCode(self.module[expr_start .. expr_start + meta.offset + 1], .I32);

                    try self.elements.list.append(ElementSegment{
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
                    // read elemkind (only 0x00 == .FuncRef supported)
                    _ = rd.readByte() catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    // Number of u32's in our data (not the length in bytes!)
                    const data_length = leb.readULEB128(u32, rd) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    const first_init_offset = self.element_init_offsets.items.len;

                    var j: usize = 0;
                    while (j < data_length) : (j += 1) {
                        // When we pre-process all this data we can just store this
                        // but for the moment we're just using it to skip over
                        const funcidx = leb.readULEB128(u32, rd) catch |err| switch (err) {
                            error.EndOfStream => return error.UnexpectedEndOfInput,
                            else => return err,
                        };

                        if (funcidx >= self.functions.list.items.len) return error.ValidatorElemUnknownFunctionIndex;

                        try self.references.append(funcidx);

                        const init_offset = self.parsed_code.items.len;
                        try self.parsed_code.append(Rr{ .@"ref.func" = funcidx });
                        try self.parsed_code.append(Rr.@"return");
                        try self.element_init_offsets.append(init_offset);
                    }

                    try self.elements.list.append(ElementSegment{
                        .reftype = .FuncRef,
                        .init = first_init_offset,
                        .count = data_length,
                        .mode = ElementSegmentMode.Declarative,
                    });
                },
                4 => {
                    const tableidx = 0;
                    if (tableidx >= self.tables.list.items.len) return error.ValidatorElemUnknownTable;

                    const parsed_offset_code = try self.readConstantExpression(.I32);

                    // Number of u32's in our data (not the length in bytes!)
                    const init_expression_count = leb.readULEB128(u32, rd) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    const first_init_offset = self.element_init_offsets.items.len;

                    var j: usize = 0;
                    while (j < init_expression_count) : (j += 1) {
                        const parsed_init_code = try self.readConstantExpression(.FuncRef);
                        try self.element_init_offsets.append(parsed_init_code.start);
                    }

                    // The parsed code defines the offset of the data
                    try self.elements.list.append(ElementSegment{
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
                    const rtype = leb.readULEB128(u32, rd) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    const reftype = std.meta.intToEnum(RefType, rtype) catch return error.MalformedRefType;

                    const expr_count = leb.readULEB128(u32, rd) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    const first_init_offset = self.element_init_offsets.items.len;

                    var j: usize = 0;
                    while (j < expr_count) : (j += 1) {
                        const expr_start = rd.context.pos;
                        const expr = self.module[expr_start..];
                        const meta = try opcode.findExprEnd(expr);

                        rd.skipBytes(meta.offset + 1, .{}) catch |err| switch (err) {
                            error.EndOfStream => return error.UnexpectedEndOfInput,
                            else => return err,
                        };

                        const init_offset = self.parsed_code.items.len;
                        _ = try self.parseConstantCode(self.module[expr_start .. expr_start + meta.offset + 1], .FuncRef);
                        try self.element_init_offsets.append(init_offset);
                    }

                    try self.elements.list.append(ElementSegment{
                        .reftype = reftype,
                        .init = first_init_offset,
                        .count = expr_count,
                        .mode = ElementSegmentMode.Passive,
                    });
                },
                7 => { // Declarative
                    const rtype = leb.readULEB128(u32, rd) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    const reftype = std.meta.intToEnum(RefType, rtype) catch return error.MalformedRefType;

                    const expr_count = leb.readULEB128(u32, rd) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    const first_init_offset = self.element_init_offsets.items.len;

                    var j: usize = 0;
                    while (j < expr_count) : (j += 1) {
                        const parsed_init_code = try self.readConstantExpression(.FuncRef);
                        try self.element_init_offsets.append(parsed_init_code.start);
                    }

                    try self.elements.list.append(ElementSegment{
                        .reftype = reftype,
                        .init = first_init_offset,
                        .count = expr_count,
                        .mode = ElementSegmentMode.Declarative,
                    });
                },
                else => {
                    std.log.err("elem type = {} not implemented", .{elem_type});
                    return error.ValidatorElemTypeNotImplemented;
                },
            }
        }
    }

    fn readConstantExpression(self: *Module, valtype: ValType) !StartWithDepth {
        const rd = self.buf.reader();

        const expr_start = rd.context.pos;
        const expr = self.module[expr_start..];
        const meta = try opcode.findExprEnd(expr);

        rd.skipBytes(meta.offset + 1, .{}) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };

        return self.parseConstantCode(self.module[expr_start .. expr_start + meta.offset + 1], valtype);
    }

    fn decodeDataCountSection(self: *Module, size: u32) !void {
        const rd = self.buf.reader();

        if (size == 0) return;

        self.dataCount = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
    }

    fn decodeCodeSection(self: *Module) !void {
        const rd = self.buf.reader();

        // count: the number of functions in the code section
        const count = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
        self.codes.count = count;

        if (count == 0) return;

        const function_index_start = self.function_index_start orelse return error.FunctionCodeSectionsInconsistent;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            // size: the number of bytes defining the function, includes bytes defining locals
            const size = leb.readULEB128(u32, rd) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            const offset = rd.context.pos;

            const locals_definitions_count = leb.readULEB128(u32, rd) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            const locals_start = self.local_types.items.len;

            // Iterate over local definitions counting them
            var j: usize = 0;
            var locals_count: usize = 0;
            while (j < locals_definitions_count) : (j += 1) {
                const type_count = leb.readULEB128(u32, rd) catch |err| switch (err) {
                    error.EndOfStream => return error.UnexpectedEndOfInput,
                    else => return err,
                };

                const local_type = rd.readEnum(ValType, .Little) catch |err| switch (err) {
                    error.EndOfStream => return error.UnexpectedEndOfInput,
                    else => return err,
                };

                locals_count += type_count;

                try self.local_types.append(.{ .count = type_count, .valtype = local_type });
            }
            if (locals_count >= 0x100000000) return error.TooManyLocals;

            const code_start = rd.context.pos;
            const code_length = try math.sub(usize, size, code_start - offset);

            rd.skipBytes(code_length, .{}) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            const locals = self.local_types.items[locals_start .. locals_start + locals_definitions_count];
            const code = self.module[code_start..rd.context.pos];
            if (code[code.len - 1] != @enumToInt(RrOpcode.end)) return error.ExpectedFunctionEnd;

            if (function_index_start + i >= self.functions.list.items.len) return error.FunctionCodeSectionsInconsistent;
            const parsed_code = try self.parseFunction(locals, code, function_index_start + i);

            try self.codes.list.append(Code{
                .locals_count = locals_count,
                .start = parsed_code.start,
                .required_stack_space = parsed_code.max_depth,
            });
        }
    }

    fn decodeDataSection(self: *Module) !void {
        const rd = self.buf.reader();

        const count = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };

        if (self.dataCount) |dataCount| {
            if (count != dataCount) return error.DataCountSectionDataSectionCountMismatch;
        }

        self.datas.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const data_section_type = leb.readULEB128(u32, rd) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            switch (data_section_type) {
                0 => {
                    const memidx = 0;

                    if (memidx >= self.memories.list.items.len) return error.ValidatorDataMemoryReferenceInvalid;

                    const expr_start = rd.context.pos;
                    const expr = self.module[expr_start..];
                    const meta = try opcode.findExprEnd(expr);

                    rd.skipBytes(meta.offset + 1, .{}) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    const data_length = leb.readULEB128(u32, rd) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    const data_start = rd.context.pos;
                    rd.skipBytes(data_length, .{}) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    const parsed_code = try self.parseConstantCode(self.module[expr_start .. expr_start + meta.offset + 1], .I32);

                    try self.datas.list.append(DataSegment{
                        .count = data_length,
                        .data = self.module[data_start..rd.context.pos],
                        .mode = DataSegmentMode{ .Active = .{
                            .memidx = 0,
                            .offset = parsed_code.start,
                        } },
                    });
                },
                1 => {
                    const data_length = leb.readULEB128(u32, rd) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    const data_start = rd.context.pos;
                    rd.skipBytes(data_length, .{}) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    try self.datas.list.append(DataSegment{
                        .count = data_length,
                        .data = self.module[data_start..rd.context.pos],
                        .mode = .Passive,
                    });
                },
                2 => {
                    const memidx = leb.readULEB128(u32, rd) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    if (memidx >= self.memories.list.items.len) return error.ValidatorDataMemoryReferenceInvalid;

                    const expr_start = rd.context.pos;
                    const expr = self.module[expr_start..];
                    const meta = try opcode.findExprEnd(expr);

                    rd.skipBytes(meta.offset + 1, .{}) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    const data_length = leb.readULEB128(u32, rd) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    const data_start = rd.context.pos;
                    rd.skipBytes(data_length, .{}) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };

                    const parsed_code = try self.parseConstantCode(self.module[expr_start .. expr_start + meta.offset + 1], .I32);

                    try self.datas.list.append(DataSegment{
                        .count = data_length,
                        .data = self.module[data_start..rd.context.pos],
                        .mode = DataSegmentMode{ .Active = .{
                            .memidx = 0,
                            .offset = parsed_code.start,
                        } },
                    });
                },
                else => {
                    std.log.err("unknown data section type {}", .{data_section_type});
                    return error.UnknownDataSectionType;
                },
            }
        }
    }

    fn decodeCustomSection(self: *Module, size: u32) !void {
        const rd = self.buf.reader();
        const offset = rd.context.pos;

        const name_length = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };

        const name_start = rd.context.pos;
        rd.skipBytes(name_length, .{}) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };

        const name = self.module[name_start .. name_start + name_length];

        if (!unicode.utf8ValidateSlice(name)) return error.NameNotUTF8;

        const data_start = rd.context.pos;
        const data_size = try math.sub(usize, size, (rd.context.pos - offset));
        rd.skipBytes(data_size, .{}) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };

        const data = self.module[data_start .. data_start + data_size];

        try self.customs.list.append(Custom{
            .name = name,
            .data = data,
        });
    }

    const StartWithDepth = struct {
        start: usize,
        max_depth: usize,
    };

    pub fn parseConstantCode(self: *Module, code: []const u8, valtype: ValType) !StartWithDepth {
        _ = try opcode.findFunctionEnd(code);
        var continuation_stack: [1024]usize = [_]usize{0} ** 1024;
        const code_start = self.parsed_code.items.len;

        var it = Parser.init(self, code, &self.parsed_code, continuation_stack[0..], true);

        const in: [0]ValType = [_]ValType{} ** 0;
        const out: [1]ValType = [_]ValType{valtype} ** 1;

        try it.validator.pushControlFrame(
            .block,
            in[0..0],
            out[0..1],
        );

        while (try it.next()) |instr| {
            switch (instr) {
                .@"i32.const",
                .@"i64.const",
                .@"f32.const",
                .@"f64.const",
                .@"global.get",
                .@"ref.null",
                .@"ref.func",
                .end,
                => |_| {},
                else => return error.ValidatorConstantExpressionRequired,
            }
            try self.parsed_code.append(instr);
        }

        // Patch last end so that it is return
        self.parsed_code.items[self.parsed_code.items.len - 1] = .@"return";

        return StartWithDepth{ .start = code_start, .max_depth = it.validator.max_depth };
    }

    pub fn parseFunction(self: *Module, locals: []LocalType, code: []const u8, funcidx: usize) !StartWithDepth {
        _ = try opcode.findFunctionEnd(code);
        var continuation_stack: [1024]usize = [_]usize{0} ** 1024;
        const code_start = self.parsed_code.items.len;

        var it = Parser.init(self, code, &self.parsed_code, continuation_stack[0..], false);

        try it.pushFunction(locals, funcidx);

        while (try it.next()) |instr| {
            try self.parsed_code.append(instr);
        }

        // Patch last end so that it is return
        self.parsed_code.items[self.parsed_code.items.len - 1] = .@"return";

        return StartWithDepth{ .start = code_start, .max_depth = it.validator.max_depth };
    }

    pub fn getExport(self: *Module, tag: Tag, name: []const u8) !usize {
        for (self.exports.list.items) |exported| {
            if (tag == exported.tag and mem.eql(u8, name, exported.name)) return exported.index;
        }

        return error.ExportNotFound;
    }

    pub fn print(module: *Module) void {
        std.debug.print("    Types: {}\n", .{module.types.list.items.len});
        std.debug.print("Functions: {}\n", .{module.functions.list.items.len});
        std.debug.print("   Tables: {}\n", .{module.tables.list.items.len});
        std.debug.print(" Memories: {}\n", .{module.memories.list.items.len});
        std.debug.print("  Globals: {}\n", .{module.globals.list.items.len});
        std.debug.print("  Exports: {}\n", .{module.exports.list.items.len});
        std.debug.print("  Imports: {}\n", .{module.imports.list.items.len});
        std.debug.print("    Codes: {}\n", .{module.codes.list.items.len});
        std.debug.print("    Datas: {}\n", .{module.datas.list.items.len});
        std.debug.print("  Customs: {}\n", .{module.customs.list.items.len});
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

        pub fn itemsSlice(self: *Self) []T {
            return self.list.items;
        }

        pub fn lookup(self: *Self, idx: u32) !T {
            if (idx >= self.list.items.len) return error.ValidatorInvalidIdx;

            return self.list.items[idx];
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

    var new_inst = Instance.init(alloc, &store, module);
    const index = try store.addInstance(new_inst);
    var inst = try store.instance(index);
    try inst.instantiate(index);

    var in = [2]u64{ 22, 23 };
    var out = [1]u64{0};
    try inst.invoke("add", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 45), @bitCast(i32, @truncate(u32, out[0])));
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

    var new_inst = Instance.init(alloc, &store, module);
    const index = try store.addInstance(new_inst);
    var inst = try store.instance(index);
    try inst.instantiate(index);

    var in = [1]u64{0};
    var out = [1]u64{0};
    try inst.invoke("fib", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 0), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 1;
    try inst.invoke("fib", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 1), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 2;
    try inst.invoke("fib", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 1), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 3;
    try inst.invoke("fib", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 2), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 4;
    try inst.invoke("fib", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 3), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 5;
    try inst.invoke("fib", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 5), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 6;
    try inst.invoke("fib", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 8), @bitCast(i32, @truncate(u32, out[0])));
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

    var new_inst = Instance.init(alloc, &store, module);
    const index = try store.addInstance(new_inst);
    var inst = try store.instance(index);
    try inst.instantiate(index);

    var in = [1]u64{1};
    var out = [1]u64{0};
    try inst.invoke("fact", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 1), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 2;
    try inst.invoke("fact", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 2), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 3;
    try inst.invoke("fact", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 6), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 4;
    try inst.invoke("fact", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 24), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 12;
    try inst.invoke("fact", in[0..], out[0..], .{});
    try testing.expectEqual(@as(i32, 479001600), @bitCast(i32, @truncate(u32, out[0])));
}
