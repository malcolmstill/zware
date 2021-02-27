const std = @import("std");
const mem = std.mem;
const leb = std.leb;
const math = std.math;
const unicode = std.unicode;
const common = @import("common.zig");
const instruction = @import("instruction.zig");
const RuntimeInstruction = @import("function.zig").RuntimeInstruction;
const Instance = @import("instance.zig").Instance;
const ArrayList = std.ArrayList;
const Instruction = instruction.Instruction;
const ParseIterator = instruction.ParseIterator;
const InstructionIterator = instruction.InstructionIterator;
const FuncType = common.FuncType;
const ValueType = common.ValueType;
const Import = common.Import;
const Export = common.Export;
const Limit = common.Limit;
const LimitType = common.LimitType;
const Mutability = common.Mutability;
const Global = common.Global;
const Element = common.Element;
const Code = function.Code;
const Segment = common.Segment;
const Tag = common.Tag;
const Interpreter = @import("interpreter.zig").Interpreter;
const Store = @import("store.zig").ArrayListStore;
const Function = @import("function.zig").Function;
const function = @import("function.zig");

pub const Module = struct {
    decoded: bool = false,
    alloc: *mem.Allocator,
    module: []const u8 = undefined,
    buf: std.io.FixedBufferStream([]const u8) = undefined,
    version: u32 = 0,
    customs: Section(Custom),
    types: Section(FuncType),
    imports: Section(Import),
    functions: Section(common.Function),
    tables: Section(Limit),
    memories: Section(Limit),
    globals: Section(Global),
    exports: Section(Export),
    start: ?u32,
    elements: Section(Segment),
    codes: Section(Code),
    datas: Section(Segment),
    parsed_code: ArrayList(RuntimeInstruction),
    br_table_indices: ArrayList(u32),

    pub fn init(alloc: *mem.Allocator, module: []const u8) Module {
        return Module{
            .alloc = alloc,
            .module = module,
            .buf = std.io.fixedBufferStream(module),
            .customs = Section(Custom).init(alloc),
            .types = Section(FuncType).init(alloc),
            .imports = Section(Import).init(alloc),
            .functions = Section(common.Function).init(alloc),
            .tables = Section(Limit).init(alloc),
            .memories = Section(Limit).init(alloc),
            .globals = Section(Global).init(alloc),
            .exports = Section(Export).init(alloc),
            .start = null,
            .elements = Section(Segment).init(alloc),
            .codes = Section(Code).init(alloc),
            .datas = Section(Segment).init(alloc),
            .parsed_code = ArrayList(RuntimeInstruction).init(alloc),
            .br_table_indices = ArrayList(u32).init(alloc),
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

    pub fn decodeSection(self: *Module) !SectionType {
        const rd = self.buf.reader();

        const id: SectionType = rd.readEnum(SectionType, .Little) catch |err| switch (err) {
            error.InvalidValue => return error.UnknownSectionId,
            else => return err,
        };

        const size = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };

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
                    const v = rd.readEnum(ValueType, .Little) catch |err| switch (err) {
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
                    const r = rd.readEnum(ValueType, .Little) catch |err| switch (err) {
                        error.EndOfStream => return error.UnexpectedEndOfInput,
                        else => return err,
                    };
                }
            }
            const results_end = rd.context.pos;

            var params = self.module[params_start..params_end];
            var results = self.module[results_start..results_end];

            var params_value_type: []const ValueType = undefined;
            params_value_type.ptr = @ptrCast([*]const ValueType, params.ptr);
            params_value_type.len = params.len;

            var results_value_type: []const ValueType = undefined;
            results_value_type.ptr = @ptrCast([*]const ValueType, results.ptr);
            results_value_type.len = results.len;

            try self.types.list.append(FuncType{
                .params = params_value_type,
                .results = results_value_type,
            });
        }

        return count;
    }

    fn decodeImportSection(self: *Module) !usize {
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

        return count;
    }

    fn decodeFunctionSection(self: *Module) !usize {
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

        return count;
    }

    fn decodeFunction(self: *Module, import: ?u32) !void {
        const rd = self.buf.reader();
        const type_index = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };

        try self.functions.list.append(common.Function{
            .typeidx = type_index,
            .import = import,
        });
    }

    fn decodeTableSection(self: *Module) !usize {
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

        return count;
    }

    fn decodeTable(self: *Module, import: ?u32) !void {
        const rd = self.buf.reader();

        const tag = rd.readByte() catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
        if (tag != 0x70) return error.ExpectedTable;

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

                try self.tables.list.append(Limit{
                    .min = min,
                    .max = null,
                    .import = import,
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

        const count = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
        self.memories.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            try self.decodeMemory(null);
        }

        return count;
    }

    fn decodeMemory(self: *Module, import: ?u32) !void {
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

                try self.memories.list.append(Limit{
                    .min = min,
                    .max = null,
                    .import = import,
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

        const count = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
        self.globals.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            try self.decodeGlobal(null);
        }

        return count;
    }

    fn decodeGlobal(self: *Module, import: ?u32) !void {
        const rd = self.buf.reader();

        const global_type = rd.readEnum(ValueType, .Little) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };

        const mutability = rd.readEnum(Mutability, .Little) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
        const offset = rd.context.pos;

        var code: ?[]const u8 = null;
        var parsed_code: ?[]RuntimeInstruction = null;

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

                if (byte == @enumToInt(Instruction.end)) break;
            }
            code = self.module[offset .. offset + j + 1];

            parsed_code = try self.parseCode(code orelse return error.NoCode);
        }

        if (code == null and import == null) return error.ExpectedOneOrTheOther;
        if (code != null and import != null) return error.ExpectedOneOrTheOther;

        try self.globals.list.append(Global{
            .value_type = global_type,
            .mutability = mutability,
            .code = parsed_code,
            .import = import,
        });
    }

    fn decodeExportSection(self: *Module) !usize {
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

            const tag = rd.readEnum(Tag, .Little) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            const index = leb.readULEB128(u32, rd) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

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

        const index = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };

        self.start = index;

        return 1;
    }

    fn decodeElementSection(self: *Module, size: u32) !usize {
        const rd = self.buf.reader();

        const count = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
        self.elements.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const table_index = leb.readULEB128(u32, rd) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            const expr_start = rd.context.pos;
            const expr = self.module[expr_start..];
            const meta = try instruction.findExprEnd(true, expr);

            rd.skipBytes(meta.offset + 1, .{}) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            // Number of u32's in our data (not the length in bytes!)
            const data_length = leb.readULEB128(u32, rd) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };
            const data_start = rd.context.pos;

            var j: usize = 0;
            while (j < data_length) : (j += 1) {
                // When we pre-process all this data we can just store this
                // but for the moment we're just using it to skip over
                _ = leb.readULEB128(u32, rd) catch |err| switch (err) {
                    error.EndOfStream => return error.UnexpectedEndOfInput,
                    else => return err,
                };
            }

            const parsed_code = try self.parseCode(self.module[expr_start .. expr_start + meta.offset + 1]);

            try self.elements.list.append(Segment{
                .index = table_index,
                .offset = parsed_code,
                .count = data_length,
                .data = self.module[data_start..rd.context.pos],
            });
        }

        return 1;
    }

    fn decodeCodeSection(self: *Module) !usize {
        const rd = self.buf.reader();

        const count = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
        self.codes.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const size = leb.readULEB128(u32, rd) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            }; // includes bytes defining locals

            const offset = rd.context.pos;

            const locals_definitions_count = leb.readULEB128(u32, rd) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            const locals_start = rd.context.pos;

            // Iterate over local definitions counting them
            var j: usize = 0;
            var locals_count: usize = 0;
            while (j < locals_definitions_count) : (j += 1) {
                const type_count = leb.readULEB128(u32, rd) catch |err| switch (err) {
                    error.EndOfStream => return error.UnexpectedEndOfInput,
                    else => return err,
                };

                const local_type = rd.readByte() catch |err| switch (err) {
                    error.EndOfStream => return error.UnexpectedEndOfInput,
                    else => return err,
                };
                locals_count += type_count;
            }
            if (locals_count > 0x100000000) return error.TooManyLocals;

            const code_start = rd.context.pos;
            const code_length = try math.sub(usize, size, code_start - offset);

            rd.skipBytes(code_length, .{}) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            const locals = self.module[locals_start..code_start];
            const code = self.module[code_start..rd.context.pos];

            const parsed_code = try self.parseCode(code);

            try self.codes.list.append(Code{
                .locals = locals,
                .locals_count = locals_count,
                .code = parsed_code,
            });
        }

        return count;
    }

    fn decodeDataSection(self: *Module, size: u32) !usize {
        const rd = self.buf.reader();
        var section_offset = rd.context.pos;

        const count = leb.readULEB128(u32, rd) catch |err| switch (err) {
            error.EndOfStream => return error.UnexpectedEndOfInput,
            else => return err,
        };
        self.datas.count = count;

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const mem_idx = leb.readULEB128(u32, rd) catch |err| switch (err) {
                error.EndOfStream => return error.UnexpectedEndOfInput,
                else => return err,
            };

            const expr_start = rd.context.pos;
            const expr = self.module[expr_start..];
            const meta = try instruction.findExprEnd(true, expr);

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

            const parsed_code = try self.parseCode(self.module[expr_start .. expr_start + meta.offset + 1]);

            try self.datas.list.append(Segment{
                .index = mem_idx,
                .offset = parsed_code,
                .count = data_length,
                .data = self.module[data_start..rd.context.pos],
            });
        }

        return count;
    }

    fn decodeCustomSection(self: *Module, size: u32) !usize {
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

        return 1;
    }

    pub fn parseCode(self: *Module, code: []const u8) ![]RuntimeInstruction {
        var it = ParseIterator.init(self, code);
        const code_start = self.parsed_code.items.len;

        // 1. Make a first pass allocating all of our RuntimeInstructions
        while (try it.next(true)) |meta| {
            try self.parsed_code.append(meta.runtime_instruction);
        }

        var parsed_code = self.parsed_code.items[code_start..self.parsed_code.items.len];

        // 2. Make a second pass where we fix up the continuations for
        //    blocks, loops and ifs
        try function.calculateContinuations(parsed_code);

        return parsed_code;
    }

    pub fn getExport(self: *Module, tag: Tag, name: []const u8) !usize {
        for (self.exports.list.items) |exported| {
            if (tag == exported.tag and mem.eql(u8, name, exported.name)) return exported.index;
        }

        return error.ExportNotFound;
    }

    pub fn signaturesEqual(self: *Module, params: []const ValueType, results: []const ValueType, b: FuncType) bool {
        if (params.len != b.params.len) return false;
        if (results.len != b.results.len) return false;

        for (params) |p_a, i| {
            if (p_a != b.params[i]) return false;
        }

        for (results) |r_a, i| {
            if (r_a != b.results[i]) return false;
        }

        return true;
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

    var store: Store = Store.init(&arena.allocator);

    var module = Module.init(&arena.allocator, bytes);
    try module.decode();

    var inst = Instance.init(&arena.allocator, &store, module);
    try inst.instantiate();

    var in = [2]u64{ 22, 23 };
    var out = [1]u64{0};
    try inst.invoke("add", in[0..], out[0..], .{});
    testing.expectEqual(@as(i32, 45), @bitCast(i32, @truncate(u32, out[0])));
}

test "module loading (fib)" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer _ = arena.deinit();

    const bytes = @embedFile("../test/fib.wasm");

    var store: Store = Store.init(&arena.allocator);

    var module = Module.init(&arena.allocator, bytes);
    try module.decode();

    var inst = Instance.init(&arena.allocator, &store, module);
    try inst.instantiate();

    var in = [1]u64{0};
    var out = [1]u64{0};
    try inst.invoke("fib", in[0..], out[0..], .{});
    testing.expectEqual(@as(i32, 1), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 1;
    try inst.invoke("fib", in[0..], out[0..], .{});
    testing.expectEqual(@as(i32, 1), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 2;
    try inst.invoke("fib", in[0..], out[0..], .{});
    testing.expectEqual(@as(i32, 2), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 3;
    try inst.invoke("fib", in[0..], out[0..], .{});
    testing.expectEqual(@as(i32, 3), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 4;
    try inst.invoke("fib", in[0..], out[0..], .{});
    testing.expectEqual(@as(i32, 5), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 5;
    try inst.invoke("fib", in[0..], out[0..], .{});
    testing.expectEqual(@as(i32, 8), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 6;
    try inst.invoke("fib", in[0..], out[0..], .{});
    testing.expectEqual(@as(i32, 13), @bitCast(i32, @truncate(u32, out[0])));
}

test "module loading (fact)" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer _ = arena.deinit();

    const bytes = @embedFile("../test/fact.wasm");

    var store: Store = Store.init(&arena.allocator);

    var module = Module.init(&arena.allocator, bytes);
    try module.decode();

    var inst = Instance.init(&arena.allocator, &store, module);
    try inst.instantiate();

    var in = [1]u64{1};
    var out = [1]u64{0};
    try inst.invoke("fact", in[0..], out[0..], .{});
    testing.expectEqual(@as(i32, 1), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 2;
    try inst.invoke("fact", in[0..], out[0..], .{});
    testing.expectEqual(@as(i32, 2), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 3;
    try inst.invoke("fact", in[0..], out[0..], .{});
    testing.expectEqual(@as(i32, 6), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 4;
    try inst.invoke("fact", in[0..], out[0..], .{});
    testing.expectEqual(@as(i32, 24), @bitCast(i32, @truncate(u32, out[0])));

    in[0] = 12;
    try inst.invoke("fact", in[0..], out[0..], .{});
    testing.expectEqual(@as(i32, 479001600), @bitCast(i32, @truncate(u32, out[0])));
}
