const std = @import("std");
const mem = std.mem;
const leb = std.debug.leb;
const ArrayList = std.ArrayList;
const Instruction = @import("instruction.zig").Instruction;
const FuncType = @import("common.zig").FuncType;
const ValueType = @import("common.zig").ValueType;
const Import = @import("common.zig").Import;
const Export = @import("common.zig").Export;
const Limit = @import("common.zig").Limit;
const LimitType = @import("common.zig").LimitType;
const Mutability = @import("common.zig").Mutability;
const Global = @import("common.zig").Global;
const Element = @import("common.zig").Element;
const Code = @import("common.zig").Code;
const Data = @import("common.zig").Data;
const Tag = @import("common.zig").Tag;
const common = @import("common.zig");
const Interpreter = @import("interpreter.zig").Interpreter;
const Store = @import("interpreter/store.zig").Store;

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
    elements: ArrayList(Element),
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
            .elements = ArrayList(Element).init(alloc),
            .codes = ArrayList(Code).init(alloc),
            .datas = ArrayList(Data).init(alloc),
        };
    }

    pub fn decode(self: *Module) !void {
        const rd = self.buf.reader();

        const magic = try rd.readBytesNoEof(4);
        if (!mem.eql(u8, magic[0..], "\x00asm")) return error.MagicNumberNotFound;

        const version = try rd.readIntLittle(u32);

        var i: usize = 0;
        while (true) : (i += 1) {
            var section = self.decodeSection() catch |err| {
                switch (err) {
                    error.EndOfStream => break,
                    else => return err,
                }
            };
        }
    }

    pub fn decodeSection(self: *Module) !SectionType {
        const rd = self.buf.reader();
        const id: SectionType = @intToEnum(SectionType, try rd.readByte());

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

    fn decodeImportSection(self: *Module) !usize {
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

    fn decodeFunctionSection(self: *Module) !usize {
        const rd = self.buf.reader();
        const count = try leb.readULEB128(u32, rd);

        var i: usize = 0;
        while (i < count) : (i += 1) {
            const type_index = try leb.readULEB128(u32, rd);
            try self.functions.append(type_index);
        }

        return count;
    }

    fn decodeTableSection(self: *Module) !usize {
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

    fn decodeMemorySection(self: *Module) !usize {
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

    fn decodeGlobalSection(self: *Module) !usize {
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

    fn decodeExportSection(self: *Module) !usize {
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

    fn decodeStartSection(self: *Module) !usize {
        const rd = self.buf.reader();
        const index = try leb.readULEB128(u32, rd);

        self.start = index;

        return 1;
    }

    fn decodeElementSection(self: *Module, size: u32) !usize {
        // , size: u32
        const rd = self.buf.reader();
        // const index = try leb.readULEB128(u32, rd);

        // self.start = index;
        try rd.skipBytes(size, .{});
        return 1;
    }

    fn decodeCodeSection(self: *Module) !usize {
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
            const locals_and_code = self.module[offset..rd.context.pos];
            const locals_definitions_count = locals_and_code[0]; // probably a leb so this won't work
            const locals_definitions = locals_and_code[1 .. 1 + 2 * locals_definitions_count];
            var j: usize = 0;
            var locals_count: usize = 0;
            while (j < locals_definitions_count) : (j += 1) {
                const definition = locals_definitions[2 * j .. 2 * j + 2];
                locals_count += definition[0];
            }

            const code = locals_and_code[1 + 2 * locals_definitions_count ..];

            try self.codes.append(Code{
                .locals = locals_definitions,
                .locals_count = locals_count,
                .code = code,
            });
        }

        return count;
    }

    fn decodeDataSection(self: *Module, size: u32) !usize {
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

    fn decodeCustomSection(self: *Module, size: u32) !usize {
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

    pub fn instantiate(self: *Module, store: *Store) !ModuleInstance {
        var inst = ModuleInstance{
            .module = self,
            .store = store,
        };

        const globals_count = self.globals.items.len;
        try inst.store.allocGlobals(globals_count);

        return inst;
    }

    pub fn print(module: *Module) void {
        std.debug.warn("    Types: {}\n", .{module.types.items.len});
        std.debug.warn("Functions: {}\n", .{module.functions.items.len});
        std.debug.warn("   Tables: {}\n", .{module.tables.items.len});
        std.debug.warn(" Memories: {}\n", .{module.memories.items.len});
        std.debug.warn("  Globals: {}\n", .{module.globals.items.len});
        std.debug.warn("  Exports: {}\n", .{module.exports.items.len});
        std.debug.warn("  Imports: {}\n", .{module.imports.items.len});
        std.debug.warn("    Codes: {}\n", .{module.codes.items.len});
        std.debug.warn("    Datas: {}\n", .{module.datas.items.len});
        std.debug.warn("  Customs: {}\n", .{module.customs.items.len});
    }
};

// Default stack sizes
const InterpreterOptions = struct {
    operand_stack_size: comptime_int = 64 * 1024,
    control_stack_size: comptime_int = 64 * 1024,
    label_stack_size: comptime_int = 64 * 1024,
};

pub const ModuleInstance = struct {
    module: *Module,
    store: *Store,

    // invoke:
    //  1. Lookup our function by name with getExport
    //  2. Get the function type signature
    //  3. Check that the incoming arguments in `args` match the function signature
    //  4. Check that the return type in `Result` matches the function signature
    //  5. Get the code for our function
    //  6. Set up the stacks (operand stack, control stack)
    //  7. Push a control frame and our parameters
    //  8. Execute our function
    //  9. Pop our result and return it
    pub fn invoke(self: *ModuleInstance, name: []const u8, args: anytype, comptime Result: type, comptime options: InterpreterOptions) !Result {
        // 1.
        const index = try self.module.getExport(.Func, name);
        if (index >= self.module.functions.items.len) return error.FuncIndexExceedsTypesLength;

        const function_type_index = self.module.functions.items[index];

        // 2.
        const func_type = self.module.types.items[function_type_index];
        const params = self.module.value_types.items[func_type.params_offset .. func_type.params_offset + func_type.params_count];
        const results = self.module.value_types.items[func_type.results_offset .. func_type.results_offset + func_type.results_count];

        if (params.len != args.len) return error.ParamCountMismatch;

        // 3. check the types of params
        inline for (args) |arg, i| {
            if (params[i] != common.toValueType(@TypeOf(arg))) return error.ParamTypeMismatch;
        }

        // 4. check the result type
        if (results.len > 1) return error.OnlySingleReturnValueSupported;
        if (Result != void and results.len == 1) {
            if (results[0] != common.toValueType(Result)) return error.ResultTypeMismatch;
        }

        // 5. get the function bytecode
        const func = self.module.codes.items[index];

        // 6. set up our stacks
        var op_stack_mem: [options.operand_stack_size]u64 = [_]u64{0} ** options.operand_stack_size;
        var frame_stack_mem: [options.control_stack_size]Interpreter.Frame = [_]Interpreter.Frame{undefined} ** options.control_stack_size;
        var label_stack_mem: [options.label_stack_size]Interpreter.Label = [_]Interpreter.Label{undefined} ** options.control_stack_size;
        var interp = Interpreter.init(op_stack_mem[0..], frame_stack_mem[0..], label_stack_mem[0..], self);

        // I think everything below here should probably live in interpret

        const locals_start = interp.op_stack.len;

        // 7b. push params
        inline for (args) |arg, i| {
            try interp.pushOperand(@TypeOf(arg), arg);
        }

        // 7c. push (i.e. make space for) locals
        var i: usize = 0;
        while (i < func.locals_count) : (i += 1) {
            try interp.pushOperand(u64, 0);
        }

        // 7a. push control frame
        try interp.pushFrame(Interpreter.Frame{
            .op_stack_len = locals_start,
            .label_stack_len = interp.label_stack.len,
            .return_arity = results.len,
        }, func.locals_count + params.len);

        // 7a.2. push label for our implicit function block. We know we don't have
        // any code to execute after calling invoke, but we will need to
        // pop a Label
        try interp.pushLabel(Interpreter.Label{
            .return_arity = results.len,
            .op_stack_len = locals_start,
            .continuation = func.code[0..0],
        });

        // 8. Execute our function
        try interp.invoke(func.code);

        // 9.
        if (Result == void) return;
        return try interp.popOperand(Result);
    }
};

const Section = struct {
    id: SectionType,
    size: u32,
    contents: []const u8,
};

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

    var store = Store.init(&arena.allocator);
    var mem0 = try store.addMemory();
    _ = try mem0.grow(1);

    var modinst = try module.instantiate(&store);

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

    var store = Store.init(&arena.allocator);
    var mem0 = try store.addMemory();
    _ = try mem0.grow(1);

    var modinst = try module.instantiate(&store);

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

    var store = Store.init(&arena.allocator);
    var mem0 = try store.addMemory();
    _ = try mem0.grow(1);

    var modinst = try module.instantiate(&store);

    testing.expectEqual(@as(i32, 1), try modinst.invoke("fact", .{@as(i32, 1)}, i32, .{}));
    testing.expectEqual(@as(i32, 2), try modinst.invoke("fact", .{@as(i32, 2)}, i32, .{}));
    testing.expectEqual(@as(i32, 6), try modinst.invoke("fact", .{@as(i32, 3)}, i32, .{}));
    testing.expectEqual(@as(i32, 24), try modinst.invoke("fact", .{@as(i32, 4)}, i32, .{}));
    testing.expectEqual(@as(i32, 479001600), try modinst.invoke("fact", .{@as(i32, 12)}, i32, .{}));
}

test "block test" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer _ = arena.deinit();

    const bytes = @embedFile("../test/testsuite/block.wasm");

    var module = Module.init(&arena.allocator, bytes);
    try module.decode();

    var store = Store.init(&arena.allocator);
    var mem0 = try store.addMemory();
    _ = try mem0.grow(1);

    var modinst = try module.instantiate(&store);

    try modinst.invoke("empty", .{}, void, .{});
    testing.expectEqual(@as(i32, 7), try modinst.invoke("singular", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 8), try modinst.invoke("multi", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 9), try modinst.invoke("nested", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 150), try modinst.invoke("deep", .{}, i32, .{}));

    testing.expectEqual(@as(i32, 1), try modinst.invoke("as-select-first", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 2), try modinst.invoke("as-select-mid", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 2), try modinst.invoke("as-select-last", .{}, i32, .{}));

    testing.expectEqual(@as(i32, 1), try modinst.invoke("as-loop-first", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 1), try modinst.invoke("as-loop-mid", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 1), try modinst.invoke("as-loop-last", .{}, i32, .{}));

    testing.expectEqual(@as(i32, 1), try modinst.invoke("as-if-then", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 2), try modinst.invoke("as-if-else", .{}, i32, .{}));

    testing.expectEqual(@as(i32, 1), try modinst.invoke("as-br_if-first", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 2), try modinst.invoke("as-br_if-last", .{}, i32, .{}));

    testing.expectEqual(@as(i32, 1), try modinst.invoke("as-br_table-first", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 2), try modinst.invoke("as-br_table-last", .{}, i32, .{}));

    // TODO: callindirect testing.expectEqual(@as(i32, 1), try modinst.invoke("as-call_indirect-first", .{}, i32, .{}));
    // TODO: callindirect testing.expectEqual(@as(i32, 2), try modinst.invoke("as-call_indirect-mid", .{}, i32, .{}));
    // TODO: callindirect testing.expectEqual(@as(i32, 1), try modinst.invoke("as-call_indirect-last", .{}, i32, .{}));

    try modinst.invoke("as-store-first", .{}, void, .{});
    try modinst.invoke("as-store-last", .{}, void, .{});

    testing.expectEqual(@as(i32, 1), try modinst.invoke("as-memory.grow-value", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 1), try modinst.invoke("as-call-value", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 1), try modinst.invoke("as-return-value", .{}, i32, .{}));
    try modinst.invoke("as-drop-operand", .{}, void, .{});
    testing.expectEqual(@as(i32, 1), try modinst.invoke("as-br-value", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 1), try modinst.invoke("as-local.set-value", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 1), try modinst.invoke("as-local.tee-value", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 1), try modinst.invoke("as-global.set-value", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 1), try modinst.invoke("as-load-operand", .{}, i32, .{}));

    testing.expectEqual(@as(i32, 0), try modinst.invoke("as-unary-operand", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 12), try modinst.invoke("as-binary-operand", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 0), try modinst.invoke("as-test-operand", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 0), try modinst.invoke("as-compare-operand", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 12), try modinst.invoke("as-binary-operands", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 0), try modinst.invoke("as-compare-operands", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 27), try modinst.invoke("as-mixed-operands", .{}, i32, .{}));

    testing.expectEqual(@as(i32, 19), try modinst.invoke("break-bare", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 18), try modinst.invoke("break-value", .{}, i32, .{}));
    // TODO: multi-value testing.expectEqual(@as(i32, 18), try modinst.invoke("break-multie-value", .{}, .{ i32, i32, i64 }, .{}));
    testing.expectEqual(@as(i32, 18), try modinst.invoke("break-repeated", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 0xf), try modinst.invoke("break-inner", .{}, i32, .{}));

    testing.expectEqual(@as(i32, 3), try modinst.invoke("param", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 3), try modinst.invoke("params", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 3), try modinst.invoke("params-id", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 3), try modinst.invoke("param-break", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 3), try modinst.invoke("params-break", .{}, i32, .{}));
    testing.expectEqual(@as(i32, 3), try modinst.invoke("params-id-break", .{}, i32, .{}));

    testing.expectEqual(@as(u32, 1), try modinst.invoke("effects", .{}, u32, .{}));
    try modinst.invoke("type-use", .{}, void, .{});
}

test "i32 test" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer _ = arena.deinit();

    const bytes = @embedFile("../test/testsuite/i32.wasm");

    var module = Module.init(&arena.allocator, bytes);
    try module.decode();

    var store = Store.init(&arena.allocator);
    var mem0 = try store.addMemory();
    _ = try mem0.grow(1);

    var modinst = try module.instantiate(&store);

    testing.expectEqual(@as(i32, 2), try modinst.invoke("add", .{ @as(i32, 1), @as(i32, 1) }, i32, .{}));
    testing.expectEqual(@as(i32, 1), try modinst.invoke("add", .{ @as(i32, 1), @as(i32, 0) }, i32, .{}));
    testing.expectEqual(@as(i32, -2), try modinst.invoke("add", .{ @as(i32, -1), @as(i32, -1) }, i32, .{}));
    testing.expectEqual(@as(i32, 0), try modinst.invoke("add", .{ @as(i32, -1), @as(i32, 1) }, i32, .{}));
    testing.expectEqual(@as(u32, 0x80000000), try modinst.invoke("add", .{ @as(i32, 0x7fffffff), @as(u32, 1) }, u32, .{}));
    testing.expectEqual(@as(i32, 0), try modinst.invoke("add", .{ @as(u32, 0x80000000), @as(u32, 0x80000000) }, i32, .{}));
    testing.expectEqual(@as(i32, 0x40000000), try modinst.invoke("add", .{ @as(u32, 0x3fffffff), @as(u32, 1) }, i32, .{}));

    testing.expectEqual(@as(i32, 0), try modinst.invoke("sub", .{ @as(u32, 1), @as(u32, 1) }, i32, .{}));
    testing.expectEqual(@as(i32, 1), try modinst.invoke("sub", .{ @as(u32, 1), @as(u32, 0) }, i32, .{}));
    testing.expectEqual(@as(i32, 0), try modinst.invoke("sub", .{ @as(i32, -1), @as(i32, -1) }, i32, .{}));
    testing.expectEqual(@as(u32, 0x80000000), try modinst.invoke("sub", .{ @as(i32, 0x7fffffff), @as(i32, -1) }, u32, .{}));
    testing.expectEqual(@as(u32, 0x7fffffff), try modinst.invoke("sub", .{ @as(u32, 0x80000000), @as(i32, 1) }, u32, .{}));
    testing.expectEqual(@as(u32, 0), try modinst.invoke("sub", .{ @as(u32, 0x80000000), @as(u32, 0x80000000) }, u32, .{}));
    testing.expectEqual(@as(u32, 0x40000000), try modinst.invoke("sub", .{ @as(u32, 0x3fffffff), @as(i32, -1) }, u32, .{}));

    testing.expectEqual(@as(i32, 1), try modinst.invoke("mul", .{ @as(u32, 1), @as(u32, 1) }, i32, .{}));
    testing.expectEqual(@as(i32, 0), try modinst.invoke("mul", .{ @as(u32, 1), @as(u32, 0) }, i32, .{}));
    testing.expectEqual(@as(i32, 1), try modinst.invoke("mul", .{ @as(i32, -1), @as(i32, -1) }, i32, .{}));
    testing.expectEqual(@as(u32, 0), try modinst.invoke("mul", .{ @as(i32, 0x10000000), @as(i32, 4096) }, u32, .{}));
    testing.expectEqual(@as(u32, 0), try modinst.invoke("mul", .{ @as(u32, 0x80000000), @as(i32, 0) }, u32, .{}));
    testing.expectEqual(@as(u32, 0x80000000), try modinst.invoke("mul", .{ @as(u32, 0x80000000), @as(i32, -1) }, u32, .{}));
    testing.expectEqual(@as(u32, 0x80000001), try modinst.invoke("mul", .{ @as(u32, 0x7fffffff), @as(i32, -1) }, u32, .{}));
    testing.expectEqual(@as(u32, 0x358e7470), try modinst.invoke("mul", .{ @as(u32, 0x01234567), @as(i32, 0x76543210) }, u32, .{}));
    testing.expectEqual(@as(u32, 1), try modinst.invoke("mul", .{ @as(u32, 0x7fffffff), @as(i32, 0x7fffffff) }, u32, .{}));

    testing.expectError(error.DivisionByZero, modinst.invoke("div_s", .{ @as(u32, 1), @as(i32, 0) }, u32, .{}));
    testing.expectError(error.DivisionByZero, modinst.invoke("div_s", .{ @as(u32, 0), @as(i32, 0) }, u32, .{}));
    testing.expectError(error.Overflow, modinst.invoke("div_s", .{ @as(u32, 0x80000000), @as(i32, -1) }, u32, .{}));
    testing.expectError(error.DivisionByZero, modinst.invoke("div_s", .{ @as(u32, 0x80000000), @as(i32, 0) }, u32, .{}));
    testing.expectEqual(@as(u32, 1), try modinst.invoke("div_s", .{ @as(u32, 1), @as(i32, 1) }, u32, .{}));
    testing.expectEqual(@as(u32, 0), try modinst.invoke("div_s", .{ @as(u32, 0), @as(i32, 1) }, u32, .{}));
    testing.expectEqual(@as(u32, 0), try modinst.invoke("div_s", .{ @as(u32, 0), @as(i32, -1) }, u32, .{}));
    testing.expectEqual(@as(u32, 1), try modinst.invoke("div_s", .{ @as(i32, -1), @as(i32, -1) }, u32, .{}));
    testing.expectEqual(@as(u32, 0xc0000000), try modinst.invoke("div_s", .{ @as(u32, 0x80000000), @as(i32, 2) }, u32, .{}));
    testing.expectEqual(@as(u32, 0xffdf3b65), try modinst.invoke("div_s", .{ @as(u32, 0x80000001), @as(i32, 1000) }, u32, .{}));
    testing.expectEqual(@as(u32, 2), try modinst.invoke("div_s", .{ @as(u32, 5), @as(i32, 2) }, u32, .{}));
    testing.expectEqual(@as(i32, -2), try modinst.invoke("div_s", .{ @as(i32, -5), @as(i32, 2) }, i32, .{}));
    testing.expectEqual(@as(i32, -2), try modinst.invoke("div_s", .{ @as(i32, 5), @as(i32, -2) }, i32, .{}));
    testing.expectEqual(@as(i32, 2), try modinst.invoke("div_s", .{ @as(i32, -5), @as(i32, -2) }, i32, .{}));
    testing.expectEqual(@as(i32, 2), try modinst.invoke("div_s", .{ @as(i32, 7), @as(i32, 3) }, i32, .{}));
    testing.expectEqual(@as(i32, -2), try modinst.invoke("div_s", .{ @as(i32, -7), @as(i32, 3) }, i32, .{}));
    testing.expectEqual(@as(i32, -2), try modinst.invoke("div_s", .{ @as(i32, 7), @as(i32, -3) }, i32, .{}));
    testing.expectEqual(@as(i32, 2), try modinst.invoke("div_s", .{ @as(i32, -7), @as(i32, -3) }, i32, .{}));
    testing.expectEqual(@as(i32, 2), try modinst.invoke("div_s", .{ @as(i32, 11), @as(i32, 5) }, i32, .{}));
    testing.expectEqual(@as(i32, 2), try modinst.invoke("div_s", .{ @as(i32, 17), @as(i32, 7) }, i32, .{}));

    testing.expectError(error.DivisionByZero, modinst.invoke("div_u", .{ @as(u32, 1), @as(i32, 0) }, u32, .{}));
    testing.expectError(error.DivisionByZero, modinst.invoke("div_u", .{ @as(u32, 0), @as(i32, 0) }, u32, .{}));
    testing.expectEqual(@as(u32, 1), try modinst.invoke("div_u", .{ @as(u32, 1), @as(i32, 1) }, u32, .{}));
    testing.expectEqual(@as(u32, 0), try modinst.invoke("div_u", .{ @as(u32, 0), @as(i32, 1) }, u32, .{}));
    testing.expectEqual(@as(u32, 1), try modinst.invoke("div_u", .{ @as(i32, -1), @as(i32, -1) }, u32, .{}));
    testing.expectEqual(@as(u32, 0), try modinst.invoke("div_u", .{ @as(u32, 0x80000000), @as(i32, -1) }, u32, .{}));
    testing.expectEqual(@as(u32, 0x40000000), try modinst.invoke("div_u", .{ @as(u32, 0x80000000), @as(i32, 2) }, u32, .{}));
    testing.expectEqual(@as(u32, 0x8fef), try modinst.invoke("div_u", .{ @as(u32, 0x8ff00ff0), @as(i32, 0x10001) }, u32, .{}));
    testing.expectEqual(@as(u32, 0x20c49b), try modinst.invoke("div_u", .{ @as(u32, 0x80000001), @as(i32, 1000) }, u32, .{}));
    testing.expectEqual(@as(u32, 2), try modinst.invoke("div_u", .{ @as(u32, 5), @as(i32, 2) }, u32, .{}));
    testing.expectEqual(@as(u32, 0x7ffffffd), try modinst.invoke("div_u", .{ @as(i32, -5), @as(i32, 2) }, u32, .{}));
    testing.expectEqual(@as(u32, 0), try modinst.invoke("div_u", .{ @as(i32, 5), @as(i32, -2) }, u32, .{}));
    testing.expectEqual(@as(u32, 0), try modinst.invoke("div_u", .{ @as(i32, -5), @as(i32, -2) }, u32, .{}));
    testing.expectEqual(@as(u32, 2), try modinst.invoke("div_u", .{ @as(i32, 7), @as(i32, 3) }, u32, .{}));
    testing.expectEqual(@as(u32, 2), try modinst.invoke("div_u", .{ @as(i32, 11), @as(i32, 5) }, u32, .{}));
    testing.expectEqual(@as(u32, 2), try modinst.invoke("div_u", .{ @as(i32, 17), @as(i32, 7) }, u32, .{}));
}
