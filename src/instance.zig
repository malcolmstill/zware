const std = @import("std");
const mem = std.mem;
const math = std.math;
const common = @import("common.zig");
const instruction = @import("instruction.zig");
const Function = @import("function.zig").Function;
const Module = @import("module.zig").Module;
const Store = @import("store.zig").ArrayListStore;
const Memory = @import("memory.zig").Memory;
const Table = @import("table.zig").Table;
const Global = @import("global.zig").Global;
const Interpreter = @import("interpreter.zig").Interpreter;
const ArrayList = std.ArrayList;
const Instruction = @import("function.zig").Instruction;

const InterpreterOptions = struct {
    operand_stack_size: comptime_int = 1024,
    control_stack_size: comptime_int = 1024,
    label_stack_size: comptime_int = 1024,
};

// Instance
//
// An Instance represents the runtime instantiation of a particular module.
//
// It contains:
//      - a copy of the module it is an instance of
//      - a pointer to the Store shared amongst modules
//      - `memaddrs`: a set of addresses in the store to map a modules
//        definitions to those in the store
//      - `tableaddrs`: as per `memaddrs` but for tables
//      - `globaladdrs`: as per `memaddrs` but for globals
pub const Instance = struct {
    module: Module,
    store: *Store,
    funcaddrs: ArrayList(usize),
    memaddrs: ArrayList(usize),
    tableaddrs: ArrayList(usize),
    globaladdrs: ArrayList(usize),

    pub fn init(alloc: *mem.Allocator, store: *Store, module: Module) Instance {
        return Instance{
            .module = module,
            .store = store,
            .funcaddrs = ArrayList(usize).init(alloc),
            .memaddrs = ArrayList(usize).init(alloc),
            .tableaddrs = ArrayList(usize).init(alloc),
            .globaladdrs = ArrayList(usize).init(alloc),
        };
    }

    pub fn instantiate(self: *Instance, index: usize) !void {
        if (self.module.decoded == false) return error.ModuleNotDecoded;

        // 1. Initialise imports
        for (self.module.imports.list.items) |import| {
            const import_handle = try self.store.import(import.module, import.name, import.desc_tag);
            switch (import.desc_tag) {
                .Func => try self.funcaddrs.append(import_handle),
                .Mem => try self.memaddrs.append(import_handle),
                .Table => try self.tableaddrs.append(import_handle),
                .Global => try self.globaladdrs.append(import_handle),
            }
        }

        // Initialise (internal) functions
        //
        // We have two possibilities:
        // 1. the function is imported
        // 2. the function is defined in this module (i.e. there is an associatd code section)
        //
        // In the case of 1 we need to check that the type of import matches the type
        // of the actual function in the store
        //
        // For 2, we need to add the function to the store
        const imported_function_count = self.funcaddrs.items.len;
        for (self.module.functions.list.items) |function_def, i| {
            if (function_def.import) |_| {
                // Check that the function defintion (which this module expects)
                // is the same as the function in the store
                const func_type = self.module.types.list.items[function_def.typeidx];
                const external_function = try self.getFunc(i);
                switch (external_function) {
                    .function => |ef| {
                        if (!Module.signaturesEqual(ef.params, ef.results, func_type)) {
                            return error.ImportedFunctionTypeSignatureDoesNotMatch;
                        }
                    },
                    .host_function => |hef| {
                        if (!Module.signaturesEqual(hef.params, hef.results, func_type)) {
                            return error.ImportedFunctionTypeSignatureDoesNotMatch;
                        }
                    },
                }
            } else {
                // TODO: clean this up
                const code = self.module.codes.list.items[i - imported_function_count];
                const func = self.module.functions.list.items[i];
                const func_type = self.module.types.list.items[func.typeidx];
                const handle = try self.store.addFunction(Function{
                    .function = .{
                        // .code = self.module.parsed_code.items[code.code.offset .. code.code.offset + code.code.count],
                        .ip_start = code.code.offset,
                        .ip_end = code.code.offset + code.code.count,
                        // .locals = code.locals,
                        .locals_count = code.locals_count,
                        .params = func_type.params,
                        .results = func_type.results,
                        .instance = index,
                    },
                });

                // Need to do this regardless of if import or internal
                try self.funcaddrs.append(handle);
            }
        }

        // 2. Initialise globals
        for (self.module.globals.list.items) |global_def, i| {
            if (global_def.import != null) {
                const imported_global = try self.getGlobal(i);
                if (imported_global.mutability != global_def.mutability) return error.MismatchedMutability;
            } else {
                const value = if (global_def.code) |code| try self.invokeExpression(code.offset, u64, .{}) else 0;
                const handle = try self.store.addGlobal(Global{
                    .value = value,
                    .mutability = global_def.mutability,
                    .value_type = global_def.value_type,
                });
                try self.globaladdrs.append(handle);
            }
        }

        // 3a. Initialise memories
        for (self.module.memories.list.items) |mem_size, i| {
            if (mem_size.import != null) {
                const imported_mem = try self.getMemory(i);
                if (!common.limitMatch(imported_mem.min, imported_mem.max, mem_size.min, mem_size.max)) return error.ImportedMemoryNotBigEnough;
            } else {
                const handle = try self.store.addMemory(mem_size.min, mem_size.max);
                try self.memaddrs.append(handle);
            }
        }

        // 3b. Initialise tables
        for (self.module.tables.list.items) |table_size, i| {
            if (table_size.import != null) {
                const imported_table = try self.getTable(i);
                if (!common.limitMatch(imported_table.min, imported_table.max, table_size.min, table_size.max)) return error.ImportedTableNotBigEnough;
            } else {
                const handle = try self.store.addTable(table_size.min, table_size.max);
                try self.tableaddrs.append(handle);
            }
        }

        // 4a. Check all data
        for (self.module.datas.list.items) |data| {
            const handle = self.memaddrs.items[data.index];
            const memory = try self.store.memory(handle);

            const offset = try self.invokeExpression(data.offset.offset, u32, .{});
            try memory.check(offset, data.data);
        }

        // 4b. Check all elements
        for (self.module.elements.list.items) |segment| {
            const table = try self.getTable(segment.index);
            const offset = try self.invokeExpression(segment.offset.offset, u32, .{});
            if ((try math.add(u32, offset, segment.count)) > table.size()) return error.OutOfBoundsMemoryAccess;
        }

        // 5a. Mutate all data
        for (self.module.datas.list.items) |data| {
            const handle = self.memaddrs.items[data.index];
            const memory = try self.store.memory(handle);

            const offset = try self.invokeExpression(data.offset.offset, u32, .{});
            try memory.copy(offset, data.data);
        }

        // 5b. If all our elements were good, initialise them
        for (self.module.elements.list.items) |segment| {
            const table = try self.getTable(segment.index);
            const offset = try self.invokeExpression(segment.offset.offset, u32, .{});

            var data = segment.data;
            var j: usize = 0;
            while (j < segment.count) : (j += 1) {
                const value = try instruction.readULEB128Mem(u32, &data);
                try table.set(@intCast(u32, offset + j), try self.funcHandle(value));
            }
        }

        if (self.module.start) |start_function| {
            try self.invokeStart(start_function, .{});
        }
    }

    pub fn getFunc(self: *Instance, index: usize) !Function {
        if (index >= self.funcaddrs.items.len) return error.FunctionIndexOutOfBounds;
        const handle = self.funcaddrs.items[index];
        return try self.store.function(handle);
    }

    pub fn funcHandle(self: *Instance, index: usize) !usize {
        if (index >= self.funcaddrs.items.len) return error.FunctionIndexOutOfBounds;
        return self.funcaddrs.items[index];
    }

    // Lookup a memory in store via the modules index
    pub fn getMemory(self: *Instance, index: usize) !*Memory {
        // TODO: with a verified program we shouldn't need to check this
        if (index >= self.memaddrs.items.len) return error.MemoryIndexOutOfBounds;
        const handle = self.memaddrs.items[index];
        return try self.store.memory(handle);
    }

    pub fn getTable(self: *Instance, index: usize) !*Table {
        // TODO: with a verified program we shouldn't need to check this
        if (index >= self.tableaddrs.items.len) return error.TableIndexOutOfBounds;
        const handle = self.tableaddrs.items[index];
        return try self.store.table(handle);
    }

    pub fn getGlobal(self: *Instance, index: usize) !*Global {
        if (index >= self.globaladdrs.items.len) return error.GlobalIndexOutOfBounds;
        const handle = self.globaladdrs.items[index];
        return try self.store.global(handle);
    }

    // invoke
    //
    // Similar to invoke, but without some type checking
    pub fn invoke(self: *Instance, name: []const u8, in: []u64, out: []u64, comptime options: InterpreterOptions) !void {
        std.debug.warn("invoke\n", .{});
        // 1.
        const index = try self.module.getExport(.Func, name);
        if (index >= self.module.functions.list.items.len) return error.FuncIndexExceedsTypesLength;

        const function = try self.getFunc(index);

        var op_stack_mem: [options.operand_stack_size]u64 = [_]u64{0} ** options.operand_stack_size;
        var frame_stack_mem: [options.control_stack_size]Interpreter.Frame = [_]Interpreter.Frame{undefined} ** options.control_stack_size;
        var label_stack_mem: [options.label_stack_size]Interpreter.Label = [_]Interpreter.Label{undefined} ** options.control_stack_size;

        switch (function) {
            .function => |f| {
                if (f.params.len != in.len) return error.ParamCountMismatch;
                if (f.results.len != out.len) return error.ResultCountMismatch;

                // 6. set up our stacks
                var interp = Interpreter.init(op_stack_mem[0..], frame_stack_mem[0..], label_stack_mem[0..], try self.store.instance(f.instance));

                const locals_start = interp.op_ptr;

                // 7b. push params
                for (in) |arg| {
                    try interp.pushOperand(u64, arg);
                }

                // 7c. push (i.e. make space for) locals
                var i: usize = 0;
                while (i < f.locals_count) : (i += 1) {
                    try interp.pushOperand(u64, 0);
                }

                // 7a. push control frame
                try interp.pushFrame(Interpreter.Frame{
                    .op_stack_len = locals_start,
                    .label_stack_len = interp.label_ptr,
                    .return_arity = f.results.len,
                    .inst = self,
                }, f.locals_count + f.params.len);

                // 7a.2. push label for our implicit function block. We know we don't have
                // any code to execute after calling invoke, but we will need to
                // pop a Label
                try interp.pushLabel(Interpreter.Label{
                    .return_arity = f.results.len,
                    .op_stack_len = locals_start,
                    .break_target = f.ip_end - 1,
                });

                // std.debug.warn("invoke[{}, {}] = {x}\n", .{ index, handle, f.code });
                // 8. Execute our function
                interp.function_start = f.ip_start;
                try interp.invoke(f.ip_start);

                // 9.
                for (out) |_, out_index| {
                    out[out_index] = interp.popOperand(u64);
                }
            },
            .host_function => |host_func| {
                var interp = Interpreter.init(op_stack_mem[0..], frame_stack_mem[0..], label_stack_mem[0..], self);
                try host_func.func(&interp);
            },
        }
    }

    pub fn invokeStart(self: *Instance, index: u32, comptime options: InterpreterOptions) !void {
        std.debug.warn("invokeStart\n", .{});
        const function = try self.getFunc(index);

        var op_stack_mem: [options.operand_stack_size]u64 = [_]u64{0} ** options.operand_stack_size;
        var frame_stack_mem: [options.control_stack_size]Interpreter.Frame = [_]Interpreter.Frame{undefined} ** options.control_stack_size;
        var label_stack_mem: [options.label_stack_size]Interpreter.Label = [_]Interpreter.Label{undefined} ** options.control_stack_size;

        switch (function) {
            .function => |f| {
                var interp = Interpreter.init(op_stack_mem[0..], frame_stack_mem[0..], label_stack_mem[0..], try self.store.instance(f.instance));

                const locals_start = interp.op_ptr;

                var i: usize = 0;
                while (i < f.locals_count) : (i += 1) {
                    try interp.pushOperand(u64, 0);
                }

                try interp.pushFrame(Interpreter.Frame{
                    .op_stack_len = locals_start,
                    .label_stack_len = interp.label_ptr,
                    .return_arity = 0,
                    .inst = self,
                }, f.locals_count);

                try interp.pushLabel(Interpreter.Label{
                    .return_arity = 0,
                    .op_stack_len = locals_start,
                    .break_target = 0,
                });

                try interp.invoke(f.ip_start);
            },
            .host_function => |host_func| {
                var interp = Interpreter.init(op_stack_mem[0..], frame_stack_mem[0..], label_stack_mem[0..], self);
                try host_func.func(&interp);
            },
        }
    }

    pub fn invokeExpression(self: *Instance, start: usize, comptime Result: type, comptime options: InterpreterOptions) !Result {
        std.debug.warn("invokeExpression\n", .{});
        var op_stack_mem: [options.operand_stack_size]u64 = [_]u64{0} ** options.operand_stack_size;
        var frame_stack_mem: [options.control_stack_size]Interpreter.Frame = [_]Interpreter.Frame{undefined} ** options.control_stack_size;
        var label_stack_mem: [options.label_stack_size]Interpreter.Label = [_]Interpreter.Label{undefined} ** options.control_stack_size;
        var interp = Interpreter.init(op_stack_mem[0..], frame_stack_mem[0..], label_stack_mem[0..], self);

        const locals_start = interp.op_ptr;

        try interp.pushFrame(Interpreter.Frame{
            .op_stack_len = locals_start,
            .label_stack_len = interp.label_ptr,
            .return_arity = 1,
            .inst = self,
        }, 0);

        try interp.pushLabel(Interpreter.Label{
            .return_arity = 1,
            .op_stack_len = locals_start,
        });

        try interp.invoke(start);

        switch (Result) {
            u64 => return interp.popAnyOperand(),
            else => return interp.popOperand(Result),
        }
    }
};
