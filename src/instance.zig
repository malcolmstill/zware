const std = @import("std");
const mem = std.mem;
const math = std.math;
const common = @import("common.zig");
const instruction = @import("instruction.zig");
const opcode = @import("opcode.zig");
const Function = @import("function.zig").Function;
const Module = @import("module.zig").Module;
const Store = @import("store.zig").ArrayListStore;
const Memory = @import("memory.zig").Memory;
const Table = @import("table.zig").Table;
const Global = @import("global.zig").Global;
const VirtualMachine = @import("vm.zig").VirtualMachine;
const ArrayList = std.ArrayList;
const Instruction = @import("function.zig").Instruction;

const VirtualMachineOptions = struct {
    frame_stack_size: comptime_int = 1024,
    label_stack_size: comptime_int = 1024,
    operand_stack_size: comptime_int = 1024,
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

    pub fn init(alloc: mem.Allocator, store: *Store, module: Module) Instance {
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

        try self.instantiateImports();
        try self.instantiateFunctions(index);
        try self.instantiateGlobals();
        try self.instantiateMemories();
        try self.instantiateTables();
        try self.checkData();
        // try self.checkElements(); // FIXME: remove once all tests passing
        try self.instantiateData();
        try self.instantiateElements();

        if (self.module.start) |start_function| {
            try self.invokeStart(start_function, .{});
        }
    }

    fn instantiateImports(self: *Instance) !void {
        for (self.module.imports.list.items) |import| {
            const import_handle = try self.store.import(import.module, import.name, import.desc_tag);
            switch (import.desc_tag) {
                .Func => try self.funcaddrs.append(import_handle),
                .Mem => try self.memaddrs.append(import_handle),
                .Table => try self.tableaddrs.append(import_handle),
                .Global => try self.globaladdrs.append(import_handle),
            }
        }
    }

    fn instantiateFunctions(self: *Instance, index: usize) !void {
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
                        .start = code.start,
                        .required_stack_space = code.required_stack_space,
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
    }

    fn instantiateGlobals(self: *Instance) !void {
        // 2. Initialise globals
        for (self.module.globals.list.items) |global_def, i| {
            if (global_def.import != null) {
                const imported_global = try self.getGlobal(i);
                if (imported_global.mutability != global_def.mutability) return error.MismatchedMutability;
                if (imported_global.value_type != global_def.value_type) return error.MismatchedGlobalType;
            } else {
                const value = if (global_def.start) |start| try self.invokeExpression(start, u64, .{}) else 0;
                const handle = try self.store.addGlobal(Global{
                    .value = value,
                    .mutability = global_def.mutability,
                    .value_type = global_def.value_type,
                });
                try self.globaladdrs.append(handle);
            }
        }
    }

    fn instantiateMemories(self: *Instance) !void {
        // 3a. Initialise memories
        for (self.module.memories.list.items) |memtype, i| {
            if (memtype.import != null) {
                const imported_mem = try self.getMemory(i);
                // Use the current size of the imported mem as min (rather than imported_mem.min). See https://github.com/WebAssembly/spec/pull/1293
                if (!common.limitMatch(imported_mem.size(), imported_mem.max, memtype.limits.min, memtype.limits.max)) return error.ImportedMemoryNotBigEnough;
            } else {
                const handle = try self.store.addMemory(memtype.limits.min, memtype.limits.max);
                try self.memaddrs.append(handle);
            }
        }
    }

    fn instantiateTables(self: *Instance) !void {
        // 3b. Initialise tables
        for (self.module.tables.list.items) |tabletype, i| {
            if (tabletype.import != null) {
                const imported_table = try self.getTable(i);
                if (imported_table.reftype != tabletype.reftype) return error.ImportedTableRefTypeMismatch;
                if (!common.limitMatch(imported_table.min, imported_table.max, tabletype.limits.min, tabletype.limits.max)) return error.ImportedTableNotBigEnough;
            } else {
                const handle = try self.store.addTable(tabletype.reftype, tabletype.limits.min, tabletype.limits.max);
                try self.tableaddrs.append(handle);
            }
        }
    }

    fn checkData(self: *Instance) !void {
        // 4a. Check all data
        for (self.module.datas.list.items) |data| {
            const handle = self.memaddrs.items[data.index];
            const memory = try self.store.memory(handle);

            const offset = try self.invokeExpression(data.start, u32, .{});
            try memory.check(offset, data.data);
        }
    }

    // fn checkElements(self: *Instance) !void {
    //     // 4b. Check all elements
    //     for (self.module.elements.list.items) |segment| {
    //         std.log.info("checkElements = {any}", .{segment});
    //         switch (segment.mode) {
    //             .Passive, .Declarative => continue,
    //             .Active => |meta| {
    //                 const table = try self.getTable(meta.tableidx);
    //                 const offset = try self.invokeExpression(meta.offset, u32, .{});

    //                 const index = math.add(u32, offset, segment.count) catch return error.OutOfBoundsMemoryAccess;
    //                 if (index > table.size()) return error.OutOfBoundsMemoryAccess;
    //             },
    //         }
    //     }
    // }

    fn instantiateData(self: *Instance) !void {
        // 5a. Mutate all data
        for (self.module.datas.list.items) |data| {
            const handle = self.memaddrs.items[data.index];
            const memory = try self.store.memory(handle);

            const offset = try self.invokeExpression(data.start, u32, .{});
            try memory.copy(offset, data.data);
        }
    }

    fn instantiateElements(self: *Instance) !void {
        // 5b. If all our elements were good, initialise them
        for (self.module.elements.list.items) |segment| {
            switch (segment.mode) {
                .Passive, .Declarative => continue,
                .Active => |meta| {
                    const table = try self.getTable(meta.tableidx);
                    const offset = try self.invokeExpression(meta.offset, u32, .{});

                    const index = math.add(u32, offset, segment.count) catch return error.OutOfBoundsMemoryAccess;
                    if (index > table.size()) return error.OutOfBoundsMemoryAccess;

                    for (self.module.element_init_offsets.items[segment.init .. segment.init + segment.count]) |expr, j| {
                        const funcidx = try self.invokeExpression(expr, u32, .{});

                        try table.set(@intCast(u32, offset + j), try self.funcHandle(funcidx));
                    }
                },
            }
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
    pub fn invoke(self: *Instance, name: []const u8, in: []u64, out: []u64, comptime options: VirtualMachineOptions) !void {
        // 1.
        const index = try self.module.getExport(.Func, name);
        if (index >= self.module.functions.list.items.len) return error.FuncIndexExceedsTypesLength;

        const function = try self.getFunc(index);

        var frame_stack: [options.frame_stack_size]VirtualMachine.Frame = [_]VirtualMachine.Frame{undefined} ** options.frame_stack_size;
        var label_stack: [options.label_stack_size]VirtualMachine.Label = [_]VirtualMachine.Label{undefined} ** options.label_stack_size;
        var op_stack: [options.operand_stack_size]u64 = [_]u64{0} ** options.operand_stack_size;

        switch (function) {
            .function => |f| {
                const function_instance = try self.store.instance(f.instance);

                if (f.params.len != in.len) return error.ParamCountMismatch;
                if (f.results.len != out.len) return error.ResultCountMismatch;

                // 6. set up our stacks
                var vm = VirtualMachine.init(op_stack[0..], frame_stack[0..], label_stack[0..], try self.store.instance(f.instance));

                const locals_start = vm.op_ptr;

                // 7b. push params
                for (in) |arg| {
                    try vm.pushOperand(u64, arg);
                }

                // 7c. push (i.e. make space for) locals
                var i: usize = 0;
                while (i < f.locals_count) : (i += 1) {
                    try vm.pushOperand(u64, 0);
                }

                // Check we have enough stack space
                try vm.checkStackSpace(f.required_stack_space);

                // 7a. push control frame
                try vm.pushFrame(VirtualMachine.Frame{
                    .op_stack_len = locals_start,
                    .label_stack_len = vm.label_ptr,
                    .return_arity = f.results.len,
                    .inst = function_instance,
                }, f.locals_count + f.params.len);

                // 7a.2. push label for our implicit function block. We know we don't have
                // any code to execute after calling invoke, but we will need to
                // pop a Label
                try vm.pushLabel(VirtualMachine.Label{
                    .return_arity = f.results.len,
                    .op_stack_len = locals_start,
                    .branch_target = 0,
                });

                // 8. Execute our function
                try vm.invoke(f.start);

                // 9.
                for (out) |_, out_index| {
                    out[out_index] = vm.popOperand(u64);
                }
            },
            .host_function => |host_func| {
                var vm = VirtualMachine.init(op_stack[0..], frame_stack[0..], label_stack[0..], self);
                try host_func.func(&vm);
            },
        }
    }

    pub fn invokeStart(self: *Instance, index: u32, comptime options: VirtualMachineOptions) !void {
        const function = try self.getFunc(index);

        var frame_stack: [options.frame_stack_size]VirtualMachine.Frame = [_]VirtualMachine.Frame{undefined} ** options.frame_stack_size;
        var label_stack: [options.label_stack_size]VirtualMachine.Label = [_]VirtualMachine.Label{undefined} ** options.label_stack_size;
        var op_stack: [options.operand_stack_size]u64 = [_]u64{0} ** options.operand_stack_size;

        switch (function) {
            .function => |f| {
                const function_instance = try self.store.instance(f.instance);
                var vm = VirtualMachine.init(op_stack[0..], frame_stack[0..], label_stack[0..], try self.store.instance(f.instance));

                const locals_start = vm.op_ptr;

                var i: usize = 0;
                while (i < f.locals_count) : (i += 1) {
                    try vm.pushOperand(u64, 0);
                }

                // Check we have enough stack space
                try vm.checkStackSpace(f.required_stack_space);

                try vm.pushFrame(VirtualMachine.Frame{
                    .op_stack_len = locals_start,
                    .label_stack_len = vm.label_ptr,
                    .return_arity = 0,
                    .inst = function_instance,
                }, f.locals_count);

                try vm.pushLabel(VirtualMachine.Label{
                    .return_arity = 0,
                    .op_stack_len = locals_start,
                    .branch_target = 0,
                });

                try vm.invoke(f.start);
            },
            .host_function => |host_func| {
                var vm = VirtualMachine.init(op_stack[0..], frame_stack[0..], label_stack[0..], self);
                try host_func.func(&vm);
            },
        }
    }

    pub fn invokeExpression(self: *Instance, start: usize, comptime Result: type, comptime options: VirtualMachineOptions) !Result {
        var frame_stack: [options.frame_stack_size]VirtualMachine.Frame = [_]VirtualMachine.Frame{undefined} ** options.frame_stack_size;
        var label_stack: [options.label_stack_size]VirtualMachine.Label = [_]VirtualMachine.Label{undefined} ** options.label_stack_size;
        var op_stack: [options.operand_stack_size]u64 = [_]u64{0} ** options.operand_stack_size;

        var vm = VirtualMachine.init(op_stack[0..], frame_stack[0..], label_stack[0..], self);

        const locals_start = vm.op_ptr;

        try vm.pushFrame(VirtualMachine.Frame{
            .op_stack_len = locals_start,
            .label_stack_len = vm.label_ptr,
            .return_arity = 1,
            .inst = self,
        }, 0);

        try vm.pushLabel(VirtualMachine.Label{
            .return_arity = 1,
            .op_stack_len = locals_start,
        });

        try vm.invoke(start);

        switch (Result) {
            u64 => return vm.popAnyOperand(),
            else => return vm.popOperand(Result),
        }
    }
};
