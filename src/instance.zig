const std = @import("std");
const mem = std.mem;
const math = std.math;
const os = std.os;
const wasi = std.os.wasi;
const ArrayList = std.ArrayList;
const Module = @import("module.zig").Module;
const Store = @import("store.zig").ArrayListStore;
const Function = @import("store/function.zig").Function;
const Memory = @import("store/memory.zig").Memory;
const Table = @import("store/table.zig").Table;
const Global = @import("store/global.zig").Global;
const Elem = @import("store/elem.zig").Elem;
const Data = @import("store/data.zig").Data;
const VirtualMachine = @import("instance/vm.zig").VirtualMachine;

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
    // TODO: exports (this was in 1.0...why didn't we need it)
    elemaddrs: ArrayList(usize),
    dataaddrs: ArrayList(usize),

    // wasi-specific fields
    //
    // They are defined on an instance but only really on an
    // initial instance that is invoked. When initialising a
    // VirtualMachine this initial instance will pass its wasi
    // data to the VirtualMachine (via pointers).
    //
    // The wasi implementations can (and must) then lookup this data via
    // the VirtualMachine, it shouldn't call e.g. `vm.inst...` because
    // a VirtualMachine swaps out its `inst` (instance) pointer as
    // it executes; an arbitrary `inst` will not contain the correct
    // data.
    wasi_preopens: std.AutoHashMap(wasi.fd_t, WasiPreopen),
    wasi_args: std.ArrayList([:0]u8),
    wasi_env: std.StringHashMap([]const u8),

    pub fn init(alloc: mem.Allocator, store: *Store, module: Module) Instance {
        return Instance{
            .module = module,
            .store = store,
            .funcaddrs = ArrayList(usize).init(alloc),
            .memaddrs = ArrayList(usize).init(alloc),
            .tableaddrs = ArrayList(usize).init(alloc),
            .globaladdrs = ArrayList(usize).init(alloc),
            .elemaddrs = ArrayList(usize).init(alloc),
            .dataaddrs = ArrayList(usize).init(alloc),

            .wasi_preopens = std.AutoHashMap(os.wasi.fd_t, WasiPreopen).init(alloc),
            .wasi_args = ArrayList([:0]u8).init(alloc),
            .wasi_env = std.StringHashMap([]const u8).init(alloc),
        };
    }

    pub fn deinit(self: *Instance) void {
        self.funcaddrs.deinit();
        self.memaddrs.deinit();
        self.tableaddrs.deinit();
        self.globaladdrs.deinit();
        self.elemaddrs.deinit();
        self.dataaddrs.deinit();

        self.wasi_preopens.deinit();
        self.wasi_args.deinit();
        self.wasi_env.deinit();
    }

    pub fn getFunc(self: *Instance, funcidx: usize) !Function {
        if (funcidx >= self.funcaddrs.items.len) return error.FunctionIndexOutOfBounds;
        const funcaddr = self.funcaddrs.items[funcidx];
        return try self.store.function(funcaddr);
    }

    // Lookup a memory in store via the modules index
    pub fn getMemory(self: *Instance, index: usize) !*Memory {
        // TODO: with a verified program we shouldn't need to check this
        if (index >= self.memaddrs.items.len) return error.MemoryIndexOutOfBounds;
        const memaddr = self.memaddrs.items[index];
        return try self.store.memory(memaddr);
    }

    pub fn getTable(self: *Instance, index: usize) !*Table {
        // TODO: with a verified program we shouldn't need to check this
        if (index >= self.tableaddrs.items.len) return error.TableIndexOutOfBounds;
        const tableaddr = self.tableaddrs.items[index];
        return try self.store.table(tableaddr);
    }

    pub fn getGlobal(self: *Instance, index: usize) !*Global {
        if (index >= self.globaladdrs.items.len) return error.GlobalIndexOutOfBounds;
        const globaladdr = self.globaladdrs.items[index];
        return try self.store.global(globaladdr);
    }

    pub fn getElem(self: *Instance, elemidx: usize) !*Elem {
        if (elemidx >= self.elemaddrs.items.len) return error.ElemIndexOutOfBounds;
        const elemaddr = self.elemaddrs.items[elemidx];
        return try self.store.elem(elemaddr);
    }

    pub fn getData(self: *Instance, dataidx: usize) !*Data {
        if (dataidx >= self.dataaddrs.items.len) return error.DataIndexOutOfBounds;
        const dataaddr = self.dataaddrs.items[dataidx];
        return try self.store.data(dataaddr);
    }

    pub fn instantiate(self: *Instance) !void {
        if (self.module.decoded == false) return error.ModuleNotDecoded;

        try self.instantiateImports();
        try self.instantiateFunctions();
        try self.instantiateGlobals();
        try self.instantiateMemories();
        try self.instantiateTables();
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

    fn instantiateFunctions(self: *Instance) !void {
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
        for (self.module.functions.list.items, 0..) |function_def, i| {
            if (function_def.import) |_| {
                // Check that the function defintion (which this module expects)
                // is the same as the function in the store
                const functype = try self.module.types.lookup(function_def.typeidx);
                const external_function = try self.getFunc(i);

                try external_function.checkSignatures(functype);
            } else {
                const code = try self.module.codes.lookup(i - imported_function_count);
                const func = try self.module.functions.lookup(i);
                const functype = try self.module.types.lookup(func.typeidx);

                const handle = try self.store.addFunction(Function{
                    .params = functype.params,
                    .results = functype.results,
                    .subtype = .{
                        .function = .{
                            .start = code.start,
                            .required_stack_space = code.required_stack_space,
                            .locals_count = code.locals_count,
                            .instance = self,
                        },
                    },
                });

                // Need to do this regardless of if import or internal
                try self.funcaddrs.append(handle);
            }
        }
    }

    fn instantiateGlobals(self: *Instance) !void {
        for (self.module.globals.list.items, 0..) |global_def, i| {
            if (global_def.import != null) {
                const imported_global = try self.getGlobal(i);
                if (imported_global.mutability != global_def.mutability) return error.MismatchedMutability;
                if (imported_global.valtype != global_def.valtype) return error.MismatchedGlobalType;
            } else {
                const value = if (global_def.start) |start| try self.invokeExpression(start, u64, .{}) else 0;
                const handle = try self.store.addGlobal(Global{
                    .value = value,
                    .mutability = global_def.mutability,
                    .valtype = global_def.valtype,
                });
                try self.globaladdrs.append(handle);
            }
        }
    }

    fn instantiateMemories(self: *Instance) !void {
        for (self.module.memories.list.items, 0..) |memtype, i| {
            if (memtype.import != null) {
                const imported_mem = try self.getMemory(i);
                // Use the current size of the imported mem as min (rather than imported_mem.min). See https://github.com/WebAssembly/spec/pull/1293
                try memtype.limits.checkMatch(imported_mem.size(), imported_mem.max);
            } else {
                const handle = try self.store.addMemory(memtype.limits.min, memtype.limits.max);
                try self.memaddrs.append(handle);
            }
        }
    }

    fn instantiateTables(self: *Instance) !void {
        for (self.module.tables.list.items, 0..) |tabletype, i| {
            if (tabletype.import != null) {
                const imported_table = try self.getTable(i);
                if (imported_table.reftype != tabletype.reftype) return error.ImportedTableRefTypeMismatch;
                try tabletype.limits.checkMatch(imported_table.min, imported_table.max);
            } else {
                const handle = try self.store.addTable(tabletype.reftype, tabletype.limits.min, tabletype.limits.max);
                try self.tableaddrs.append(handle);
            }
        }
    }

    fn instantiateData(self: *Instance) !void {
        for (self.module.datas.list.items) |datatype| {
            const dataddr = try self.store.addData(datatype.count);
            try self.dataaddrs.append(dataddr);
            var data = try self.store.data(dataddr);

            // TODO: Do we actually need to copy the data or just close over module bytes?
            for (datatype.data, 0..) |byte, j| {
                try data.set(j, byte);
            }

            switch (datatype.mode) {
                .Passive => continue,
                .Active => |active| {
                    const memaddr = self.memaddrs.items[active.memidx];
                    const memory = try self.store.memory(memaddr);

                    const offset = try self.invokeExpression(active.offset, u32, .{});
                    try memory.copy(offset, data.data);
                    data.dropped = true;
                },
            }
        }
    }

    fn instantiateElements(self: *Instance) !void {
        for (self.module.elements.list.items) |elemtype| {
            const elemaddr = try self.store.addElem(elemtype.reftype, elemtype.count);
            try self.elemaddrs.append(elemaddr);
            var elem = try self.store.elem(elemaddr);

            for (self.module.element_init_offsets.items[elemtype.init .. elemtype.init + elemtype.count], 0..) |expr, j| {
                const funcaddr = try self.invokeExpression(expr, u32, .{});
                try elem.set(@intCast(j), funcaddr);
            }

            if (elemtype.mode != .Passive) {
                elem.dropped = true;
            }

            if (elemtype.mode == .Active) {
                const table = try self.getTable(elemtype.mode.Active.tableidx);
                const offset = try self.invokeExpression(elemtype.mode.Active.offset, u32, .{});

                const index = math.add(u32, offset, elemtype.count) catch return error.OutOfBoundsMemoryAccess;
                if (index > table.size()) return error.OutOfBoundsMemoryAccess;

                for (elem.elem, 0..) |funcaddr, i| {
                    try table.set(@intCast(offset + i), funcaddr);
                }
            }
        }
    }

    // invoke
    //
    // Similar to invoke, but without some type checking
    pub fn invoke(self: *Instance, name: []const u8, in: []u64, out: []u64, comptime options: VirtualMachineOptions) !void {
        const funcidx = try self.module.getExport(.Func, name);
        if (funcidx >= self.module.functions.list.items.len) return error.FuncIndexExceedsTypesLength;

        const function = try self.getFunc(funcidx);

        var frame_stack: [options.frame_stack_size]VirtualMachine.Frame = [_]VirtualMachine.Frame{undefined} ** options.frame_stack_size;
        var label_stack: [options.label_stack_size]VirtualMachine.Label = [_]VirtualMachine.Label{undefined} ** options.label_stack_size;
        var op_stack: [options.operand_stack_size]u64 = [_]u64{0} ** options.operand_stack_size;

        switch (function.subtype) {
            .function => |f| {
                if (function.params.len != in.len) return error.ParamCountMismatch;
                if (function.results.len != out.len) return error.ResultCountMismatch;

                // 6. set up our stacks
                var vm = VirtualMachine.init(op_stack[0..], frame_stack[0..], label_stack[0..], f.instance);

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
                    .return_arity = function.results.len,
                    .inst = f.instance,
                }, f.locals_count + function.params.len);

                // 7a.2. push label for our implicit function block. We know we don't have
                // any code to execute after calling invoke, but we will need to
                // pop a Label
                try vm.pushLabel(VirtualMachine.Label{
                    .return_arity = function.results.len,
                    .op_stack_len = locals_start,
                    .branch_target = 0,
                });

                // 8. Execute our function
                try vm.invoke(f.start);

                // 9.
                for (out, 0..) |_, out_index| {
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

        switch (function.subtype) {
            .function => |f| {
                var vm = VirtualMachine.init(op_stack[0..], frame_stack[0..], label_stack[0..], f.instance);

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
                    .inst = f.instance,
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

    pub fn addWasiPreopen(self: *Instance, wasi_fd: os.wasi.fd_t, name: []const u8, host_fd: os.fd_t) !void {
        return self.wasi_preopens.put(wasi_fd, .{
            .wasi_fd = wasi_fd,
            .name = name,
            .host_fd = host_fd,
        });
    }

    // FIXME: hide any allocation / deinit inside Instance
    // Caller must call std.process.argsFree on returned args
    //
    // This forwards all the processes's command line args to the
    // virtual machine.
    //
    // TODO: we probably want to allow consumers of zware more fine-grained
    //       control of which arguments get exposed to an instance. A similar
    //       thing would be desirable for env vars.
    pub fn forwardArgs(self: *Instance, alloc: mem.Allocator) ![][:0]u8 {
        const args = try std.process.argsAlloc(alloc);

        for (args) |arg| {
            try self.wasi_args.append(arg);
        }

        return args;
    }
};

pub const WasiPreopen = struct {
    wasi_fd: wasi.fd_t,
    name: []const u8,
    host_fd: os.fd_t,
};
