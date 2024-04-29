const std = @import("std");
const mem = std.mem;
const math = std.math;
const ArrayList = std.ArrayList;
const Function = @import("store/function.zig").Function;
const Memory = @import("store/memory.zig").Memory;
const Table = @import("store/table.zig").Table;
const Global = @import("store/global.zig").Global;
const Elem = @import("store/elem.zig").Elem;
const Data = @import("store/data.zig").Data;
const Import = @import("module.zig").Import;
const Tag = @import("module.zig").Tag;
const Mutability = @import("module.zig").Mutability;
const RefType = @import("valtype.zig").RefType;
const ValType = @import("valtype.zig").ValType;
const Instance = @import("instance.zig").Instance;

// - Stores provide the runtime memory shared between modules
// - For different applications you may want to use a store that
//   that allocates individual items differently. For example,
//   if you know ahead of time that you will only load a fixed
//   number of modules during the lifetime of the program, then
//   you might be quite happy with a store that uses ArrayLists
//   with an arena allocator, i.e. you are going to deallocate
//   everything at the same time.

pub const ImportExport = struct {
    import: Import,
    handle: usize,
};

pub const ArrayListStore = struct {
    alloc: mem.Allocator,
    functions: ArrayList(Function),
    memories: ArrayList(Memory),
    tables: ArrayList(Table),
    globals: ArrayList(Global),
    elems: ArrayList(Elem),
    datas: ArrayList(Data),
    imports: ArrayList(ImportExport),

    pub fn init(alloc: mem.Allocator) ArrayListStore {
        const store = ArrayListStore{
            .alloc = alloc,
            .functions = ArrayList(Function).init(alloc),
            .memories = ArrayList(Memory).init(alloc),
            .tables = ArrayList(Table).init(alloc),
            .globals = ArrayList(Global).init(alloc),
            .elems = ArrayList(Elem).init(alloc),
            .datas = ArrayList(Data).init(alloc),
            .imports = ArrayList(ImportExport).init(alloc),
        };

        return store;
    }

    pub fn deinit(self: *ArrayListStore) void {
        for (self.memories.items) |*m| {
            m.deinit();
        }
        for (self.tables.items) |*t| {
            t.deinit();
        }
        for (self.elems.items) |*e| {
            e.deinit();
        }
        for (self.datas.items) |*d| {
            d.deinit();
        }

        self.functions.deinit();
        self.memories.deinit();
        self.tables.deinit();
        self.globals.deinit();
        self.elems.deinit();
        self.datas.deinit();
        self.imports.deinit();
    }

    // import
    //
    // import attempts to find in the store, the given module.name pair
    // for the given type
    pub fn import(self: *ArrayListStore, module: []const u8, name: []const u8, tag: Tag) !usize {
        for (self.imports.items) |importexport| {
            if (tag != importexport.import.desc_tag) continue;
            if (!mem.eql(u8, module, importexport.import.module)) continue;
            if (!mem.eql(u8, name, importexport.import.name)) continue;

            return importexport.handle;
        }

        return error.ImportNotFound;
    }

    pub fn @"export"(self: *ArrayListStore, module: []const u8, name: []const u8, tag: Tag, handle: usize) !void {
        try self.imports.append(ImportExport{
            .import = Import{
                .module = module,
                .name = name,
                .desc_tag = tag,
            },
            .handle = handle,
        });
    }

    /// Given a funcaddr return a Function if the funcaddr exists
    /// otherwise error with BadFunctionIndex.
    pub fn function(self: *ArrayListStore, funcaddr: usize) !Function {
        if (funcaddr >= self.functions.items.len) return error.BadFunctionIndex;
        return self.functions.items[funcaddr];
    }

    pub fn addFunction(self: *ArrayListStore, func: Function) !usize {
        const fun_ptr = try self.functions.addOne();
        fun_ptr.* = func;
        return self.functions.items.len - 1;
    }

    /// Given a memaddr return a pointer to the Memory if the memaddr exists
    /// otherwise error with BadMemoryIndex.
    pub fn memory(self: *ArrayListStore, memaddr: usize) !*Memory {
        if (memaddr >= self.memories.items.len) return error.BadMemoryIndex;
        return &self.memories.items[memaddr];
    }

    /// Allocate a new Memory with min / max size and add to store.
    pub fn addMemory(self: *ArrayListStore, min: u32, max: ?u32) !usize {
        const mem_ptr = try self.memories.addOne();
        mem_ptr.* = Memory.init(self.alloc, min, max);
        _ = try mem_ptr.grow(min);
        return self.memories.items.len - 1;
    }

    /// Given a tableaddr return a pointer to the Table if the tableaddr exists
    /// otherwise error with BadTableIndex.
    pub fn table(self: *ArrayListStore, tableaddr: usize) !*Table {
        if (tableaddr >= self.tables.items.len) return error.BadTableIndex;
        return &self.tables.items[tableaddr];
    }

    pub fn addTable(self: *ArrayListStore, reftype: RefType, entries: u32, max: ?u32) !usize {
        const tbl_ptr = try self.tables.addOne();
        tbl_ptr.* = try Table.init(self.alloc, reftype, entries, max);
        return self.tables.items.len - 1;
    }

    /// Given a globaladdr return a pointer to the Global if the globaladdr exists
    /// otherwise error with BadGlobalIndex.
    pub fn global(self: *ArrayListStore, globaladdr: usize) !*Global {
        if (globaladdr >= self.globals.items.len) return error.BadGlobalIndex;
        return &self.globals.items[globaladdr];
    }

    /// Add a Global to the store and return its globaladdr
    pub fn addGlobal(self: *ArrayListStore, value: Global) !usize {
        const glbl_ptr = try self.globals.addOne();
        glbl_ptr.* = value;

        return self.globals.items.len - 1;
    }

    /// Given a elemaddr return a pointer to the Elem if the elemaddr exists
    /// otherwise error with BadElemAddr.
    pub fn elem(self: *ArrayListStore, elemaddr: usize) !*Elem {
        if (elemaddr >= self.elems.items.len) return error.BadElemAddr;
        return &self.elems.items[elemaddr];
    }

    pub fn addElem(self: *ArrayListStore, reftype: RefType, count: u32) !usize {
        const elem_ptr = try self.elems.addOne();
        elem_ptr.* = try Elem.init(self.alloc, reftype, count);
        return self.elems.items.len - 1;
    }

    /// Given a dataaddr return a pointer to the Data if the dataaddr exists
    /// otherwise error with BadDataAddr.
    pub fn data(self: *ArrayListStore, dataaddr: usize) !*Data {
        if (dataaddr >= self.datas.items.len) return error.BadDataAddr;
        return &self.datas.items[dataaddr];
    }

    pub fn addData(self: *ArrayListStore, count: u32) !usize {
        const data_ptr = try self.datas.addOne();
        data_ptr.* = try Data.init(self.alloc, count);
        return self.datas.items.len - 1;
    }

    // Helper functions for exposing values

    pub fn exposeHostFunction(self: *ArrayListStore, module: []const u8, function_name: []const u8, host_function_pointer: anytype, params: []const ValType, results: []const ValType) !void {
        const funcaddr = try self.addFunction(Function{
            .params = params,
            .results = results,
            .subtype = .{
                .host_function = .{
                    .func = host_function_pointer,
                },
            },
        });

        try self.@"export"(module[0..], function_name[0..], .Func, funcaddr);
    }

    pub fn exposeMemory(self: *ArrayListStore, module: []const u8, name: []const u8, min: u32, max: ?u32) !void {
        const memaddr = try self.addMemory(min, max);

        try self.@"export"(module, name, .Mem, memaddr);
    }

    pub fn exposeTable(self: *ArrayListStore, module: []const u8, name: []const u8, reftype: RefType, entries: u32, max: ?u32) !void {
        const tableaddr = try self.addTable(reftype, entries, max);

        try self.@"export"(module, name, .Table, tableaddr);
    }

    pub fn exposeGlobal(self: *ArrayListStore, module: []const u8, name: []const u8, value: u64, valtype: ValType, mutability: Mutability) !void {
        const globaladdr = try self.addGlobal(.{
            .value = value,
            .valtype = valtype,
            .mutability = mutability,
        });

        try self.@"export"(module, name, .Global, globaladdr);
    }

    pub fn exposeElem(self: *ArrayListStore, module: []const u8, name: []const u8, reftype: RefType, count: u32) !void {
        const elemaddr = try self.addElem(reftype, count);

        try self.@"export"(module, name, .Elem, elemaddr);
    }

    pub fn exposeData(self: *ArrayListStore, module: []const u8, name: []const u8, count: u32) !void {
        const dataaddr = try self.addData(count);

        try self.@"export"(module, name, .Data, dataaddr);
    }
};
