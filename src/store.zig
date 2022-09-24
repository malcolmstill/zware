const std = @import("std");
const mem = std.mem;
const math = std.math;
const ArrayList = std.ArrayList;
const Function = @import("function.zig").Function;
const Memory = @import("memory.zig").Memory;
const Table = @import("table.zig").Table;
const Global = @import("global.zig").Global;
const Elem = @import("elem.zig").Elem;
const Data = @import("data.zig").Data;
const Import = @import("module.zig").Import;
const Tag = @import("module.zig").Tag;
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
    instances: ArrayList(Instance),

    pub fn init(alloc: mem.Allocator) ArrayListStore {
        var store = ArrayListStore{
            .alloc = alloc,
            .functions = ArrayList(Function).init(alloc),
            .memories = ArrayList(Memory).init(alloc),
            .tables = ArrayList(Table).init(alloc),
            .globals = ArrayList(Global).init(alloc),
            .elems = ArrayList(Elem).init(alloc),
            .datas = ArrayList(Data).init(alloc),
            .imports = ArrayList(ImportExport).init(alloc),
            .instances = ArrayList(Instance).init(alloc),
        };

        return store;
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
        std.log.warn("Import not found: {s}.{s}\n", .{ module, name });
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

    pub fn function(self: *ArrayListStore, handle: usize) !Function {
        if (handle >= self.functions.items.len) return error.BadFunctionIndex;
        return self.functions.items[handle];
    }

    pub fn addFunction(self: *ArrayListStore, func: Function) !usize {
        const fun_ptr = try self.functions.addOne();
        fun_ptr.* = func;
        return self.functions.items.len - 1;
    }

    pub fn memory(self: *ArrayListStore, handle: usize) !*Memory {
        if (handle >= self.memories.items.len) return error.BadMemoryIndex;
        return &self.memories.items[handle];
    }

    pub fn addMemory(self: *ArrayListStore, min: u32, max: ?u32) !usize {
        const mem_ptr = try self.memories.addOne();
        mem_ptr.* = Memory.init(self.alloc, min, max);
        _ = try mem_ptr.grow(min);
        return self.memories.items.len - 1;
    }

    pub fn table(self: *ArrayListStore, handle: usize) !*Table {
        if (handle >= self.tables.items.len) return error.BadTableIndex;
        return &self.tables.items[handle];
    }

    pub fn addTable(self: *ArrayListStore, reftype: RefType, entries: u32, max: ?u32) !usize {
        const tbl_ptr = try self.tables.addOne();
        tbl_ptr.* = try Table.init(self.alloc, reftype, entries, max);
        return self.tables.items.len - 1;
    }

    pub fn global(self: *ArrayListStore, handle: usize) !*Global {
        if (handle >= self.globals.items.len) return error.BadGlobalIndex;
        return &self.globals.items[handle];
    }

    pub fn addGlobal(self: *ArrayListStore, value: Global) !usize {
        const glbl_ptr = try self.globals.addOne();
        glbl_ptr.* = value;

        return self.globals.items.len - 1;
    }

    pub fn elem(self: *ArrayListStore, elemaddr: usize) !*Elem {
        if (elemaddr >= self.elems.items.len) return error.BadElemAddr;
        return &self.elems.items[elemaddr];
    }

    pub fn addElem(self: *ArrayListStore, reftype: RefType, count: u32) !usize {
        const elem_ptr = try self.elems.addOne();
        elem_ptr.* = try Elem.init(self.alloc, reftype, count);
        return self.elems.items.len - 1;
    }

    pub fn data(self: *ArrayListStore, dataaddr: usize) !*Data {
        if (dataaddr >= self.datas.items.len) return error.BadDataAddr;
        return &self.datas.items[dataaddr];
    }

    pub fn addData(self: *ArrayListStore, count: u32) !usize {
        const data_ptr = try self.datas.addOne();
        data_ptr.* = try Data.init(self.alloc, count);
        return self.datas.items.len - 1;
    }

    pub fn instance(self: *ArrayListStore, handle: usize) !*Instance {
        if (handle >= self.instances.items.len) return error.BadInstanceIndex;
        return &self.instances.items[handle];
    }

    pub fn addInstance(self: *ArrayListStore, inst: Instance) !usize {
        const instance_ptr = try self.instances.addOne();
        instance_ptr.* = inst;
        return self.instances.items.len - 1;
    }
};
