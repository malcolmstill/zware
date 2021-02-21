const std = @import("std");
const mem = std.mem;
const math = std.math;
const ArrayList = std.ArrayList;
const Function = @import("function.zig").Function;
const Memory = @import("memory.zig").Memory;
const Table = @import("table.zig").Table;
const Import = @import("common.zig").Import;
const Tag = @import("common.zig").Tag;
const ValueType = @import("common.zig").ValueType;

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
    alloc: *mem.Allocator,
    functions: ArrayList(Function),
    memories: ArrayList(Memory),
    tables: ArrayList(Table),
    globals: ArrayList(u64),
    imports: ArrayList(ImportExport),

    pub fn init(alloc: *mem.Allocator) ArrayListStore {
        var store = ArrayListStore{
            .alloc = alloc,
            .functions = ArrayList(Function).init(alloc),
            .memories = ArrayList(Memory).init(alloc),
            .tables = ArrayList(Table).init(alloc),
            .globals = ArrayList(u64).init(alloc),
            .imports = ArrayList(ImportExport).init(alloc),
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

    pub fn function(self: *ArrayListStore, handle: usize) !*Function {
        if (handle >= self.functions.items.len) return error.BadFunctionIndex;
        return &self.functions.items[handle];
    }

    pub fn addFunction(self: *ArrayListStore, code: []const u8, locals: []const u8, locals_count: usize, params: []ValueType, results: []ValueType) !usize {
        const fun_ptr = try self.functions.addOne();
        fun_ptr.* = Function{
            .code = code,
            .locals = locals,
            .locals_count = locals_count,
            .params = params,
            .results = results,
        };
        return self.functions.items.len - 1;
    }

    pub fn memory(self: *ArrayListStore, handle: usize) !*Memory {
        if (handle >= self.memories.items.len) return error.BadMemoryIndex;
        return &self.memories.items[handle];
    }

    pub fn addMemory(self: *ArrayListStore) !usize {
        const mem_ptr = try self.memories.addOne();
        mem_ptr.* = Memory.init(self.alloc);
        return self.memories.items.len - 1;
    }

    pub fn table(self: *ArrayListStore, handle: usize) !*Table {
        if (handle >= self.tables.items.len) return error.BadTableIndex;
        return &self.tables.items[handle];
    }

    pub fn addTable(self: *ArrayListStore, entries: u32, max: ?u32) !usize {
        const tbl_ptr = try self.tables.addOne();
        tbl_ptr.* = try Table.init(self.alloc, entries, max);
        return self.tables.items.len - 1;
    }

    pub fn global(self: *ArrayListStore, handle: usize) !*u64 {
        if (handle >= self.globals.items.len) return error.BadGlobalIndex;
        return &self.globals.items[handle];
    }

    pub fn addGlobal(self: *ArrayListStore, value: u64) !usize {
        const glbl_ptr = try self.globals.addOne();
        glbl_ptr.* = value;

        return self.globals.items.len - 1;
    }
};
