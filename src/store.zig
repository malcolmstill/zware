const std = @import("std");
const mem = std.mem;
const math = std.math;
const ArrayList = std.ArrayList;
const Memory = @import("memory.zig").Memory;
const Table = @import("table.zig").Table;

// - Stores provide the runtime memory shared between modules
// - For different applications you may want to use a store that
//   that allocates individual items differently. For example,
//   if you know ahead of time that you will only load a fixed
//   number of modules during the lifetime of the program, then
//   you might be quite happy with a store that uses ArrayLists
//   with an arena allocator, i.e. you are going to deallocate
//   everything at the same time.

pub const ArrayListStore = struct {
    alloc: *mem.Allocator,
    memories: ArrayList(Memory),
    tables: ArrayList(Table),
    globals: ArrayList(u64),

    pub fn init(alloc: *mem.Allocator) ArrayListStore {
        var store = ArrayListStore{
            .alloc = alloc,
            .memories = ArrayList(Memory).init(alloc),
            .tables = ArrayList(Table).init(alloc),
            .globals = ArrayList(u64).init(alloc),
        };

        return store;
    }

    pub fn addMemory(self: *ArrayListStore) !*Memory {
        const mem_ptr = try self.memories.addOne();
        mem_ptr.* = Memory.init(self.alloc);
        return mem_ptr;
    }

    pub fn addTable(self: *ArrayListStore, entries: u32, max: ?u32) !*Table {
        const tbl_ptr = try self.tables.addOne();
        tbl_ptr.* = try Table.init(self.alloc, entries, max);
        return tbl_ptr;
    }

    pub fn addGlobals(self: *ArrayListStore, count: usize) !void {
        _ = try self.globals.resize(count);
        mem.set(u64, self.globals.items[0..], 0);
    }
};
