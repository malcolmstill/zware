const std = @import("std");
const mem = std.mem;
const math = std.math;
const ArrayList = std.ArrayList;
const Memory = @import("memory.zig").Memory;
const Table = @import("table.zig").Table;

pub const Store = struct {
    alloc: *mem.Allocator,
    memories: ArrayList(Memory),
    tables: ArrayList(Table),
    globals: ArrayList(u64),

    pub fn init(alloc: *mem.Allocator) Store {
        var store = Store{
            .alloc = alloc,
            .memories = ArrayList(Memory).init(alloc),
            .tables = ArrayList(Table).init(alloc),
            .globals = ArrayList(u64).init(alloc),
        };

        return store;
    }

    pub fn addMemory(self: *Store) !*Memory {
        const mem_ptr = try self.memories.addOne();
        mem_ptr.* = Memory.init(self.alloc);
        return mem_ptr;
    }

    pub fn addTable(self: *Store, entries: u32, max: ?u32) !*Table {
        const tbl_ptr = try self.tables.addOne();
        tbl_ptr.* = try Table.init(self.alloc, entries, max);
        return tbl_ptr;
    }

    pub fn addGlobals(self: *Store, count: usize) !void {
        _ = try self.globals.resize(count);
        mem.set(u64, self.globals.items[0..], 0);
    }
};
