const std = @import("std");
const mem = std.mem;
const math = std.math;
const ArrayList = std.ArrayList;
const Memory = @import("memory.zig").Memory;

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

    pub fn addMemories(self: *Store, count: usize) !void {
        try self.memories.appendNTimes(Memory.init(self.alloc), count);
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

    pub fn allocGlobals(self: *Store, count: usize) !void {
        _ = try self.globals.resize(count);
        mem.set(u64, self.globals.items[0..], 0);
    }
};

pub const Table = struct {
    data: []?u32,
    max: ?u32,

    pub fn init(alloc: *mem.Allocator, min: u32, max: ?u32) !Table {
        return Table{
            .data = try alloc.alloc(?u32, min),
            .max = max,
        };
    }

    pub fn lookup(self: *Table, index: u32) !u32 {
        if (index >= self.data.len) return error.OutOfBoundsMemoryAccess;
        return self.data[index] orelse return error.UndefinedElement;
    }

    pub fn set(self: *Table, index: u32, value: u32) !void {
        if (index >= self.data.len) return error.OutOfBoundsMemoryAccess;
        self.data[index] = value;
    }
};
