const std = @import("std");
const mem = std.mem;
const math = std.math;
const ArrayList = std.ArrayList;

pub const MAX_PAGES = 64 * 1024;
pub const PAGE_SIZE = 64 * 1024;

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

    pub fn addTable(self: *Store, entries: usize) !*Table {
        const tbl_ptr = try self.tables.addOne();
        tbl_ptr.* = try Table.init(self.alloc, entries);
        return tbl_ptr;
    }

    pub fn allocGlobals(self: *Store, count: usize) !void {
        _ = try self.globals.resize(count);
        mem.set(u64, self.globals.items[0..], 0);
    }
};

pub const Memory = struct {
    alloc: *mem.Allocator,
    max_size: ?u32 = null,
    data: ArrayList([PAGE_SIZE]u8),

    pub fn init(alloc: *mem.Allocator) Memory {
        return Memory{
            .alloc = alloc,
            .data = ArrayList([PAGE_SIZE]u8).init(alloc),
        };
    }

    // TODO: is usize correct here?
    pub fn size(self: *Memory) usize {
        return self.data.items.len;
    }

    pub fn grow(self: *Memory, num_pages: u32) !usize {
        if (self.max_size) |max_size| {
            if (self.data.items.len + num_pages > math.min(max_size, MAX_PAGES)) return error.OutOfBoundsMemoryAccess;
        }
        const old_size = self.data.items.len;
        _ = try self.data.resize(self.data.items.len + num_pages);
        mem.set(u8, self.asSlice()[PAGE_SIZE * old_size .. PAGE_SIZE * (old_size + num_pages)], 0);
        return old_size;
    }

    pub fn read(self: *Memory, comptime T: type, address: u32) !T {
        if (address + @sizeOf(T) - 1 >= PAGE_SIZE * self.data.items.len) return error.OutOfBoundsMemoryAccess;

        const page: u32 = address / PAGE_SIZE;
        const offset: u32 = address % PAGE_SIZE;

        switch (T) {
            u8,
            u16,
            u32,
            u64,
            i8,
            i16,
            i32,
            i64,
            => return mem.readInt(T, @ptrCast(*const [@sizeOf(T)]u8, &self.data.items[page][offset]), .Little),
            f32 => {
                const x = mem.readInt(u32, @ptrCast(*const [@sizeOf(T)]u8, &self.data.items[page][offset]), .Little);
                return @bitCast(f32, x);
            },
            f64 => {
                const x = mem.readInt(u64, @ptrCast(*const [@sizeOf(T)]u8, &self.data.items[page][offset]), .Little);
                return @bitCast(f64, x);
            },
            else => @compileError("Memory.read unsupported type (not int/float): " ++ @typeName(T)),
        }
    }

    pub fn write(self: *Memory, comptime T: type, address: u32, value: T) !void {
        if (address + @sizeOf(T) - 1 >= PAGE_SIZE * self.data.items.len) return error.OutOfBoundsMemoryAccess;

        const page: u32 = address / PAGE_SIZE;
        const offset: u32 = address % PAGE_SIZE;

        switch (T) {
            u8,
            u16,
            u32,
            u64,
            i8,
            i16,
            i32,
            i64,
            => std.mem.writeInt(T, @ptrCast(*[@sizeOf(T)]u8, &self.data.items[page][offset]), value, .Little),
            f32 => {
                const x = @bitCast(u32, value);
                std.mem.writeInt(u32, @ptrCast(*[@sizeOf(u32)]u8, &self.data.items[page][offset]), x, .Little);
            },
            f64 => {
                const x = @bitCast(u64, value);
                std.mem.writeInt(u64, @ptrCast(*[@sizeOf(u64)]u8, &self.data.items[page][offset]), x, .Little);
            },
            else => @compileError("Memory.read unsupported type (not int/float): " ++ @typeName(T)),
        }
    }

    pub fn asSlice(self: *Memory) []u8 {
        var slice: []u8 = undefined;
        slice.ptr = if (self.data.items.len > 0) @ptrCast([*]u8, &self.data.items[0][0]) else undefined;
        slice.len = PAGE_SIZE * self.data.items.len;
        return slice;
    }
};

const testing = std.testing;
test "Memory test" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer _ = arena.deinit();

    var store = Store.init(&arena.allocator);
    var mem0 = try store.addMemory();
    testing.expectEqual(@as(usize, 0), mem0.asSlice().len);

    _ = try mem0.grow(1);
    testing.expectEqual(@as(usize, 1 * PAGE_SIZE), mem0.asSlice().len);
    testing.expectEqual(@as(usize, 1), mem0.size());

    testing.expectEqual(@as(u8, 0x00), try mem0.read(u8, 0));
    testing.expectEqual(@as(u32, 0x00000000), try mem0.read(u32, 0));
    try mem0.write(u8, 0, 15);
    testing.expectEqual(@as(u8, 15), try mem0.read(u8, 0));
    testing.expectEqual(@as(u32, 0x0000000F), try mem0.read(u32, 0));

    try mem0.write(u8, 0xFFFF, 42);
    testing.expectEqual(@as(u8, 42), try mem0.read(u8, 0xFFFF));
    testing.expectError(error.MemoryIndexOutOfBounds, mem0.read(u16, 0xFFFF));
    testing.expectError(error.MemoryIndexOutOfBounds, mem0.read(u8, 0xFFFF + 1));

    _ = try mem0.grow(1);
    testing.expectEqual(@as(usize, 2 * PAGE_SIZE), mem0.asSlice().len);
    testing.expectEqual(@as(usize, 2), mem0.size());

    testing.expectEqual(@as(u8, 0x00), try mem0.read(u8, 0xFFFF + 1));
    // Write across page boundary
    try mem0.write(u16, 0xFFFF, 0xDEAD);
    testing.expectEqual(@as(u8, 0xAD), try mem0.read(u8, 0xFFFF));
    testing.expectEqual(@as(u8, 0xDE), try mem0.read(u8, 0xFFFF + 1));
    testing.expectEqual(@as(u16, 0xDEAD), try mem0.read(u16, 0xFFFF));
    const slice = mem0.asSlice();
    testing.expectEqual(@as(u8, 0xAD), slice[0xFFFF]);
    testing.expectEqual(@as(u8, 0xDE), slice[0xFFFF + 1]);

    testing.expectEqual(@as(u8, 0x00), try mem0.read(u8, 0x1FFFF));
    testing.expectError(error.MemoryIndexOutOfBounds, mem0.read(u8, 0x1FFFF + 1));

    mem0.max_size = 2;
    testing.expectError(error.MemoryGrowExceedsMaxSize, mem0.grow(1));

    mem0.max_size = null;
    _ = try mem0.grow(1);
    testing.expectEqual(@as(usize, 3), mem0.size());

    testing.expectEqual(@as(usize, 3 * PAGE_SIZE), mem0.asSlice().len);
}

pub const Table = struct {
    // Let's assume indices are u32
    data: []?u32,

    pub fn init(alloc: *mem.Allocator, max: usize) !Table {
        return Table{
            .data = try alloc.alloc(?u32, max),
        };
    }

    pub fn lookup(self: *Table, index: usize) !u32 {
        if (self.data.len < index + 1) return error.TableLookupOutOfBounds;
        return self.data[index];
    }
};
