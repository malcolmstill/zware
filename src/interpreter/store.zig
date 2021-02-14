const std = @import("std");
const mem = std.mem;
const ArrayList = std.ArrayList;

pub const PAGE_SIZE = 64 * 1024;

pub const Store = struct {
    alloc: *mem.Allocator,
    memories: ArrayList(Memory),
    globals: ArrayList(u64),

    pub fn init(alloc: *mem.Allocator) Store {
        var store = Store{
            .alloc = alloc,
            .memories = ArrayList(Memory).init(alloc),
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

    pub fn allocGlobals(self: *Store, count: usize) !void {
        _ = try self.globals.resize(count);
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
            if (self.data.items.len + num_pages >= max_size) return error.MemoryGrowExceedsMaxSize;
        }
        const old_size = self.data.items.len;
        _ = try self.data.resize(self.data.items.len + num_pages);
        return old_size;
    }

    pub fn read(self: *Memory, comptime T: type, address: u32) !T {
        if (address + @sizeOf(T) - 1 >= PAGE_SIZE * self.data.items.len) return error.MemoryIndexOutOfBounds;

        const page: u32 = address / PAGE_SIZE;
        const offset: u32 = address % PAGE_SIZE;

        return mem.readInt(T, @ptrCast(*const [@sizeOf(T)]u8, &self.data.items[page][offset]), .Little);
    }

    pub fn write(self: *Memory, comptime T: type, address: u32, value: T) !void {
        if (address + @sizeOf(T) - 1 >= PAGE_SIZE * self.data.items.len) return error.MemoryIndexOutOfBounds;

        const page: u32 = address / PAGE_SIZE;
        const offset: u32 = address % PAGE_SIZE;

        std.mem.writeInt(T, @ptrCast(*[@sizeOf(T)]u8, &self.data.items[page][offset]), value, .Little);
    }
};

const testing = std.testing;
test "Memory test" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer _ = arena.deinit();

    var store = Store.init(&arena.allocator);
    var mem0 = try store.addMemory();

    _ = try mem0.grow(1);
    testing.expectEqual(@as(usize, 1), mem0.size());

    testing.expectEqual(@as(u8, 0xAA), try mem0.read(u8, 0));
    testing.expectEqual(@as(u32, 0xAAAAAAAA), try mem0.read(u32, 0));
    try mem0.write(u8, 0, 15);
    testing.expectEqual(@as(u8, 15), try mem0.read(u8, 0));
    testing.expectEqual(@as(u32, 0xAAAAAA0F), try mem0.read(u32, 0));

    try mem0.write(u8, 0xFFFF, 42);
    testing.expectEqual(@as(u8, 42), try mem0.read(u8, 0xFFFF));
    testing.expectError(error.MemoryIndexOutOfBounds, mem0.read(u16, 0xFFFF));
    testing.expectError(error.MemoryIndexOutOfBounds, mem0.read(u8, 0xFFFF + 1));

    _ = try mem0.grow(1);
    testing.expectEqual(@as(usize, 2), mem0.size());

    testing.expectEqual(@as(u8, 0xAA), try mem0.read(u8, 0xFFFF + 1));
    testing.expectEqual(@as(u8, 0xAA), try mem0.read(u8, 0x1FFFF));
    testing.expectError(error.MemoryIndexOutOfBounds, mem0.read(u8, 0x1FFFF + 1));

    mem0.max_size = 2;
    testing.expectError(error.MemoryGrowExceedsMaxSize, mem0.grow(1));

    mem0.max_size = null;
    _ = try mem0.grow(1);
    testing.expectEqual(@as(usize, 3), mem0.size());
}
