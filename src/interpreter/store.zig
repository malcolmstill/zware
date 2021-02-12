const std = @import("std");
const mem = std.mem;
const ArrayList = std.ArrayList;

pub const PAGE_SIZE = 64 * 1024;

pub const Store = struct {
    alloc: *mem.Allocator,
    memories: ArrayList(Memory),

    pub fn init(alloc: *mem.Allocator) Store {
        var store = Store{
            .alloc = alloc,
            .memories = ArrayList(Memory).init(alloc),
        };

        return store;
    }

    pub fn addMemory(self: *Store) !*Memory {
        const mem_ptr = try self.memories.addOne();
        mem_ptr.* = Memory.init(self.alloc);
        return mem_ptr;
    }

    pub fn readMemory(self: *Store, mem_index: u32, address: u32) !u8 {
        if (mem_index >= self.memories.items.len) return error.NoSuchMemoryAddress;
        var memory = self.memories.items[mem_index];

        return try memory.read(address);
    }

    pub fn writeMemory(self: *Store, mem_index: u32, address: u32, value: u8) !void {
        if (mem_index >= self.memories.items.len) return error.NoSuchMemoryAddress;
        var memory = self.memories.items[mem_index];

        try memory.write(address, value);
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

    pub fn grow(self: *Memory, num_pages: u32) !void {
        _ = try self.data.resize(self.data.items.len + num_pages);
    }

    pub fn read(self: *Memory, address: u32) !u8 {
        if (address >= PAGE_SIZE * self.data.items.len) return error.MemoryIndexOutOfBounds;

        const page: u32 = address / PAGE_SIZE;
        const offset: u32 = address % PAGE_SIZE;

        return self.data.items[page][offset];
    }

    pub fn write(self: *Memory, address: u32, value: u8) !void {
        if (address >= PAGE_SIZE * self.data.items.len) return error.MemoryIndexOutOfBounds;

        const page: u32 = address / PAGE_SIZE;
        const offset: u32 = address % PAGE_SIZE;

        self.data.items[page][offset] = value;
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

    testing.expectEqual(@as(u8, 0xAA), try mem0.read(0));
    try mem0.write(0, 22);
    testing.expectEqual(@as(u8, 22), try mem0.read(0));

    try mem0.write(0xFFFF, 42);
    testing.expectEqual(@as(u8, 42), try mem0.read(0xFFFF));

    testing.expectError(error.MemoryIndexOutOfBounds, mem0.read(0xFFFF + 1));

    _ = try mem0.grow(1);
    testing.expectEqual(@as(u8, 0xAA), try mem0.read(0xFFFF + 1));

    testing.expectEqual(@as(u8, 42), try store.readMemory(0, 0xFFFF));
}
