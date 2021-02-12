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

    testing.expectEqual(@as(u8, 0xAA), try mem0.read(u8, 0));
    testing.expectEqual(@as(u32, 0xAAAAAAAA), try mem0.read(u32, 0));
    try mem0.write(u8, 0, 15);
    testing.expectEqual(@as(u8, 15), try mem0.read(u8, 0));
    testing.expectEqual(@as(u32, 0xAAAAAA0F), try mem0.read(u32, 0));

    try mem0.write(u8, 0xFFFF, 42);
    testing.expectEqual(@as(u8, 42), try mem0.read(u8, 0xFFFF));

    testing.expectError(error.MemoryIndexOutOfBounds, mem0.read(u8, 0xFFFF + 1));
    testing.expectError(error.MemoryIndexOutOfBounds, mem0.read(u16, 0xFFFF));

    _ = try mem0.grow(1);
    testing.expectEqual(@as(u8, 0xAA), try mem0.read(u8, 0xFFFF + 1));
}
