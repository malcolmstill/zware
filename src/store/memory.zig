const std = @import("std");
const mem = std.mem;
const ArrayList = std.ArrayList;

pub const MAX_PAGES = 64 * 1024;
pub const PAGE_SIZE = 64 * 1024;

pub const Memory = struct {
    alloc: mem.Allocator,
    min: u32,
    max: ?u32 = null,
    data: ArrayList(u8),

    pub fn init(alloc: mem.Allocator, min: u32, max: ?u32) Memory {
        return Memory{
            .alloc = alloc,
            .data = ArrayList(u8).init(alloc),
            .min = min,
            .max = max,
        };
    }

    pub fn deinit(self: *Memory) void {
        self.data.deinit();
    }

    // Return the size of the memory (in pages)
    pub fn size(self: *Memory) u32 {
        std.debug.assert(self.data.items.len % PAGE_SIZE == 0);

        return @truncate(self.data.items.len / PAGE_SIZE);
    }

    pub fn sizeBytes(self: *Memory) u33 {
        // FIXME: add assertion that len fits in u33
        return @truncate(self.data.items.len);
    }

    pub fn grow(self: *Memory, num_pages: u32) !usize {
        if (self.size() + num_pages > @min(self.max orelse MAX_PAGES, MAX_PAGES)) return error.OutOfBoundsMemoryAccess;

        const old_size_in_bytes = self.data.items.len;
        const old_size_in_pages = self.size();
        _ = try self.data.resize(self.data.items.len + PAGE_SIZE * num_pages);

        // Zero memory. FIXME: I don't think this is required (maybe do this only in debug build)
        @memset(self.data.items[old_size_in_bytes .. old_size_in_bytes + PAGE_SIZE * num_pages], 0);

        return old_size_in_pages;
    }

    pub fn copy(self: *Memory, address: u32, data: []const u8) !void {
        if (address + data.len > self.data.items.len) return error.OutOfBoundsMemoryAccess;

        mem.copy(u8, self.data.items[address .. address + data.len], data);
    }

   pub fn uncheckedFill(self: *Memory, dst_address: u32, n: u32, value: u8) void {
        @memset(self.data.items[dst_address .. dst_address + n], value);
    }

    pub fn uncheckedCopy(self: *Memory, dst_address: u32, data: []const u8) void {
        mem.copy(u8, self.data.items[dst_address .. dst_address + data.len], data);
    }

    pub fn uncheckedCopyBackwards(self: *Memory, dst_address: u32, data: []const u8) void {
        mem.copyBackwards(u8, self.data.items[dst_address .. dst_address + data.len], data);
    }

    // as per copy but don't actually mutate
    pub fn check(self: *Memory, address: u32, data: []const u8) !void {
        if (address + data.len > self.data.items.len) return error.OutOfBoundsMemoryAccess;
    }

    pub fn read(self: *Memory, comptime T: type, offset: u32, address: u32) !T {
        const effective_address = @as(u33, offset) + @as(u33, address);
        if (effective_address + @sizeOf(T) - 1 >= self.data.items.len) return error.OutOfBoundsMemoryAccess;


        switch (T) {
            u8,
            u16,
            u32,
            u64,
            i8,
            i16,
            i32,
            i64,
            => return mem.readInt(T, @as(*const [@sizeOf(T)]u8, @ptrCast(&self.data.items[effective_address])), .Little),
            f32 => {
                const x = mem.readInt(u32, @as(*const [@sizeOf(T)]u8, @ptrCast(&self.data.items[effective_address])), .Little);
                return @bitCast(x);
            },
            f64 => {
                const x = mem.readInt(u64, @as(*const [@sizeOf(T)]u8, @ptrCast(&self.data.items[effective_address])), .Little);
                return @bitCast(x);
            },
            else => @compileError("Memory.read unsupported type (not int/float): " ++ @typeName(T)),
        }
    }

    pub fn write(self: *Memory, comptime T: type, offset: u32, address: u32, value: T) !void {
        const effective_address = @as(u33, offset) + @as(u33, address);
        if (effective_address + @sizeOf(T) - 1 >= self.data.items.len) return error.OutOfBoundsMemoryAccess;

        switch (T) {
            u8,
            u16,
            u32,
            u64,
            i8,
            i16,
            i32,
            i64,
            => std.mem.writeInt(T, @as(*[@sizeOf(T)]u8, @ptrCast(&self.data.items[effective_address])), value, .Little),
            f32 => {
                const x: u32 = @bitCast(value);
                std.mem.writeInt(u32, @as(*[@sizeOf(u32)]u8, @ptrCast(&self.data.items[effective_address])), x, .Little);
            },
            f64 => {
                const x: u64 = @bitCast(value);
                std.mem.writeInt(u64, @as(*[@sizeOf(u64)]u8, @ptrCast(&self.data.items[effective_address])), x, .Little);
            },
            else => @compileError("Memory.read unsupported type (not int/float): " ++ @typeName(T)),
        }
    }

    pub fn memory(self: *Memory) []u8 {
        return self.data.items[0..];
    }
};

const testing = std.testing;
test "Memory test" {
    const ArrayListStore = @import("../store.zig").ArrayListStore;
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer _ = arena.deinit();

    const alloc = arena.allocator();

    var store = ArrayListStore.init(alloc);
    const mem_handle = try store.addMemory(0, null);
    var mem0 = try store.memory(mem_handle);
    try testing.expectEqual(@as(usize, 0), mem0.sizeBytes());

    _ = try mem0.grow(1);

    try testing.expectEqual(@as(usize, 1 * PAGE_SIZE), mem0.sizeBytes());
    try testing.expectEqual(@as(usize, 1), mem0.size());

    try testing.expectEqual(@as(u8, 0x00), try mem0.read(u8, 0, 0));
    try testing.expectEqual(@as(u32, 0x00000000), try mem0.read(u32, 0, 0));
    try mem0.write(u8, 0, 0, 15);
    try testing.expectEqual(@as(u8, 15), try mem0.read(u8, 0, 0));
    try testing.expectEqual(@as(u32, 0x0000000F), try mem0.read(u32, 0, 0));

    try mem0.write(u8, 0, 0xFFFF, 42);
    try testing.expectEqual(@as(u8, 42), try mem0.read(u8, 0, 0xFFFF));
    try testing.expectError(error.OutOfBoundsMemoryAccess, mem0.read(u16, 0, 0xFFFF));
    try testing.expectError(error.OutOfBoundsMemoryAccess, mem0.read(u8, 0, 0xFFFF + 1));

    _ = try mem0.grow(1);
    try testing.expectEqual(@as(usize, 2 * PAGE_SIZE), mem0.sizeBytes());
    try testing.expectEqual(@as(usize, 2), mem0.size());

    try testing.expectEqual(@as(u8, 0x00), try mem0.read(u8, 0, 0xFFFF + 1));
    // Write across page boundary
    try mem0.write(u16, 0, 0xFFFF, 0xDEAD);
    try testing.expectEqual(@as(u8, 0xAD), try mem0.read(u8, 0, 0xFFFF));
    try testing.expectEqual(@as(u8, 0xDE), try mem0.read(u8, 0, 0xFFFF + 1));
    try testing.expectEqual(@as(u16, 0xDEAD), try mem0.read(u16, 0, 0xFFFF));
    const slice = mem0.memory();
    try testing.expectEqual(@as(u8, 0xAD), slice[0xFFFF]);
    try testing.expectEqual(@as(u8, 0xDE), slice[0xFFFF + 1]);

    try testing.expectEqual(@as(u8, 0x00), try mem0.read(u8, 0, 0x1FFFF));
    try testing.expectError(error.OutOfBoundsMemoryAccess, mem0.read(u8, 0, 0x1FFFF + 1));

    mem0.max = 2;
    try testing.expectError(error.OutOfBoundsMemoryAccess, mem0.grow(1));

    mem0.max = null;
    _ = try mem0.grow(1);
    try testing.expectEqual(@as(usize, 3), mem0.size());

    try testing.expectEqual(@as(usize, 3 * PAGE_SIZE), mem0.sizeBytes());
}
