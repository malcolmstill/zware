const std = @import("std");
const mem = std.mem;
const math = std.math;
const ArrayList = std.ArrayList;

pub const MAX_PAGES = 64 * 1024;
pub const PAGE_SIZE = 64 * 1024;

pub const Memory = struct {
    alloc: *mem.Allocator,
    min: u32,
    max: ?u32 = null,
    data: ArrayList([PAGE_SIZE]u8),

    pub fn init(alloc: *mem.Allocator, min: u32, max: ?u32) Memory {
        return Memory{
            .alloc = alloc,
            .data = ArrayList([PAGE_SIZE]u8).init(alloc),
            .min = min,
            .max = max,
        };
    }

    // TODO: is usize correct here?
    pub fn size(self: *Memory) usize {
        return self.data.items.len;
    }

    pub fn grow(self: *Memory, num_pages: u32) !usize {
        if (self.data.items.len + num_pages > math.min(self.max orelse MAX_PAGES, MAX_PAGES)) return error.OutOfBoundsMemoryAccess;

        const old_size = self.data.items.len;
        _ = try self.data.resize(self.data.items.len + num_pages);
        mem.set(u8, self.asSlice()[PAGE_SIZE * old_size .. PAGE_SIZE * (old_size + num_pages)], 0);
        return old_size;
    }

    pub fn copy(self: *Memory, address: u32, data: []const u8) !void {
        if (address + data.len > PAGE_SIZE * self.data.items.len) return error.OutOfBoundsMemoryAccess;

        mem.copy(u8, self.asSlice()[address .. address + data.len], data);
    }

    // as per copy but don't actually mutate
    pub fn check(self: *Memory, address: u32, data: []const u8) !void {
        if (address + data.len > PAGE_SIZE * self.data.items.len) return error.OutOfBoundsMemoryAccess;
    }

    pub fn read(self: *Memory, comptime T: type, offset: u32, address: u32) !T {
        const effective_address = @as(u33, offset) + @as(u33, address);
        if (effective_address + @sizeOf(T) - 1 >= PAGE_SIZE * self.data.items.len) return error.OutOfBoundsMemoryAccess;

        const page = effective_address / PAGE_SIZE;
        const page_offset = effective_address % PAGE_SIZE;

        switch (T) {
            u8,
            u16,
            u32,
            u64,
            i8,
            i16,
            i32,
            i64,
            => return mem.readInt(T, @ptrCast(*const [@sizeOf(T)]u8, &self.data.items[page][page_offset]), .Little),
            f32 => {
                const x = mem.readInt(u32, @ptrCast(*const [@sizeOf(T)]u8, &self.data.items[page][page_offset]), .Little);
                return @bitCast(f32, x);
            },
            f64 => {
                const x = mem.readInt(u64, @ptrCast(*const [@sizeOf(T)]u8, &self.data.items[page][page_offset]), .Little);
                return @bitCast(f64, x);
            },
            else => @compileError("Memory.read unsupported type (not int/float): " ++ @typeName(T)),
        }
    }

    pub fn write(self: *Memory, comptime T: type, offset: u32, address: u32, value: T) !void {
        const effective_address = @as(u33, offset) + @as(u33, address);
        if (effective_address + @sizeOf(T) - 1 >= PAGE_SIZE * self.data.items.len) return error.OutOfBoundsMemoryAccess;

        const page = effective_address / PAGE_SIZE;
        const page_offset = effective_address % PAGE_SIZE;

        switch (T) {
            u8,
            u16,
            u32,
            u64,
            i8,
            i16,
            i32,
            i64,
            => std.mem.writeInt(T, @ptrCast(*[@sizeOf(T)]u8, &self.data.items[page][page_offset]), value, .Little),
            f32 => {
                const x = @bitCast(u32, value);
                std.mem.writeInt(u32, @ptrCast(*[@sizeOf(u32)]u8, &self.data.items[page][page_offset]), x, .Little);
            },
            f64 => {
                const x = @bitCast(u64, value);
                std.mem.writeInt(u64, @ptrCast(*[@sizeOf(u64)]u8, &self.data.items[page][page_offset]), x, .Little);
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
    const ArrayListStore = @import("store.zig").ArrayListStore;
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer _ = arena.deinit();

    var store = ArrayListStore.init(&arena.allocator);
    const mem_handle = try store.addMemory(0, null);
    var mem0 = try store.memory(mem_handle);
    try testing.expectEqual(@as(usize, 0), mem0.asSlice().len);

    _ = try mem0.grow(1);
    try testing.expectEqual(@as(usize, 1 * PAGE_SIZE), mem0.asSlice().len);
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
    try testing.expectEqual(@as(usize, 2 * PAGE_SIZE), mem0.asSlice().len);
    try testing.expectEqual(@as(usize, 2), mem0.size());

    try testing.expectEqual(@as(u8, 0x00), try mem0.read(u8, 0, 0xFFFF + 1));
    // Write across page boundary
    try mem0.write(u16, 0, 0xFFFF, 0xDEAD);
    try testing.expectEqual(@as(u8, 0xAD), try mem0.read(u8, 0, 0xFFFF));
    try testing.expectEqual(@as(u8, 0xDE), try mem0.read(u8, 0, 0xFFFF + 1));
    try testing.expectEqual(@as(u16, 0xDEAD), try mem0.read(u16, 0, 0xFFFF));
    const slice = mem0.asSlice();
    try testing.expectEqual(@as(u8, 0xAD), slice[0xFFFF]);
    try testing.expectEqual(@as(u8, 0xDE), slice[0xFFFF + 1]);

    try testing.expectEqual(@as(u8, 0x00), try mem0.read(u8, 0, 0x1FFFF));
    try testing.expectError(error.OutOfBoundsMemoryAccess, mem0.read(u8, 0, 0x1FFFF + 1));

    mem0.max = 2;
    try testing.expectError(error.OutOfBoundsMemoryAccess, mem0.grow(1));

    mem0.max = null;
    _ = try mem0.grow(1);
    try testing.expectEqual(@as(usize, 3), mem0.size());

    try testing.expectEqual(@as(usize, 3 * PAGE_SIZE), mem0.asSlice().len);
}
