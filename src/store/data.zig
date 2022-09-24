const std = @import("std");
const mem = std.mem;

pub const Data = struct {
    data: []u8,
    alloc: mem.Allocator,
    dropped: bool = false,

    pub fn init(alloc: mem.Allocator, count: u32) !Data {
        const data = try alloc.alloc(u8, count);

        return Data{
            .data = data,
            .alloc = alloc,
        };
    }

    pub fn deinit(self: *Data) void {
        self.alloc.free(self.data);
    }

    pub fn set(self: *Data, index: usize, value: u8) !void {
        if (index >= self.data.len) return error.DataIndexOutOfBounds;
        self.data[index] = value;
    }
};
