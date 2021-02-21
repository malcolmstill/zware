const std = @import("std");
const mem = std.mem;

pub const Table = struct {
    data: []?usize,
    min: u32,
    max: ?u32,

    pub fn init(alloc: *mem.Allocator, min: u32, max: ?u32) !Table {
        return Table{
            .data = try alloc.alloc(?usize, min),
            .min = min,
            .max = max,
        };
    }

    pub fn lookup(self: *Table, index: u32) !usize {
        if (index >= self.data.len) return error.OutOfBoundsMemoryAccess;
        return self.data[index] orelse return error.UndefinedElement;
    }

    pub fn set(self: *Table, index: u32, value: usize) !void {
        if (index >= self.data.len) return error.OutOfBoundsMemoryAccess;
        self.data[index] = value;
    }

    pub fn size(self: *Table) usize {
        return self.data.len;
    }
};
