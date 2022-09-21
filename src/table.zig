const std = @import("std");
const mem = std.mem;
const RefType = @import("common.zig").RefType;

pub const Table = struct {
    data: []?usize,
    min: u32,
    max: ?u32,
    reftype: RefType,

    pub fn init(alloc: mem.Allocator, reftype: RefType, min: u32, max: ?u32) !Table {
        const data = try alloc.alloc(?usize, min);
        mem.set(?usize, data, null);

        return Table{
            .data = data,
            .min = min,
            .max = max,
            .reftype = reftype,
        };
    }

    pub fn lookup(self: *Table, index: u32) !usize {
        if (index >= self.data.len) return error.OutOfBoundsMemoryAccess;
        return self.data[index] orelse return error.UndefinedElement;
    }

    pub fn get(self: *Table, index: u32) !?usize {
        if (index >= self.data.len) return error.OutOfBoundsMemoryAccess;
        return self.data[index];
    }

    pub fn set(self: *Table, index: u32, value: usize) !void {
        if (index >= self.data.len) return error.OutOfBoundsMemoryAccess;
        self.data[index] = value;
    }

    pub fn size(self: *Table) usize {
        return self.data.len;
    }
};
