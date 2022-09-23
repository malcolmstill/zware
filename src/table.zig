const std = @import("std");
const mem = std.mem;
const RefType = @import("common.zig").RefType;
const ArrayList = std.ArrayList;

pub const Table = struct {
    alloc: mem.Allocator,
    data: ArrayList(?usize),
    min: u32,
    max: ?u32,
    reftype: RefType,

    pub fn init(alloc: mem.Allocator, reftype: RefType, min: u32, max: ?u32) !Table {
        var data = ArrayList(?usize).init(alloc);
        try data.appendNTimes(null, min);

        return Table{
            .alloc = alloc,
            .data = data,
            .min = min,
            .max = max,
            .reftype = reftype,
        };
    }

    pub fn lookup(self: *Table, index: u32) !usize {
        if (index >= self.data.items.len) return error.OutOfBoundsMemoryAccess;
        return self.data.items[index] orelse return error.UndefinedElement;
    }

    pub fn get(self: *Table, index: u32) !?usize {
        if (index >= self.data.items.len) return error.OutOfBoundsMemoryAccess;
        return self.data.items[index];
    }

    pub fn set(self: *Table, index: u32, value: ?usize) !void {
        if (index >= self.data.items.len) return error.OutOfBoundsMemoryAccess;
        self.data.items[index] = value;
    }

    pub fn size(self: *Table) usize {
        return self.data.items.len;
    }

    pub fn grow(self: *Table, n: u32) !usize {
        if (self.max) |max| {
            if (self.data.items.len + n > max) return error.TableGrowWouldExceedMax;
        }

        const old_size = self.data.items.len;

        try self.data.appendNTimes(null, n);

        return old_size;
    }
};
