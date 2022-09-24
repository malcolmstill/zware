const std = @import("std");
const mem = std.mem;
const RefType = @import("../valtype.zig").RefType;

pub const Elem = struct {
    @"type": RefType,
    elem: []u32,
    alloc: mem.Allocator,
    dropped: bool = false,

    pub fn init(alloc: mem.Allocator, reftype: RefType, count: u32) !Elem {
        const elem = try alloc.alloc(u32, count);

        return Elem{
            .@"type" = reftype,
            .elem = elem,
            .alloc = alloc,
        };
    }

    pub fn deinit(self: *Elem) void {
        self.alloc.free(self.elem);
    }

    pub fn set(self: *Elem, index: usize, value: u32) !void {
        if (index >= self.elem.len) return error.ElemIndexOutOfBounds;
        self.elem[index] = value;
    }
};
