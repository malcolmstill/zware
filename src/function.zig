const ValueType = @import("common.zig").ValueType;

pub const Function = struct {
    locals: []const u8,
    locals_count: usize,
    code: []const u8,
    params: []ValueType,
    results: []ValueType,
};
