const ValueType = @import("common.zig").ValueType;
const Interpreter = @import("interpreter.zig").Interpreter;

pub const Function = union(enum) {
    function: struct {
        locals: []const u8,
        locals_count: usize,
        code: []const u8,
        params: []ValueType,
        results: []ValueType,
    },
    host_function: struct {
        func: fn (*Interpreter) anyerror!void,
        params: []ValueType,
        results: []ValueType,
    },
};
