const ValueType = @import("common.zig").ValueType;
const Interpreter = @import("interpreter.zig").Interpreter;
const Instance = @import("instance.zig").Instance;

pub const Function = union(enum) {
    function: struct {
        locals: []const u8,
        locals_count: usize,
        code: []const u8,
        params: []const ValueType,
        results: []const ValueType,
        instance: *Instance,
    },
    host_function: struct {
        func: fn (*Interpreter) anyerror!void,
        params: []const ValueType,
        results: []const ValueType,
    },
};
