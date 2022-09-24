const std = @import("std");
const ValType = @import("../valtype.zig").ValType;
const VirtualMachine = @import("../vm.zig").VirtualMachine;
const WasmError = @import("../vm.zig").WasmError;
const Instance = @import("../instance.zig").Instance;

pub const Function = union(enum) {
    function: struct {
        locals_count: usize,
        start: usize,
        required_stack_space: usize,
        params: []const ValType,
        results: []const ValType,
        instance: usize,
    },
    host_function: struct {
        func: *const fn (*VirtualMachine) WasmError!void,
        params: []const ValType,
        results: []const ValType,
    },
};

pub const Code = struct {
    start: usize,
    locals_count: usize,
    required_stack_space: usize,
};
