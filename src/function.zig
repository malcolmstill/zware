const std = @import("std");
const ValueType = @import("common.zig").ValueType;
const VirtualMachine = @import("vm.zig").VirtualMachine;
const Instance = @import("instance.zig").Instance;
const Opcode = @import("instruction.zig").Opcode;
const Range = @import("common.zig").Range;

pub const WasmError = error{
    NotImplemented,
    StackUnderflow,
    StackOverflow,
    TrapUnreachable,
    LabelStackUnderflow,
    LabelStackOverflow,
    OperandStackUnderflow,
    ControlStackUnderflow,
    OperandStackOverflow,
    FunctionIndexOutOfBounds,
    BadFunctionIndex,
    ControlStackOverflow,
    BadInstanceIndex,
    DivisionByZero,
    Overflow,
    InvalidConversion,
    OutOfBoundsMemoryAccess,
    IndirectCallTypeMismatch,
    UndefinedElement,
    //
    BadMemoryIndex, // TODO: I think we won't see this with validation
    MemoryIndexOutOfBounds, // TODO: I think we won't see this with validation?
    BadTableIndex,
    TableIndexOutOfBounds,
    BadGlobalIndex,
    GlobalIndexOutOfBounds,
    NegativeDenominator,
    Trap,
    CheckStackSpace,
};

pub const Function = union(enum) {
    function: struct {
        locals_count: usize,
        start: usize,
        required_stack_space: usize,
        params: []const ValueType,
        results: []const ValueType,
        instance: usize,
    },
    host_function: struct {
        func: *const fn (*VirtualMachine) WasmError!void,
        params: []const ValueType,
        results: []const ValueType,
    },
};

pub const Code = struct {
    start: usize,
    locals_count: usize,
    required_stack_space: usize,
};
