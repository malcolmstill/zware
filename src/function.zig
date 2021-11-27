const std = @import("std");
const ValueType = @import("common.zig").ValueType;
const Interpreter = @import("interpreter.zig").Interpreter;
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
};

pub const Function = union(enum) {
    function: struct {
        // locals: []const u8,
        locals_count: usize,
        // code: []Instruction,
        ip_start: usize,
        ip_end: usize,
        params: []const ValueType,
        results: []const ValueType,
        instance: usize,
    },
    host_function: struct {
        func: fn (*Interpreter) WasmError!void,
        params: []const ValueType,
        results: []const ValueType,
    },
};

pub const Code = struct {
    // locals: []const u8,
    locals_count: usize,
    code: Range,
};
