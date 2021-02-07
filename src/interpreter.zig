const std = @import("std");
const mem = std.mem;
const leb = std.debug.leb;
const ArrayList = std.ArrayList;
const ValueType = @import("module.zig").ValueType;
const Instruction = @import("instruction.zig").Instruction;

// Interpreter:
//
// The Interpreter interprets WebAssembly bytecode, i.e. it
// is the engine of execution and the whole reason we're here.
//
// Whilst executing code, the Interpreter maintains three stacks.
// An operand stack, a control stack and a label stack.
// The WebAssembly spec models execution as a single stack where operands,
// activation frames, and labels are all interleaved. Here we split
// those out for convenience.
//
// Note: I had considered four stacks (separating out the params / locals) to
// there own stack, but I don't think that's necessary.
//
pub const Interpreter = struct {
    op_stack: []u64 = undefined,
    op_stack_size: usize = 0,
    ctrl_stack: []ControlFrame = undefined,
    ctrl_stack_size: usize = 0,
    function_code: []const u8 = undefined,
    window: []const u8 = undefined,

    pub fn init(op_stack: []u64, ctrl_stack: []ControlFrame) Interpreter {
        return Interpreter{
            .op_stack = op_stack,
            .ctrl_stack = ctrl_stack,
        };
    }

    pub fn interpretFunction(i: *Interpreter, code: []const u8) !void {
        i.function_code = code;
        i.window = code;
        while (i.window.len > 0) {
            const instr = i.window[0];
            i.window = i.window[1..];
            try i.interpret(@intToEnum(Instruction, instr));
        }
    }

    pub fn interpret(i: *Interpreter, opcode: Instruction) !void {
        switch (opcode) {
            .Unreachable => return error.TrapUnreachable,
            .Nop => return,
            .End => return,
            .Drop => try i.popAnyOperand(),
            .LocalGet => {
                const frame = i.ctrl_stack[i.ctrl_stack_size - 1];
                const local_index = try readULEB128Mem(u32, &i.window);
                const local_value: u64 = frame.locals[local_index];
                try i.pushOperand(u64, local_value);
            },
            .I32Add => {
                // TODO: does wasm wrap?
                const a = try i.popOperand(i32);
                const b = try i.popOperand(i32);
                try i.pushOperand(i32, a + b);
            },
            .I32Sub => {
                const a = try i.popOperand(i32);
                const b = try i.popOperand(i32);
                try i.pushOperand(i32, b - a);
            },
            .I32Mul => {
                const a = try i.popOperand(i32);
                const b = try i.popOperand(i32);
                try i.pushOperand(i32, a * b);
            },
            .I64Add => {
                const a = try i.popOperand(i64);
                const b = try i.popOperand(i64);
                try i.pushOperand(i64, a + b);
            },
            .F32Add => {
                const a = try i.popOperand(f32);
                const b = try i.popOperand(f32);
                try i.pushOperand(f32, a + b);
            },
            .F64Add => {
                const a = try i.popOperand(f64);
                const b = try i.popOperand(f64);
                try i.pushOperand(f64, a + b);
            },
            else => {
                std.debug.warn("unimplemented instruction: {}\n", .{opcode});
                unreachable;
            },
        }
    }

    pub fn pushOperand(i: *Interpreter, comptime T: type, value: T) !void {
        // TODO: if we've validated the wasm, do we need to perform this check:
        if (i.op_stack_size == i.op_stack.len) return error.OperandStackOverflow;
        i.op_stack_size += 1;
        i.op_stack[i.op_stack_size - 1] = switch (T) {
            i32 => @bitCast(u64, @intCast(i64, value)),
            i64 => @bitCast(u64, value),
            f32 => @bitCast(u64, @floatCast(f64, value)),
            f64 => @bitCast(u64, value),
            u64 => value,
            else => |t| @compileError("Unsupported operand type: " ++ @typeName(t)),
        };
    }

    pub fn popOperand(i: *Interpreter, comptime T: type) !T {
        if (i.op_stack_size == 0) return error.OperandStackUnderflow;
        defer i.op_stack_size -= 1;

        const value = i.op_stack[i.op_stack_size - 1];
        return switch (T) {
            i32 => @intCast(i32, @bitCast(i64, value)),
            i64 => @bitCast(i64, value),
            f32 => @floatCast(f32, @bitCast(f64, value)),
            f64 => @bitCast(f64, value),
            else => |t| @compileError("Unsupported operand type: " ++ @typeName(t)),
        };
    }

    pub fn popAnyOperand(i: *Interpreter) !void {
        if (i.op_stack_size == 0) return error.OperandStackUnderflow;
        i.op_stack_size -= 1;
    }

    pub fn pushControlFrame(i: *Interpreter, frame: ControlFrame, params_and_locals_count: usize) !void {
        if (i.ctrl_stack_size == i.ctrl_stack.len) return error.ControlStackOverflow;
        i.ctrl_stack_size += 1;
        i.ctrl_stack[i.ctrl_stack_size - 1] = frame;
        i.ctrl_stack[i.ctrl_stack_size - 1].locals = i.op_stack[i.op_stack_size .. i.op_stack_size + params_and_locals_count];
    }

    pub const ControlFrame = struct {
        locals: []u64 = undefined,
        return_arity: usize = 0,
    };
};

pub fn readULEB128Mem(comptime T: type, ptr: *[]const u8) !T {
    var buf = std.io.fixedBufferStream(ptr.*);
    const value = try leb.readULEB128(T, buf.reader());
    ptr.*.ptr += buf.pos;
    ptr.*.len -= buf.pos;
    return value;
}

const testing = std.testing;

test "operand push / pop test" {
    var op_stack: [6]u64 = [_]u64{0} ** 6;
    var ctrl_stack: [1024]Interpreter.ControlFrame = [_]Interpreter.ControlFrame{undefined} ** 1024;

    var i = Interpreter.init(op_stack[0..], ctrl_stack[0..]);

    try i.pushOperand(i32, 22);
    try i.pushOperand(i32, -23);
    try i.pushOperand(i64, 44);
    try i.pushOperand(i64, -43);
    try i.pushOperand(f32, 22.07);
    try i.pushOperand(f64, 43.07);

    // stack overflow:
    if (i.pushOperand(i32, 0)) |r| {
        return error.TestExpectedError;
    } else |err| {
        if (err != error.OperandStackOverflow) return error.TestUnexpectedError;
    }

    testing.expectEqual(@as(f64, 43.07), try i.popOperand(f64));
    testing.expectEqual(@as(f32, 22.07), try i.popOperand(f32));
    testing.expectEqual(@as(i64, -43), try i.popOperand(i64));
    testing.expectEqual(@as(i64, 44), try i.popOperand(i64));
    testing.expectEqual(@as(i32, -23), try i.popOperand(i32));
    testing.expectEqual(@as(i32, 22), try i.popOperand(i32));

    // stack underflow:
    if (i.popOperand(i32)) |r| {
        return error.TestExpectedError;
    } else |err| {
        if (err != error.OperandStackUnderflow) return error.TestUnexpectedError;
    }
}

test "simple interpret tests" {
    var op_stack: [6]u64 = [_]u64{0} ** 6;
    var ctrl_stack: [1024]Interpreter.ControlFrame = [_]Interpreter.ControlFrame{undefined} ** 1024;
    var i = Interpreter.init(op_stack[0..], ctrl_stack[0..]);

    try i.pushOperand(i32, 22);
    try i.pushOperand(i32, -23);

    try i.interpret(.I32Add);

    testing.expectEqual(@as(i32, -1), try i.popOperand(i32));
}
