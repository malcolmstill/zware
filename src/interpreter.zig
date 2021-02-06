const std = @import("std");
const mem = std.mem;
const LinearFifo = std.fifo.LinearFifo;
const ArrayList = std.ArrayList;
const ValueType = @import("module.zig").ValueType;
const Instruction = @import("instruction.zig").Instruction;

pub const Interpreter = struct {
    op_stack: []u64 = undefined,
    op_stack_size: usize = 0,
    ctrl_stack: []ControlFrame = undefined,

    pub fn init(op_stack: []u64, ctrl_stack: []ControlFrame) Interpreter {
        return Interpreter{
            .op_stack = op_stack,
            .ctrl_stack = ctrl_stack,
        };
    }

    pub fn interpret(i: *Interpreter, opcode: Instruction) !void {
        switch (opcode) {
            .I32Add => {
                const a = try i.popOperand(i32);
                const b = try i.popOperand(i32);
                try i.pushOperand(i32, a + b);
            },
            else => unreachable,
        }
    }

    fn pushOperand(i: *Interpreter, comptime T: type, value: T) !void {
        if (i.op_stack_size == i.op_stack.len) return error.OperandStackOverflow;
        i.op_stack_size += 1;
        i.op_stack[i.op_stack_size - 1] = switch (T) {
            i32 => @bitCast(u64, @intCast(i64, value)),
            i64 => @bitCast(u64, value),
            f32 => @bitCast(u64, @floatCast(f64, value)),
            f64 => @bitCast(u64, value),
            else => |t| @compileError("Unsupported operand type: " ++ @typeName(t)),
        };
    }

    fn popOperand(i: *Interpreter, comptime T: type) !T {
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
};

const ControlFrame = struct {
    arity: usize = 0,
};

const testing = std.testing;

test "operand push / pop test" {
    var op_stack: [6]u64 = [_]u64{0} ** 6;
    var ctrl_stack: [1024]ControlFrame = [_]ControlFrame{undefined} ** 1024;
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
    var ctrl_stack: [1024]ControlFrame = [_]ControlFrame{undefined} ** 1024;
    var i = Interpreter.init(op_stack[0..], ctrl_stack[0..]);

    try i.pushOperand(i32, 22);
    try i.pushOperand(i32, -23);

    try i.interpret(.I32Add);

    testing.expectEqual(@as(i32, -1), try i.popOperand(i32));
}
