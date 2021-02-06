const std = @import("std");
const mem = std.mem;
const LinearFifo = std.fifo.LinearFifo;
const ArrayList = std.ArrayList;
const ValueType = @import("module.zig").ValueType;
const Instruction = @import("instruction.zig").Instruction;

pub const Validator = struct {
    op_stack: OperandStack = undefined,
    ctrl_stack: ControlStack = undefined,

    pub fn init(alloc: *mem.Allocator) Validator {
        return Validator{
            .op_stack = OperandStack.init(alloc),
            .ctrl_stack = ControlStack.init(alloc),
        };
    }

    pub fn validate(v: *Validator, opcode: Instruction) !void {
        switch (opcode) {
            .I32Add => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I32 });
            },
            .I64Add => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I64 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I64 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I64 });
            },
            .F32Add => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F32 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F32 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .F32 });
            },
            .F64Add => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F64 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F64 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .F64 });
            },
            .I32Const => {
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I32 });
            },
            .I64Const => {
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I64 });
            },
            .F32Const => {
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .F32 });
            },
            .F64Const => {
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .F64 });
            },
            else => unreachable,
        }
    }

    fn pushOperand(v: *Validator, t: ValueTypeUnknown) !void {
        try v.op_stack.append(t);
    }

    fn popOperand(v: *Validator) !ValueTypeUnknown {
        if (v.ctrl_stack.items.len == 0) return error.ControlStackEmpty;
        const ctrl_frame = v.ctrl_stack.items[v.ctrl_stack.items.len - 1];
        if (v.op_stack.items.len == ctrl_frame.height and ctrl_frame.unreachable_flag) {
            return ValueTypeUnknown.Unknown;
        }
        if (v.op_stack.items.len == ctrl_frame.height) return error.ValidatorPopOperandError;
        return v.op_stack.pop();
    }

    fn popOperandExpecting(v: *Validator, expected: ValueTypeUnknown) !ValueTypeUnknown {
        const actual = try v.popOperand();

        const actual_type: ValueType = switch (actual) {
            .Unknown => return expected,
            .Known => |k| k,
        };

        const expected_type: ValueType = switch (expected) {
            .Unknown => return actual,
            .Known => |k| k,
        };

        if (actual_type != expected_type) return error.MismatchedTypes;
        return actual;
    }

    fn pushOperands(v: *Validator, operands: []const ValueType) !void {
        for (operands) |op| {
            try v.pushOperand(ValueTypeUnknown{ .Known = op });
        }
    }

    fn popOperands(v: *Validator, operands: []const ValueType) !void {
        const len = operands.len;
        for (operands) |op, i| {
            try popOperandExpecting(operands[len - i]);
        }
    }

    fn pushControlFrame(v: *Validator, opcode: Instruction, in: []ValueType, out: []ValueType) !void {
        const frame = ControlFrame{
            .opcode = opcode,
            .start_types = in,
            .end_types = out,
            .height = v.op_stack.items.len,
            .unreachable_flag = false,
        };
        try v.ctrl_stack.append(frame);
        try v.pushOperands(in);
    }

    fn popControlFrame(v: *Validator) !ControlFrame {
        if (v.ctrl_stack.items.len == 0) return error.ValidatorPopControlFrameControlStackEmpty;
        const frame = v.ctrl_stack.items[v.ctrl_stack.items - 1];
        try popOperands(frame.end_types);
        if (v.op_stack.items.len != frame.height) return error.ValidatorPopControlFrameMismatchedSizes;
        _ = v.ctrl_stack.pop();
        return frame;
    }

    fn labelTypes(v: *Validator, frame: ControlFrame) []ValueTypes {
        if (frame.opcode == Instruction.Loop) {
            return frame.start_types;
        } else {
            return frame.end_types;
        }
    }

    fn setUnreachable(v: *Validator) void {}
};

const ValueTypeUnknownTag = enum {
    Known,
    Unknown,
};

const ValueTypeUnknown = union(ValueTypeUnknownTag) {
    Known: ValueType,
    Unknown: void,
};

const OperandStack = ArrayList(ValueTypeUnknown);
const ControlStack = ArrayList(ControlFrame);

const ControlFrame = struct {
    opcode: Instruction = undefined,
    start_types: []const ValueType,
    end_types: []const ValueType,
    height: usize = 0,
    unreachable_flag: bool = false,
};

const testing = std.testing;
test "validate add i32" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var v = Validator.init(&arena.allocator);

    var in: [0]ValueType = [_]ValueType{} ** 0;
    var out: [1]ValueType = [_]ValueType{.I32} ** 1;
    _ = try v.pushControlFrame(.Nop, in[0..], out[0..]);
    _ = try v.validate(.I32Const);
    _ = try v.validate(.I32Const);
    _ = try v.validate(.I32Add);
}

test "validate add i64" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var v = Validator.init(&arena.allocator);

    var in: [0]ValueType = [_]ValueType{} ** 0;
    var out: [1]ValueType = [_]ValueType{.I64} ** 1;
    _ = try v.pushControlFrame(.Nop, in[0..], out[0..]);
    _ = try v.validate(.I64Const);
    _ = try v.validate(.I64Const);
    _ = try v.validate(.I64Add);
}

test "validate add f32" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var v = Validator.init(&arena.allocator);

    var in: [0]ValueType = [_]ValueType{} ** 0;
    var out: [1]ValueType = [_]ValueType{.F32} ** 1;
    _ = try v.pushControlFrame(.Nop, in[0..], out[0..]);
    _ = try v.validate(.F32Const);
    _ = try v.validate(.F32Const);
    _ = try v.validate(.F32Add);
}

test "validate add f64" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var v = Validator.init(&arena.allocator);

    var in: [0]ValueType = [_]ValueType{} ** 0;
    var out: [1]ValueType = [_]ValueType{.F64} ** 1;
    _ = try v.pushControlFrame(.Nop, in[0..], out[0..]);
    _ = try v.validate(.F64Const);
    _ = try v.validate(.F64Const);
    _ = try v.validate(.F64Add);
}

test "validate: add error on mismatched types" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var v = Validator.init(&arena.allocator);

    var in: [0]ValueType = [_]ValueType{} ** 0;
    var out: [1]ValueType = [_]ValueType{.I32} ** 1;
    _ = try v.pushControlFrame(.Block, in[0..], out[0..]);
    _ = try v.validate(.I64Const);
    _ = try v.validate(.I32Const);
    _ = v.validate(.I32Add) catch |err| {
        if (err == error.MismatchedTypes) return;
    };
    return error.ExpectedFailure;
}
