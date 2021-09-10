const std = @import("std");
const mem = std.mem;
const LinearFifo = std.fifo.LinearFifo;
const ArrayList = std.ArrayList;
const FuncType = @import("common.zig").FuncType;
const ValueType = @import("common.zig").ValueType;
const Opcode = @import("instruction.zig").Opcode;

pub const Validator = struct {
    op_stack: OperandStack = undefined,
    ctrl_stack: ControlStack = undefined,

    pub fn init(alloc: *mem.Allocator) Validator {
        return Validator{
            .op_stack = OperandStack.init(alloc),
            .ctrl_stack = ControlStack.init(alloc),
        };
    }

    pub fn validateBlock(v: *Validator, in_operands: []const ValueType, out_operands: []const ValueType) !void {
        try v.popOperands(in_operands);
        try v.pushControlFrame(.block, in_operands, out_operands);
    }

    pub fn validateLoop(v: *Validator, in_operands: []const ValueType, out_operands: []const ValueType) !void {
        try v.popOperands(in_operands);
        try v.pushControlFrame(.loop, in_operands, out_operands);
    }

    pub fn validateIf(v: *Validator, in_operands: []const ValueType, out_operands: []const ValueType) !void {
        _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
        try v.popOperands(in_operands);
        try v.pushControlFrame(.@"if", in_operands, out_operands);
    }

    pub fn validateBr(v: *Validator, label: usize) !void {
        if (v.ctrl_stack.items.len < label) return error.ValidateBrInvalidLabel;
        const frame = v.ctrl_stack.items[label];
        try v.popOperands(v.labelTypes(frame));
        try v.setUnreachable();
    }

    pub fn validateBrIf(v: *Validator, label: usize) !void {
        if (v.ctrl_stack.items.len < label) return error.ValidateBrIfInvalidLabel;
        const frame = v.ctrl_stack.items[label];

        _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
        try v.popOperands(v.labelTypes(frame));
        try v.pushOperands(v.labelTypes(frame));
    }

    pub fn validateBrTable(v: *Validator, n_star: []u32, label: u32) !void {
        _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
        if (v.ctrl_stack.items.len < label) return error.ValidateBrTableInvalidLabel;
        const frame = v.ctrl_stack.items[label];

        const arity = v.labelTypes(frame).len;

        for (n_star) |n| {
            if (v.ctrl_stack.items.len < n) return error.ValidateBrTableInvalidLabel;
            const frame_n = v.ctrl_stack.items[n];
            if (v.labelTypes(frame_n).len != arity) return error.ValidateBrTableInvalidLabel;

            if (!(arity <= 64)) return error.TODOAllocation;

            var temp = [_]ValueTypeUnknown{.{ .Known = .I32 }} ** 64; // TODO: allocate some memory for this
            for (v.labelTypes(frame_n)) |_, i| {
                temp[i] = try v.popOperandExpecting(ValueTypeUnknown{ .Known = v.labelTypes(frame)[arity - i - 1] });
            }

            for (v.labelTypes(frame_n)) |_, i| {
                try v.pushOperand(temp[i]);
            }
        }

        try v.popOperands(v.labelTypes(frame));
        try v.setUnreachable();
    }

    pub fn validateCall(v: *Validator, func_type: FuncType) !void {
        try v.popOperands(func_type.params);
        try v.pushOperands(func_type.results);
    }

    pub fn validateLocalGet(v: *Validator, local_type: ValueType) !void {
        try v.pushOperand(ValueTypeUnknown{ .Known = local_type });
    }

    pub fn validateLocalSet(v: *Validator, local_type: ValueType) !void {
        const t = try v.popOperand();
        if (!valueTypeEqual(ValueTypeUnknown{ .Known = local_type }, t)) return error.ValidatorLocalSetTypeMismatch;
    }

    pub fn validateLocalTee(v: *Validator, local_type: ValueType) !void {
        const t = try v.popOperand();
        if (!valueTypeEqual(ValueTypeUnknown{ .Known = local_type }, t)) return error.ValidatorLocalSetTypeMismatch;
        try v.pushOperand(t);
    }

    pub fn validate(v: *Validator, opcode: Opcode) !void {
        std.log.info("validate {}\n", .{opcode});
        switch (opcode) {
            .@"unreachable" => try v.setUnreachable(),
            .nop => {},
            .end => {
                const frame = try v.popControlFrame();
                _ = try v.pushOperands(frame.end_types);
            },
            .@"else" => {
                const frame = try v.popControlFrame();
                if (frame.opcode != .@"if") return error.ElseMustOnlyOccurAfterIf;
                try v.pushControlFrame(.@"else", frame.start_types, frame.end_types);
            },
            .@"return" => {
                // No idea if this is correct...
                const frame = try v.popControlFrame();
                _ = try v.pushOperands(frame.end_types);
            },
            .drop => {
                _ = try v.popOperand();
            },
            .select => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                const t1 = try v.popOperand();
                const t2 = try v.popOperand();

                if (!(isNum(t1) and isNum(t2))) return error.ExpectingBothNum;
                if (valueTypeEqual(t1, t2) and !valueTypeEqual(t1, ValueTypeUnknown.Unknown) and !valueTypeEqual(t2, ValueTypeUnknown.Unknown)) return error.ValidatorSelect;
                if (valueTypeEqual(t1, ValueTypeUnknown.Unknown)) {
                    try v.pushOperand(t2);
                } else {
                    try v.pushOperand(t1);
                }
            },
            .@"i32.extend8_s", .@"i32.extend16_s", .@"i32.eqz", .@"memory.grow", .@"i32.load" => {
                // no change
            },
            .@"i32.add",
            .@"i32.sub",
            .@"i32.eq",
            .@"i32.ne",
            .@"i32.le_s",
            .@"i32.le_u",
            .@"i32.lt_s",
            .@"i32.lt_u",
            .@"i32.ge_s",
            .@"i32.ge_u",
            .@"i32.gt_s",
            .@"i32.gt_u",
            .@"i32.xor",
            .@"i32.and",
            .@"i32.div_s",
            .@"i32.div_u",
            .@"i32.mul",
            .@"i32.or",
            .@"i32.rem_s",
            .@"i32.rem_u",
            .@"i32.rotl",
            .@"i32.rotr",
            .@"i32.shl",
            .@"i32.shr_s",
            .@"i32.shr_u",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I32 });
            },
            .@"i32.store" => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
            },
            .@"i32.clz", .@"i32.ctz", .@"i32.popcnt" => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I32 });
            },
            .@"i64.extend8_s", .@"i64.extend16_s", .@"i64.extend32_s" => {
                // no change
            },
            .@"i64.add",
            .@"i64.sub",
            .@"i64.xor",
            .@"i64.and",
            .@"i64.div_s",
            .@"i64.div_u",
            .@"i64.mul",
            .@"i64.or",
            .@"i64.rem_s",
            .@"i64.rem_u",
            .@"i64.rotl",
            .@"i64.rotr",
            .@"i64.shl",
            .@"i64.shr_s",
            .@"i64.shr_u",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I64 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I64 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I64 });
            },
            .@"i64.eq",
            .@"i64.ne",
            .@"i64.le_s",
            .@"i64.le_u",
            .@"i64.lt_s",
            .@"i64.lt_u",
            .@"i64.ge_s",
            .@"i64.ge_u",
            .@"i64.gt_s",
            .@"i64.gt_u",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I64 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I64 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I32 });
            },
            .@"i64.eqz" => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I64 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I32 });
            },
            .@"i64.clz", .@"i64.ctz", .@"i64.popcnt" => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I64 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I64 });
            },
            .@"i64.load" => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I64 });
            },
            .@"f32.add" => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F32 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F32 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .F32 });
            },
            .@"f64.add" => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F64 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F64 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .F64 });
            },
            .@"i32.const" => {
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I32 });
            },
            .@"i64.const" => {
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I64 });
            },
            .@"f32.const" => {
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .F32 });
            },
            .@"f64.const" => {
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
            _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = operands[len - i - 1] });
        }
    }

    pub fn pushControlFrame(v: *Validator, opcode: Opcode, in: []const ValueType, out: []const ValueType) !void {
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
        // TODO: control stack size check should maybe only be in debug builds
        if (v.ctrl_stack.items.len == 0) return error.ValidatorPopControlFrameControlStackEmpty;
        const frame = v.ctrl_stack.items[v.ctrl_stack.items.len - 1];
        try v.popOperands(frame.end_types);
        if (v.op_stack.items.len != frame.height) return error.ValidatorPopControlFrameMismatchedSizes;
        _ = v.ctrl_stack.pop();
        return frame;
    }

    fn labelTypes(v: *Validator, frame: ControlFrame) []const ValueType {
        if (frame.opcode == Opcode.loop) {
            return frame.start_types;
        } else {
            return frame.end_types;
        }
    }

    fn setUnreachable(v: *Validator) !void {
        // TODO: control stack size check should maybe only be in debug builds
        if (v.ctrl_stack.items.len == 0) return error.ValidatorPopControlFrameControlStackEmpty;
        const frame = &v.ctrl_stack.items[v.ctrl_stack.items.len - 1];
        v.op_stack.shrinkRetainingCapacity(frame.height);
        frame.unreachable_flag = true;
    }
};

fn isNum(value_type: ValueTypeUnknown) bool {
    // TODO: for version 1.1 value type may be ref type
    // so we'll have to update this:
    return true;
}

fn valueTypeEqual(v1: ValueTypeUnknown, v2: ValueTypeUnknown) bool {
    switch (v1) {
        .Known => |t1| {
            switch (v2) {
                .Known => |t2| return t1 == t2,
                .Unknown => return false,
            }
        },
        .Unknown => {
            switch (v2) {
                .Known => return false,
                .Unknown => return true,
            }
        },
    }
}

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
    opcode: Opcode = undefined,
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
    _ = try v.pushControlFrame(.block, in[0..], out[0..]);
    _ = try v.validate(.@"i32.const");
    _ = try v.validate(.drop);
    _ = try v.validate(.@"i32.const");
    _ = try v.validate(.@"i32.const");
    _ = try v.validate(.@"i32.add");
    _ = try v.validate(.end);
}

test "validate add i64" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var v = Validator.init(&arena.allocator);

    var in: [0]ValueType = [_]ValueType{} ** 0;
    var out: [1]ValueType = [_]ValueType{.I64} ** 1;
    _ = try v.pushControlFrame(.block, in[0..], out[0..]);
    _ = try v.validate(.@"i64.const");
    _ = try v.validate(.@"i64.const");
    _ = try v.validate(.@"i64.add");
    _ = try v.validate(.end);
}

test "validate add f32" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var v = Validator.init(&arena.allocator);

    var in: [0]ValueType = [_]ValueType{} ** 0;
    var out: [1]ValueType = [_]ValueType{.F32} ** 1;
    _ = try v.pushControlFrame(.block, in[0..], out[0..]);
    _ = try v.validate(.@"f32.const");
    _ = try v.validate(.@"f32.const");
    _ = try v.validate(.@"f32.add");
    _ = try v.validate(.end);
}

test "validate add f64" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var v = Validator.init(&arena.allocator);

    var in: [0]ValueType = [_]ValueType{} ** 0;
    var out: [1]ValueType = [_]ValueType{.F64} ** 1;
    _ = try v.pushControlFrame(.block, in[0..], out[0..]);
    _ = try v.validate(.@"f64.const");
    _ = try v.validate(.@"f64.const");
    _ = try v.validate(.@"f64.add");
    _ = try v.validate(.end);
}

test "validate: add error on mismatched types" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var v = Validator.init(&arena.allocator);

    var in: [0]ValueType = [_]ValueType{} ** 0;
    var out: [1]ValueType = [_]ValueType{.I32} ** 1;
    _ = try v.pushControlFrame(.block, in[0..], out[0..]);
    _ = try v.validate(.@"i64.const");
    _ = try v.validate(.@"i32.const");
    _ = v.validate(.@"i32.add") catch |err| {
        if (err == error.MismatchedTypes) return;
    };
    return error.ExpectedFailure;
}
