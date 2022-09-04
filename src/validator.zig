const std = @import("std");
const mem = std.mem;
const LinearFifo = std.fifo.LinearFifo;
const ArrayList = std.ArrayList;
const FuncType = @import("common.zig").FuncType;
const ValueType = @import("common.zig").ValueType;
const Global = @import("common.zig").Global;
const Opcode = @import("opcode.zig").Opcode;

pub const Validator = struct {
    op_stack: OperandStack = undefined,
    ctrl_stack: ControlStack = undefined,
    max_depth: usize = 0,

    pub fn init(alloc: mem.Allocator) Validator {
        return Validator{
            .op_stack = OperandStack.init(alloc),
            .ctrl_stack = ControlStack.init(alloc),
        };
    }

    // TODO: deinit?

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
        if (label >= v.ctrl_stack.items.len) return error.ValidateBrInvalidLabel;
        const frame = v.ctrl_stack.items[v.ctrl_stack.items.len - 1 - label];
        try v.popOperands(labelTypes(frame));
        try v.setUnreachable();
    }

    pub fn validateBrIf(v: *Validator, label: usize) !void {
        if (label >= v.ctrl_stack.items.len) return error.ValidateBrIfInvalidLabel;
        const frame = v.ctrl_stack.items[v.ctrl_stack.items.len - 1 - label];

        _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
        try v.popOperands(labelTypes(frame));
        try v.pushOperands(labelTypes(frame));
    }

    pub fn validateBrTable(v: *Validator, n_star: []u32, label: u32) !void {
        _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
        if (label >= v.ctrl_stack.items.len) return error.ValidateBrTableInvalidLabel;
        const frame = v.ctrl_stack.items[v.ctrl_stack.items.len - 1 - label];

        const arity = labelTypes(frame).len;

        for (n_star) |n| {
            if (n >= v.ctrl_stack.items.len) return error.ValidateBrTableInvalidLabelN;
            const frame_n = v.ctrl_stack.items[v.ctrl_stack.items.len - 1 - n];
            if (labelTypes(frame_n).len != arity) return error.ValidateBrTableInvalidLabelWrongArity;

            if (!(arity <= 64)) return error.TODOAllocation;

            var temp = [_]ValueTypeUnknown{.{ .Known = .I32 }} ** 64; // TODO: allocate some memory for this
            for (labelTypes(frame_n)) |_, i| {
                temp[i] = try v.popOperandExpecting(ValueTypeUnknown{ .Known = labelTypes(frame_n)[arity - i - 1] });
            }

            for (labelTypes(frame_n)) |_, i| {
                try v.pushOperand(temp[arity - 1 - i]);
            }
        }

        try v.popOperands(labelTypes(frame));
        try v.setUnreachable();
    }

    pub fn validateCall(v: *Validator, func_type: FuncType) !void {
        try v.popOperands(func_type.params);
        try v.pushOperands(func_type.results);
    }

    pub fn validateCallIndirect(v: *Validator, func_type: FuncType) !void {
        _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
        try v.popOperands(func_type.params);
        try v.pushOperands(func_type.results);
    }

    pub fn validateLocalGet(v: *Validator, local_type: ValueType) !void {
        try v.pushOperand(ValueTypeUnknown{ .Known = local_type });
    }

    pub fn validateLocalSet(v: *Validator, local_type: ValueType) !void {
        _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = local_type });
    }

    pub fn validateLocalTee(v: *Validator, local_type: ValueType) !void {
        const t = try v.popOperandExpecting(ValueTypeUnknown{ .Known = local_type });
        try v.pushOperand(t);
    }

    pub fn validateGlobalGet(v: *Validator, global: Global) !void {
        try v.pushOperand(ValueTypeUnknown{ .Known = global.value_type });
    }

    pub fn validateGlobalSet(v: *Validator, global: Global) !void {
        if (global.mutability == .Immutable) return error.ValidatorAttemptToMutateImmutableGlobal;
        _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = global.value_type });
    }

    pub fn validateTrunc(v: *Validator, trunc_type: u32) !void {
        switch (trunc_type) {
            0, 1 => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F32 });
                try v.pushOperand(ValueTypeUnknown{ .Known = .I32 });
            },
            2, 3 => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F64 });
                try v.pushOperand(ValueTypeUnknown{ .Known = .I32 });
            },
            4, 5 => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F32 });
                try v.pushOperand(ValueTypeUnknown{ .Known = .I64 });
            },
            6, 7 => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F64 });
                try v.pushOperand(ValueTypeUnknown{ .Known = .I64 });
            },
            else => return error.UnknownTruncType,
        }
    }

    pub fn validate(v: *Validator, opcode: Opcode) !void {
        switch (opcode) {
            .block,
            .loop,
            .@"if",
            .br,
            .br_if,
            .br_table,
            .call,
            .call_indirect,
            .@"global.get",
            .@"global.set",
            .@"local.get",
            .@"local.set",
            .@"local.tee",
            .misc,
            => {
                // These instructions are handle separately
                unreachable;
            },
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
                const frame = v.ctrl_stack.items[0];
                try v.popOperands(labelTypes(frame));
                try v.setUnreachable();
            },
            .drop => {
                _ = try v.popOperand();
            },
            .select => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                const t1 = try v.popOperand();
                const t2 = try v.popOperand();

                if (!(isNum(t1) and isNum(t2))) return error.ExpectingBothNum;
                if (!valueTypeEqual(t1, t2) and !valueTypeEqual(t1, ValueTypeUnknown.Unknown) and !valueTypeEqual(t2, ValueTypeUnknown.Unknown)) return error.ValidatorSelect;
                if (valueTypeEqual(t1, ValueTypeUnknown.Unknown)) {
                    try v.pushOperand(t2);
                } else {
                    try v.pushOperand(t1);
                }
            },
            .@"memory.size" => {
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I32 });
            },
            .@"i32.extend8_s",
            .@"i32.extend16_s",
            .@"i32.eqz",
            .@"memory.grow",
            .@"i32.load",
            .@"i32.load8_u",
            .@"i32.load8_s",
            .@"i32.load16_u",
            .@"i32.load16_s",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I32 });
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
            .@"i32.store",
            .@"i32.store8",
            .@"i32.store16",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
            },
            .@"i32.clz", .@"i32.ctz", .@"i32.popcnt" => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I32 });
            },
            .@"i32.wrap_i64" => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I64 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I32 });
            },
            .@"i32.reinterpret_f32",
            .@"i32.trunc_f32_s",
            .@"i32.trunc_f32_u",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F32 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I32 });
            },
            .@"i32.trunc_f64_s",
            .@"i32.trunc_f64_u",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F64 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I32 });
            },
            .@"i64.extend8_s",
            .@"i64.extend16_s",
            .@"i64.extend32_s",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I64 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I64 });
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
            .@"i64.store",
            .@"i64.store8",
            .@"i64.store16",
            .@"i64.store32",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I64 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
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
            .@"i64.load",
            .@"i64.load8_s",
            .@"i64.load8_u",
            .@"i64.load16_s",
            .@"i64.load16_u",
            .@"i64.load32_s",
            .@"i64.load32_u",
            .@"i64.extend_i32_s",
            .@"i64.extend_i32_u",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I64 });
            },
            .@"i64.trunc_f32_s",
            .@"i64.trunc_f32_u",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F32 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I64 });
            },
            .@"i64.reinterpret_f64",
            .@"i64.trunc_f64_s",
            .@"i64.trunc_f64_u",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F64 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I64 });
            },
            .@"f32.add",
            .@"f32.sub",
            .@"f32.mul",
            .@"f32.div",
            .@"f32.min",
            .@"f32.max",
            .@"f32.copysign",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F32 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F32 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .F32 });
            },
            .@"f32.abs",
            .@"f32.neg",
            .@"f32.sqrt",
            .@"f32.ceil",
            .@"f32.floor",
            .@"f32.trunc",
            .@"f32.nearest",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F32 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .F32 });
            },
            .@"f32.eq",
            .@"f32.ne",
            .@"f32.lt",
            .@"f32.le",
            .@"f32.gt",
            .@"f32.ge",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F32 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F32 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I32 });
            },
            .@"f32.store" => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F32 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
            },
            .@"f32.load",
            .@"f32.convert_i32_s",
            .@"f32.convert_i32_u",
            .@"f32.reinterpret_i32",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .F32 });
            },
            .@"f32.demote_f64" => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F64 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .F32 });
            },
            .@"f32.convert_i64_s",
            .@"f32.convert_i64_u",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I64 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .F32 });
            },
            .@"f64.add",
            .@"f64.sub",
            .@"f64.mul",
            .@"f64.div",
            .@"f64.min",
            .@"f64.max",
            .@"f64.copysign",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F64 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F64 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .F64 });
            },
            .@"f64.abs",
            .@"f64.neg",
            .@"f64.sqrt",
            .@"f64.ceil",
            .@"f64.floor",
            .@"f64.trunc",
            .@"f64.nearest",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F64 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .F64 });
            },
            .@"f64.eq",
            .@"f64.ne",
            .@"f64.lt",
            .@"f64.le",
            .@"f64.gt",
            .@"f64.ge",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F64 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F64 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .I32 });
            },
            .@"f64.store" => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F64 });
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
            },
            .@"f64.load",
            .@"f64.convert_i32_s",
            .@"f64.convert_i32_u",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .F64 });
            },
            .@"f64.reinterpret_i64",
            .@"f64.convert_i64_s",
            .@"f64.convert_i64_u",
            => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .I64 });
                _ = try v.pushOperand(ValueTypeUnknown{ .Known = .F64 });
            },
            .@"f64.promote_f32" => {
                _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = .F32 });
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
        }
    }

    fn pushOperand(v: *Validator, t: ValueTypeUnknown) !void {
        defer v.trackMaxDepth();
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
        for (operands) |_, i| {
            _ = try v.popOperandExpecting(ValueTypeUnknown{ .Known = operands[len - i - 1] });
        }
    }

    fn trackMaxDepth(v: *Validator) void {
        if (v.op_stack.items.len > v.max_depth) {
            v.max_depth = v.op_stack.items.len;
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

    fn labelTypes(frame: ControlFrame) []const ValueType {
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

fn isNum(_: ValueTypeUnknown) bool {
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
    var v = Validator.init(arena.allocator());

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
    var v = Validator.init(arena.allocator());

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
    var v = Validator.init(arena.allocator());

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
    var v = Validator.init(arena.allocator());

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
    var v = Validator.init(arena.allocator());

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
