const std = @import("std");
const mem = std.mem;
const LinearFifo = std.fifo.LinearFifo;
const ArrayList = std.ArrayList;
const FuncType = @import("../module.zig").FuncType;
const ValType = @import("../valtype.zig").ValType;
const RefType = @import("../valtype.zig").RefType;
const GlobalType = @import("../module.zig").GlobalType;
const Opcode = @import("../opcode.zig").Opcode;
const MiscOpcode = @import("../opcode.zig").MiscOpcode;
const Module = @import("../module.zig").Module;

pub const Validator = struct {
    op_stack: OperandStack = undefined,
    ctrl_stack: ControlStack = undefined,
    max_depth: usize = 0,
    is_constant: bool,

    pub fn init(alloc: mem.Allocator, is_constant: bool) Validator {
        return Validator{
            .op_stack = OperandStack.init(alloc),
            .ctrl_stack = ControlStack.init(alloc),
            .is_constant = is_constant,
        };
    }

    pub fn deinit(self: *Validator) void {
        // Static allocation?
        self.op_stack.deinit();
        self.ctrl_stack.deinit();
    }

    pub fn validateBlock(v: *Validator, in_operands: []const ValType, out_operands: []const ValType) !void {
        try v.popOperands(in_operands);
        try v.pushControlFrame(.block, in_operands, out_operands);
    }

    pub fn validateLoop(v: *Validator, in_operands: []const ValType, out_operands: []const ValType) !void {
        try v.popOperands(in_operands);
        try v.pushControlFrame(.loop, in_operands, out_operands);
    }

    pub fn validateSelectT(v: *Validator, valuetype: ValType) !void {
        _ = try v.popOperandExpecting(Type{ .Known = .I32 });
        _ = try v.popOperandExpecting(Type{ .Known = valuetype });
        _ = try v.popOperandExpecting(Type{ .Known = valuetype });
        try v.pushOperand(Type{ .Known = valuetype });
    }

    pub fn validateIf(v: *Validator, in_operands: []const ValType, out_operands: []const ValType) !void {
        _ = try v.popOperandExpecting(Type{ .Known = .I32 });
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

        _ = try v.popOperandExpecting(Type{ .Known = .I32 });
        try v.popOperands(labelTypes(frame));
        try v.pushOperands(labelTypes(frame));
    }

    pub fn validateBrTable(v: *Validator, n_star: []u32, label: u32) !void {
        _ = try v.popOperandExpecting(Type{ .Known = .I32 });
        if (label >= v.ctrl_stack.items.len) return error.ValidateBrTableInvalidLabel;
        const frame = v.ctrl_stack.items[v.ctrl_stack.items.len - 1 - label];

        const arity = labelTypes(frame).len;

        for (n_star) |n| {
            if (n >= v.ctrl_stack.items.len) return error.ValidateBrTableInvalidLabelN;
            const frame_n = v.ctrl_stack.items[v.ctrl_stack.items.len - 1 - n];
            if (labelTypes(frame_n).len != arity) return error.ValidateBrTableInvalidLabelWrongArity;

            if (!(arity <= 64)) return error.TODOAllocation;

            var temp = [_]Type{.{ .Known = .I32 }} ** 64; // TODO: allocate some memory for this
            for (labelTypes(frame_n), 0..) |_, i| {
                temp[i] = try v.popOperandExpecting(Type{ .Known = labelTypes(frame_n)[arity - i - 1] });
            }

            for (labelTypes(frame_n), 0..) |_, i| {
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
        _ = try v.popOperandExpecting(Type{ .Known = .I32 });
        try v.popOperands(func_type.params);
        try v.pushOperands(func_type.results);
    }

    pub fn validateLocalGet(v: *Validator, local_type: ValType) !void {
        try v.pushOperand(Type{ .Known = local_type });
    }

    pub fn validateLocalSet(v: *Validator, local_type: ValType) !void {
        _ = try v.popOperandExpecting(Type{ .Known = local_type });
    }

    pub fn validateLocalTee(v: *Validator, local_type: ValType) !void {
        const t = try v.popOperandExpecting(Type{ .Known = local_type });
        try v.pushOperand(t);
    }

    pub fn validateGlobalGet(v: *Validator, globaltype: GlobalType) !void {
        if (v.is_constant and globaltype.mutability == .Mutable) return error.ValidatorMutableGlobalInConstantExpr;
        try v.pushOperand(Type{ .Known = globaltype.valtype });
    }

    pub fn validateGlobalSet(v: *Validator, globaltype: GlobalType) !void {
        if (globaltype.mutability == .Immutable) return error.ValidatorAttemptToMutateImmutableGlobal;
        _ = try v.popOperandExpecting(Type{ .Known = globaltype.valtype });
    }

    pub fn validateRefNull(v: *Validator, reftype: RefType) !void {
        switch (reftype) {
            .FuncRef => _ = try v.pushOperand(Type{ .Known = .FuncRef }),
            .ExternRef => _ = try v.pushOperand(Type{ .Known = .ExternRef }),
        }
    }

    pub fn validateMisc(v: *Validator, misc_type: MiscOpcode) !void {
        switch (misc_type) {
            .@"i32.trunc_sat_f32_s",
            .@"i32.trunc_sat_f32_u",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .F32 });
                try v.pushOperand(Type{ .Known = .I32 });
            },
            .@"i32.trunc_sat_f64_s",
            .@"i32.trunc_sat_f64_u",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .F64 });
                try v.pushOperand(Type{ .Known = .I32 });
            },
            .@"i64.trunc_sat_f32_s",
            .@"i64.trunc_sat_f32_u",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .F32 });
                try v.pushOperand(Type{ .Known = .I64 });
            },
            .@"i64.trunc_sat_f64_s",
            .@"i64.trunc_sat_f64_u",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .F64 });
                try v.pushOperand(Type{ .Known = .I64 });
            },
            .@"memory.init" => {
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
            },
            .@"data.drop" => {
                //
            },
            .@"memory.copy" => {
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
            },
            .@"memory.fill" => {
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
            },
            .@"table.init" => {
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
            },
            .@"elem.drop" => {
                //
            },
            .@"table.copy" => {
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
            },
            .@"table.grow" => {},
            .@"table.size" => {
                try v.pushOperand(Type{ .Known = .I32 });
            },
            .@"table.fill" => {},
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
            .@"ref.null",
            .misc,
            .select_t,
            .@"table.get",
            .@"table.set",
            => {},
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
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                const t1 = try v.popOperand();
                const t2 = try v.popOperand();

                if (!(isNum(t1) and isNum(t2))) return error.ExpectingBothNum;
                if (!typeEqual(t1, t2) and !typeEqual(t1, Type.Unknown) and !typeEqual(t2, Type.Unknown)) return error.ValidatorSelect;
                if (typeEqual(t1, Type.Unknown)) {
                    try v.pushOperand(t2);
                } else {
                    try v.pushOperand(t1);
                }
            },
            .@"memory.size" => {
                _ = try v.pushOperand(Type{ .Known = .I32 });
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
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.pushOperand(Type{ .Known = .I32 });
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
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.pushOperand(Type{ .Known = .I32 });
            },
            .@"i32.store",
            .@"i32.store8",
            .@"i32.store16",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
            },
            .@"i32.clz", .@"i32.ctz", .@"i32.popcnt" => {
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.pushOperand(Type{ .Known = .I32 });
            },
            .@"i32.wrap_i64" => {
                _ = try v.popOperandExpecting(Type{ .Known = .I64 });
                _ = try v.pushOperand(Type{ .Known = .I32 });
            },
            .@"i32.reinterpret_f32",
            .@"i32.trunc_f32_s",
            .@"i32.trunc_f32_u",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .F32 });
                _ = try v.pushOperand(Type{ .Known = .I32 });
            },
            .@"i32.trunc_f64_s",
            .@"i32.trunc_f64_u",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .F64 });
                _ = try v.pushOperand(Type{ .Known = .I32 });
            },
            .@"i64.extend8_s",
            .@"i64.extend16_s",
            .@"i64.extend32_s",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .I64 });
                _ = try v.pushOperand(Type{ .Known = .I64 });
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
                _ = try v.popOperandExpecting(Type{ .Known = .I64 });
                _ = try v.popOperandExpecting(Type{ .Known = .I64 });
                _ = try v.pushOperand(Type{ .Known = .I64 });
            },
            .@"i64.store",
            .@"i64.store8",
            .@"i64.store16",
            .@"i64.store32",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .I64 });
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
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
                _ = try v.popOperandExpecting(Type{ .Known = .I64 });
                _ = try v.popOperandExpecting(Type{ .Known = .I64 });
                _ = try v.pushOperand(Type{ .Known = .I32 });
            },
            .@"i64.eqz" => {
                _ = try v.popOperandExpecting(Type{ .Known = .I64 });
                _ = try v.pushOperand(Type{ .Known = .I32 });
            },
            .@"i64.clz", .@"i64.ctz", .@"i64.popcnt" => {
                _ = try v.popOperandExpecting(Type{ .Known = .I64 });
                _ = try v.pushOperand(Type{ .Known = .I64 });
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
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.pushOperand(Type{ .Known = .I64 });
            },
            .@"i64.trunc_f32_s",
            .@"i64.trunc_f32_u",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .F32 });
                _ = try v.pushOperand(Type{ .Known = .I64 });
            },
            .@"i64.reinterpret_f64",
            .@"i64.trunc_f64_s",
            .@"i64.trunc_f64_u",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .F64 });
                _ = try v.pushOperand(Type{ .Known = .I64 });
            },
            .@"f32.add",
            .@"f32.sub",
            .@"f32.mul",
            .@"f32.div",
            .@"f32.min",
            .@"f32.max",
            .@"f32.copysign",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .F32 });
                _ = try v.popOperandExpecting(Type{ .Known = .F32 });
                _ = try v.pushOperand(Type{ .Known = .F32 });
            },
            .@"f32.abs",
            .@"f32.neg",
            .@"f32.sqrt",
            .@"f32.ceil",
            .@"f32.floor",
            .@"f32.trunc",
            .@"f32.nearest",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .F32 });
                _ = try v.pushOperand(Type{ .Known = .F32 });
            },
            .@"f32.eq",
            .@"f32.ne",
            .@"f32.lt",
            .@"f32.le",
            .@"f32.gt",
            .@"f32.ge",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .F32 });
                _ = try v.popOperandExpecting(Type{ .Known = .F32 });
                _ = try v.pushOperand(Type{ .Known = .I32 });
            },
            .@"f32.store" => {
                _ = try v.popOperandExpecting(Type{ .Known = .F32 });
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
            },
            .@"f32.load",
            .@"f32.convert_i32_s",
            .@"f32.convert_i32_u",
            .@"f32.reinterpret_i32",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.pushOperand(Type{ .Known = .F32 });
            },
            .@"f32.demote_f64" => {
                _ = try v.popOperandExpecting(Type{ .Known = .F64 });
                _ = try v.pushOperand(Type{ .Known = .F32 });
            },
            .@"f32.convert_i64_s",
            .@"f32.convert_i64_u",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .I64 });
                _ = try v.pushOperand(Type{ .Known = .F32 });
            },
            .@"f64.add",
            .@"f64.sub",
            .@"f64.mul",
            .@"f64.div",
            .@"f64.min",
            .@"f64.max",
            .@"f64.copysign",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .F64 });
                _ = try v.popOperandExpecting(Type{ .Known = .F64 });
                _ = try v.pushOperand(Type{ .Known = .F64 });
            },
            .@"f64.abs",
            .@"f64.neg",
            .@"f64.sqrt",
            .@"f64.ceil",
            .@"f64.floor",
            .@"f64.trunc",
            .@"f64.nearest",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .F64 });
                _ = try v.pushOperand(Type{ .Known = .F64 });
            },
            .@"f64.eq",
            .@"f64.ne",
            .@"f64.lt",
            .@"f64.le",
            .@"f64.gt",
            .@"f64.ge",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .F64 });
                _ = try v.popOperandExpecting(Type{ .Known = .F64 });
                _ = try v.pushOperand(Type{ .Known = .I32 });
            },
            .@"f64.store" => {
                _ = try v.popOperandExpecting(Type{ .Known = .F64 });
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
            },
            .@"f64.load",
            .@"f64.convert_i32_s",
            .@"f64.convert_i32_u",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .I32 });
                _ = try v.pushOperand(Type{ .Known = .F64 });
            },
            .@"f64.reinterpret_i64",
            .@"f64.convert_i64_s",
            .@"f64.convert_i64_u",
            => {
                _ = try v.popOperandExpecting(Type{ .Known = .I64 });
                _ = try v.pushOperand(Type{ .Known = .F64 });
            },
            .@"f64.promote_f32" => {
                _ = try v.popOperandExpecting(Type{ .Known = .F32 });
                _ = try v.pushOperand(Type{ .Known = .F64 });
            },
            .@"i32.const" => {
                _ = try v.pushOperand(Type{ .Known = .I32 });
            },
            .@"i64.const" => {
                _ = try v.pushOperand(Type{ .Known = .I64 });
            },
            .@"f32.const" => {
                _ = try v.pushOperand(Type{ .Known = .F32 });
            },
            .@"f64.const" => {
                _ = try v.pushOperand(Type{ .Known = .F64 });
            },
            .@"ref.is_null" => {
                _ = try v.popOperandExpecting(Type.Unknown); // Is this right? Do we need UnknownRefType + UnknownValType
                _ = try v.pushOperand(Type{ .Known = .I32 });
            },
            .@"ref.func" => {
                _ = try v.pushOperand(Type{ .Known = .FuncRef });
            },
        }
    }

    pub fn pushOperand(v: *Validator, t: Type) !void {
        defer v.trackMaxDepth();
        try v.op_stack.append(t);
    }

    fn popOperand(v: *Validator) !Type {
        if (v.ctrl_stack.items.len == 0) return error.ControlStackEmpty;
        const ctrl_frame = v.ctrl_stack.items[v.ctrl_stack.items.len - 1];
        if (v.op_stack.items.len == ctrl_frame.height and ctrl_frame.unreachable_flag) {
            return Type.Unknown;
        }
        if (v.op_stack.items.len == ctrl_frame.height) return error.ValidatorPopOperandError;
        return v.op_stack.pop();
    }

    pub fn popOperandExpecting(v: *Validator, expected: Type) !Type {
        const actual = try v.popOperand();

        if (!typeEqual(actual, expected) and !typeEqual(actual, Type.Unknown) and !typeEqual(expected, Type.Unknown)) {
            return error.MismatchedTypes;
        } else {
            return actual;
        }
    }

    fn pushOperands(v: *Validator, operands: []const ValType) !void {
        for (operands) |op| {
            try v.pushOperand(Type{ .Known = op });
        }
    }

    fn popOperands(v: *Validator, operands: []const ValType) !void {
        const len = operands.len;
        for (operands, 0..) |_, i| {
            _ = try v.popOperandExpecting(Type{ .Known = operands[len - i - 1] });
        }
    }

    fn trackMaxDepth(v: *Validator) void {
        if (v.op_stack.items.len > v.max_depth) {
            v.max_depth = v.op_stack.items.len;
        }
    }

    pub fn pushControlFrame(v: *Validator, opcode: Opcode, in: []const ValType, out: []const ValType) !void {
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
        const frame = v.ctrl_stack.items[v.ctrl_stack.items.len - 1];
        try v.popOperands(frame.end_types);
        if (v.op_stack.items.len != frame.height) return error.ValidatorPopControlFrameMismatchedSizes;
        _ = v.ctrl_stack.pop();
        return frame;
    }

    fn labelTypes(frame: ControlFrame) []const ValType {
        if (frame.opcode == Opcode.loop) {
            return frame.start_types;
        } else {
            return frame.end_types;
        }
    }

    fn setUnreachable(v: *Validator) !void {
        if (v.ctrl_stack.items.len == 0) return error.ValidatorPopControlFrameControlStackEmpty;
        const frame = &v.ctrl_stack.items[v.ctrl_stack.items.len - 1];
        v.op_stack.shrinkRetainingCapacity(frame.height);
        frame.unreachable_flag = true;
    }
};

fn isNum(valuetype: Type) bool {
    return switch (valuetype) {
        .Unknown => true,
        .Known => |k| switch (k) {
            .I32, .I64, .F32, .F64 => true,
            else => false,
        },
    };
}

fn typeEqual(v1: Type, v2: Type) bool {
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

const TypeTag = enum {
    Known,
    Unknown,
};

pub const Type = union(TypeTag) {
    Known: ValType,
    Unknown: void,
};

const OperandStack = ArrayList(Type);
const ControlStack = ArrayList(ControlFrame);

const ControlFrame = struct {
    opcode: Opcode = undefined,
    start_types: []const ValType,
    end_types: []const ValType,
    height: usize = 0,
    unreachable_flag: bool = false,
};

const testing = std.testing;
test "validate add i32" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    var arena = ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    var v = Validator.init(arena.allocator(), false);
    defer v.deinit();

    var in: [0]ValType = [_]ValType{} ** 0;
    var out: [1]ValType = [_]ValType{.I32} ** 1;
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
    var v = Validator.init(arena.allocator(), false);
    defer v.deinit();

    var in: [0]ValType = [_]ValType{} ** 0;
    var out: [1]ValType = [_]ValType{.I64} ** 1;
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
    var v = Validator.init(arena.allocator(), false);
    defer v.deinit();

    var in: [0]ValType = [_]ValType{} ** 0;
    var out: [1]ValType = [_]ValType{.F32} ** 1;
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
    var v = Validator.init(arena.allocator(), false);
    defer v.deinit();

    var in: [0]ValType = [_]ValType{} ** 0;
    var out: [1]ValType = [_]ValType{.F64} ** 1;
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
    var v = Validator.init(arena.allocator(), false);
    defer v.deinit();

    var in: [0]ValType = [_]ValType{} ** 0;
    var out: [1]ValType = [_]ValType{.I32} ** 1;
    _ = try v.pushControlFrame(.block, in[0..], out[0..]);
    _ = try v.validate(.@"i64.const");
    _ = try v.validate(.@"i32.const");
    _ = v.validate(.@"i32.add") catch |err| {
        if (err == error.MismatchedTypes) return;
    };
    return error.ExpectedFailure;
}
