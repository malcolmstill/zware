const std = @import("std");
const mem = std.mem;
const ArrayList = std.ArrayList;
const ValueType = @import("module.zig").ValueType;
const Module = @import("module.zig").Module;
const Instruction = @import("instruction.zig").Instruction;
const instruction = @import("instruction.zig");

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
    op_stack_mem: []u64 = undefined,
    ctrl_stack_mem: []ControlFrame = undefined,
    ctrl_stack: []ControlFrame = undefined,
    label_stack_mem: []Label = undefined,
    label_stack: []Label = undefined,

    continuation: []const u8 = undefined,
    module: ?*Module = undefined,

    pub fn init(op_stack_mem: []u64, ctrl_stack_mem: []ControlFrame, label_stack_mem: []Label, module: ?*Module) Interpreter {
        return Interpreter{
            .op_stack_mem = op_stack_mem,
            .op_stack = op_stack_mem[0..0],
            .ctrl_stack_mem = ctrl_stack_mem,
            .ctrl_stack = ctrl_stack_mem[0..0],
            .label_stack_mem = label_stack_mem,
            .label_stack = label_stack_mem[0..0],
            .module = module,
        };
    }

    pub fn invoke(self: *Interpreter, code: []const u8) !void {
        self.continuation = code;
        while (self.continuation.len > 0) {
            const instr = self.continuation[0];
            const instr_code = self.continuation;
            self.continuation = self.continuation[1..];
            try self.interpret(@intToEnum(Instruction, instr), instr_code);
        }
    }

    pub fn interpret(self: *Interpreter, opcode: Instruction, code: []const u8) !void {
        // defer {
        //     std.debug.warn("stack after: {}\n", .{opcode});
        //     var i: usize = 0;
        //     while (i < self.op_stack.len) : (i += 1) {
        //         std.debug.warn("stack[{}] = {}\n", .{ i, self.op_stack[i] });
        //     }
        //     std.debug.warn("\n", .{});
        // }
        switch (opcode) {
            .Unreachable => return error.TrapUnreachable,
            .Nop => return,
            // .Block => {
            //     const meta = try instruction.findEnd(i.window);
            // },
            .Loop => {
                const block_type = try instruction.readULEB128Mem(i32, &self.continuation);

                // For loop control flow, the continuation is the loop body (including
                // the initiating loop instruction, as branch consumes the existing label)
                const continuation = code[0..];
                try self.pushLabel(Label{
                    .return_arity = if (block_type == 0x40) 0 else 1,
                    .op_stack_len = self.op_stack.len,
                    .continuation = continuation,
                });
            },
            .If => {
                // TODO: perform findEnd during parsing
                const block_type = try instruction.readULEB128Mem(i32, &self.continuation);
                const end = try instruction.findEnd(code);
                const else_branch = try instruction.findElse(code);

                // For if control flow, the continuation for our label
                // is the continuation of code after end
                const continuation = code[end.offset..];

                const condition = try self.popOperand(i32);
                if (condition == 0) {
                    // We'll skip to end
                    self.continuation = continuation;
                    // unless we have an else branch
                    if (else_branch) |eb| self.continuation = code[eb.offset..];
                }

                try self.pushLabel(Label{
                    .return_arity = if (block_type == 0x40) 0 else 1,
                    .op_stack_len = self.op_stack.len,
                    .continuation = continuation,
                });
            },
            .End => {
                // https://webassembly.github.io/spec/core/exec/instructions.html#exiting-xref-syntax-instructions-syntax-instr-mathit-instr-ast-with-label-l
                const label = try self.popLabel();
            },
            .BrIf => {
                const target = try instruction.readULEB128Mem(u32, &self.continuation);

                const condition = try self.popOperand(i32);
                if (condition == 0) return;

                try self.branch(target);
            },
            .Return => {
                const frame = try self.peekNthControlFrame(0);
                const n = frame.return_arity;

                if (std.builtin.mode == .Debug) {
                    if (self.op_stack.len < n) return error.OperandStackUnderflow;
                }

                const label = self.label_stack[frame.label_stack_len];
                self.continuation = label.continuation;

                // The mem copy is equivalent of popping n operands, doing everything
                // up to and including popControlFrame and then repushing the n operands
                var dst = self.op_stack[label.op_stack_len .. label.op_stack_len + n];
                const src = self.op_stack[self.op_stack.len - n ..];
                mem.copy(u64, dst, src);

                self.op_stack = self.op_stack[0 .. label.op_stack_len + n];
                self.label_stack = self.label_stack[0..frame.label_stack_len];

                _ = try self.popControlFrame();
            },
            .Call => {
                // The spec says:
                //      The call instruction invokes another function, consuming the necessary
                //      arguments from the stack and returning the result values of the call.

                // TODO: we need to verify that we're okay to lookup this function.
                //       we can (and probably should) do that at validation time.
                const module = self.module orelse return error.NoModule;
                const function_index = try instruction.readULEB128Mem(usize, &self.continuation);
                const func_type = module.types.items[function_index];
                const func = module.codes.items[function_index];
                const params = module.value_types.items[func_type.params_offset .. func_type.params_offset + func_type.params_count];
                const results = module.value_types.items[func_type.results_offset .. func_type.results_offset + func_type.results_count];

                // Consume parameters from the stack
                try self.pushControlFrame(Interpreter.ControlFrame{
                    .op_stack_len = self.op_stack.len - params.len,
                    .label_stack_len = self.label_stack.len,
                    .return_arity = results.len,
                }, func.locals_count + params.len);

                // Our continuation is the code after call
                try self.pushLabel(Interpreter.Label{
                    .return_arity = results.len,
                    .op_stack_len = self.op_stack.len - params.len,
                    .continuation = self.continuation,
                });

                // Make space for locals (again, params already on stack)
                var j: usize = 0;
                while (j < func.locals_count) {
                    try self.pushOperand(u64, 0);
                }

                self.continuation = func.code;
            },
            .Drop => _ = try self.popAnyOperand(),
            .LocalGet => {
                const frame = try self.peekNthControlFrame(0);
                const local_index = try instruction.readULEB128Mem(u32, &self.continuation);
                const local_value: u64 = frame.locals[local_index];
                try self.pushOperand(u64, local_value);
            },
            .LocalSet => {
                const value = try self.popAnyOperand();
                const frame = try self.peekNthControlFrame(0);
                const local_index = try instruction.readULEB128Mem(u32, &self.continuation);
                frame.locals[local_index] = value;
            },
            .I32Const => {
                const x = try instruction.readILEB128Mem(i32, &self.continuation);
                try self.pushOperand(i32, x);
            },
            .I32LtS => {
                const c2 = try self.popOperand(i32);
                const c1 = try self.popOperand(i32);
                // std.debug.warn("b < a = {} < {} = {}\n", .{ b, a, b < a });
                try self.pushOperand(i32, @as(i32, if (c1 < c2) 1 else 0));
            },
            .I32GeS => {
                const c2 = try self.popOperand(i32);
                const c1 = try self.popOperand(i32);
                // std.debug.warn("c1 >= c2 = {} >= {} = {}\n", .{ c1, c2, c1 >= c2 });
                try self.pushOperand(i32, @as(i32, if (c1 >= c2) 1 else 0));
            },
            .I32Add => {
                // TODO: does wasm wrap?
                const c2 = try self.popOperand(i32);
                const c1 = try self.popOperand(i32);
                // std.debug.warn("a + b = {} + {} = {}\n", .{ a, b, a + b });
                try self.pushOperand(i32, c1 + c2);
            },
            .I32Sub => {
                const c2 = try self.popOperand(i32);
                const c1 = try self.popOperand(i32);
                // std.debug.warn("b - a = {} - {} = {}\n", .{ b, a, b - a });
                try self.pushOperand(i32, c1 - c2);
            },
            .I32Mul => {
                const c2 = try self.popOperand(i32);
                const c1 = try self.popOperand(i32);
                try self.pushOperand(i32, c1 * c2);
            },
            .I64Add => {
                const c2 = try self.popOperand(i64);
                const c1 = try self.popOperand(i64);
                try self.pushOperand(i64, c1 + c2);
            },
            .F32Add => {
                const c2 = try self.popOperand(f32);
                const c1 = try self.popOperand(f32);
                try self.pushOperand(f32, c1 + c2);
            },
            .F64Add => {
                const c2 = try self.popOperand(f64);
                const c1 = try self.popOperand(f64);
                try self.pushOperand(f64, c1 + c2);
            },
            else => {
                std.debug.warn("unimplemented instruction: {}\n", .{opcode});
                unreachable;
            },
        }
    }

    // https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-br-l
    pub fn branch(self: *Interpreter, target: u32) !void {
        const label = try self.peekNthLabel(target);
        const n = label.return_arity;

        var dest = self.op_stack[label.op_stack_len .. label.op_stack_len + n];
        const src = self.op_stack[self.op_stack.len - n ..];

        mem.copy(u64, dest, src);

        self.op_stack = self.op_stack[0 .. label.op_stack_len + n];
        _ = try self.popLabels(target);

        self.continuation = label.continuation;
    }

    pub fn pushOperand(self: *Interpreter, comptime T: type, value: T) !void {
        if (self.op_stack.len == self.op_stack_mem.len) return error.OperandStackOverflow;

        self.op_stack = self.op_stack_mem[0 .. self.op_stack.len + 1];

        self.op_stack[self.op_stack.len - 1] = switch (T) {
            i32 => @bitCast(u64, @intCast(i64, value)),
            i64 => @bitCast(u64, value),
            f32 => @bitCast(u64, @floatCast(f64, value)),
            f64 => @bitCast(u64, value),
            u64 => value,
            else => |t| @compileError("Unsupported operand type: " ++ @typeName(t)),
        };
    }

    pub fn popOperand(self: *Interpreter, comptime T: type) !T {
        // TODO: if we've validated the wasm, do we need to perform this check:
        if (self.op_stack.len == 0) return error.OperandStackUnderflow;
        defer self.op_stack = self.op_stack[0 .. self.op_stack.len - 1];

        const value = self.op_stack[self.op_stack.len - 1];
        return switch (T) {
            i32 => @intCast(i32, @bitCast(i64, value)),
            i64 => @bitCast(i64, value),
            f32 => @floatCast(f32, @bitCast(f64, value)),
            f64 => @bitCast(f64, value),
            else => |t| @compileError("Unsupported operand type: " ++ @typeName(t)),
        };
    }

    pub fn popAnyOperand(self: *Interpreter) !u64 {
        if (self.op_stack.len == 0) return error.OperandStackUnderflow;
        defer self.op_stack = self.op_stack[0 .. self.op_stack.len - 1];

        return self.op_stack[self.op_stack.len - 1];
    }

    fn peekNthOperand(self: *Interpreter, index: u32) !u64 {
        if (index + 1 > self.op_stack.len) return error.OperandStackUnderflow;
        return self.op_stack[self.op_stack.len - index - 1];
    }

    // TODO: if the code is validated, do we need to know the params count
    //       i.e. can we get rid of the dependency on params so that we don't
    //       have to lookup a function (necessarily)
    pub fn pushControlFrame(self: *Interpreter, frame: ControlFrame, params_and_locals_count: usize) !void {
        if (self.ctrl_stack.len == self.ctrl_stack_mem.len) return error.ControlStackOverflow;
        self.ctrl_stack = self.ctrl_stack_mem[0 .. self.ctrl_stack.len + 1];

        const current_frame = &self.ctrl_stack[self.ctrl_stack.len - 1];
        current_frame.* = frame;
        // TODO: index out of bounds (error if we've run out of operand stack space):
        current_frame.locals = self.op_stack[frame.op_stack_len .. frame.op_stack_len + params_and_locals_count];
    }

    pub fn popControlFrame(self: *Interpreter) !ControlFrame {
        if (self.ctrl_stack.len == 0) return error.ControlStackUnderflow;
        defer self.ctrl_stack = self.ctrl_stack[0 .. self.ctrl_stack.len - 1];

        return self.ctrl_stack[self.ctrl_stack.len - 1];
    }

    // peekNthControlFrame
    //
    // Returns nth label on the Label stack relative to the top of the stack
    //
    fn peekNthControlFrame(self: *Interpreter, index: u32) !*ControlFrame {
        if (index + 1 > self.ctrl_stack.len) return error.ControlStackUnderflow;
        return &self.ctrl_stack[self.ctrl_stack.len - index - 1];
    }

    pub fn pushLabel(self: *Interpreter, label: Label) !void {
        if (self.label_stack.len == self.label_stack_mem.len) return error.LabelStackOverflow;
        self.label_stack = self.label_stack_mem[0 .. self.label_stack.len + 1];
        const current_label = try self.peekNthLabel(0);
        current_label.* = label;
    }

    pub fn popLabel(self: *Interpreter) !Label {
        if (self.label_stack.len == 0) return error.ControlStackUnderflow;
        defer self.label_stack = self.label_stack[0 .. self.label_stack.len - 1];

        return self.label_stack[self.label_stack.len - 1];
    }

    // peekNthLabel
    //
    // Returns nth label on the Label stack relative to the top of the stack
    //
    fn peekNthLabel(self: *Interpreter, index: u32) !*Label {
        if (index + 1 > self.label_stack.len) return error.LabelStackUnderflow;
        return &self.label_stack[self.label_stack.len - index - 1];
    }

    // popLabels
    //
    // target: branch target (relative to current scope which is 0)
    //
    // popLabels pops labels up to and including `target`. Returns the
    // the label at `target`.
    pub fn popLabels(self: *Interpreter, target: u32) !Label {
        if (target >= self.label_stack.len) return error.LabelStackUnderflow;
        const target_label = self.label_stack[self.label_stack.len - target - 1];

        self.label_stack = self.label_stack[0 .. self.label_stack.len - target - 1];

        // Return target_label so the caller can get the continuation
        return target_label;
    }

    pub const ControlFrame = struct {
        locals: []u64 = undefined, // TODO: we're in trouble if we move our stacks in memory
        return_arity: usize = 0,
        op_stack_len: usize,
        label_stack_len: usize,
    };

    // Label
    //
    // - code: the code we should interpret after `end`
    pub const Label = struct {
        return_arity: usize = 0,
        continuation: []const u8 = undefined,
        op_stack_len: usize, // u32?
    };
};

const testing = std.testing;

test "operand push / pop test" {
    var op_stack: [6]u64 = [_]u64{0} ** 6;
    var ctrl_stack_mem: [1024]Interpreter.ControlFrame = [_]Interpreter.ControlFrame{undefined} ** 1024;
    var label_stack_mem: [1024]Interpreter.Label = [_]Interpreter.Label{undefined} ** 1024;

    var i = Interpreter.init(op_stack[0..], ctrl_stack_mem[0..], label_stack_mem[0..], null);

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
    var label_stack_mem: [1024]Interpreter.Label = [_]Interpreter.Label{undefined} ** 1024;
    var i = Interpreter.init(op_stack[0..], ctrl_stack[0..], label_stack_mem[0..], null);

    try i.pushOperand(i32, 22);
    try i.pushOperand(i32, -23);

    const code: [0]u8 = [_]u8{0} ** 0;

    try i.interpret(.I32Add, code[0..]);

    testing.expectEqual(@as(i32, -1), try i.popOperand(i32));
}
