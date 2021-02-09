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
    op_stack_size: usize = 0,
    ctrl_stack_mem: []ControlFrame = undefined,
    ctrl_stack: []ControlFrame = undefined,
    label_stack_mem: []Label = undefined,
    label_stack: []Label = undefined,
    function_code: []const u8 = undefined,
    window: []const u8 = undefined,
    module: ?*Module = undefined,

    pub fn init(op_stack: []u64, ctrl_stack: []ControlFrame, label_stack_mem: []Label, module: ?*Module) Interpreter {
        return Interpreter{
            .op_stack = op_stack,
            .ctrl_stack_mem = ctrl_stack,
            .ctrl_stack = ctrl_stack[0..0],
            .label_stack_mem = label_stack_mem,
            .label_stack = label_stack_mem[0..0],
            .module = module,
        };
    }

    pub fn interpretFunction(i: *Interpreter, code: []const u8) !void {
        errdefer {
            for (i.op_stack) |op, j| {
                // std.debug.warn("stack[{}]: {}\n", .{ j, op });
            }
        }
        i.function_code = code;
        i.window = code;
        while (i.window.len > 0) {
            const instr = i.window[0];
            const instr_code = i.window;
            i.window = i.window[1..];
            try i.interpret(@intToEnum(Instruction, instr), instr_code);
        }
    }

    pub fn interpret(i: *Interpreter, opcode: Instruction, code: []const u8) !void {
        // defer {
        //     std.debug.warn("stack after: {}\n", .{opcode});
        //     var j: usize = 0;
        //     while (j < i.op_stack_size) : (j += 1) {
        //         std.debug.warn("stack[{}] = {}\n", .{ j, i.op_stack[j] });
        //     }
        //     std.debug.warn("\n", .{});
        // }
        switch (opcode) {
            .Unreachable => return error.TrapUnreachable,
            .Nop => return,
            // .Block => {
            //     const meta = try instruction.findEnd(i.window);
            // },
            // .Loop => {
            //     // 1. We need to push a new label onto our label stack.
            //     //    The continuation for a loop is the the loop body.
            //     //    Or is it the loop body + the rest of the function?
            // },
            .If => {
                // TODO: perform findEnd during parsing
                const block_type = try instruction.readILEB128Mem(i32, &i.window);

                const x = try i.popOperand(i32);
                const end = try instruction.findEnd(code);
                const continuation = code[end.offset..];

                if (x == 0) {
                    i.window = continuation;
                } else {
                    try i.pushLabel(Label{
                        .op_stack_start = i.op_stack_size,
                        .code = continuation,
                    });
                }
            },
            .End => {
                std.debug.warn("END\n", .{});
                const label = try i.peekNthLabel(0);
                i.window = label.code;
            },
            .Return => {
                // Pop labels and control frame
                // Pop all labels that have their stack position after control
                // frames stack position
                const frame = try i.peekNthControlFrame(0);
                var l: usize = i.label_stack.len;
                while (l > 0) : (l -= 1) {
                    const label = i.label_stack[l - 1];
                    if (label.op_stack_start >= frame.locals_start) {
                        _ = try i.popLabel();

                        if (label.op_stack_start == frame.locals_start) {
                            i.window = label.code;
                        }
                    }
                }

                if (frame.return_arity == 1) {
                    const value = try i.popAnyOperand();
                    i.op_stack_size = frame.locals_start;
                    _ = try i.popControlFrame();
                    try i.pushOperand(u64, value);
                    std.debug.warn("return: {}\n", .{value});
                } else {
                    i.op_stack_size = frame.locals_start;
                    _ = try i.popControlFrame();
                    std.debug.warn("return (none)\n", .{});
                }
            },
            .Call => {
                // TODO: we need to verify that we're okay to lookup this function.
                //       we can (and probably should) do that at validation time.
                const module = i.module orelse return error.NoModule;
                const function_index = try instruction.readULEB128Mem(usize, &i.window);
                const func_type = module.types.items[function_index];
                const func = module.codes.items[function_index];
                const params = module.value_types.items[func_type.params_offset .. func_type.params_offset + func_type.params_count];
                const results = module.value_types.items[func_type.results_offset .. func_type.results_offset + func_type.results_count];

                // Assume params are already on stack
                try i.pushControlFrame(Interpreter.ControlFrame{
                    .locals_start = i.op_stack_size - params.len,
                    .return_arity = results.len,
                }, func.locals_count + params.len);

                // Our continuation is the code after call
                try i.pushLabel(Interpreter.Label{
                    .op_stack_start = i.op_stack_size - params.len,
                    .code = i.window,
                });

                // call takes parameters from what's on the stack so we don't
                // need to pop and then repush
                // inline for (args) |arg, j| {
                //    try i.pushOperand(@TypeOf(arg), arg);
                // }

                // but we do need to make space for new locals
                var j: usize = 0;
                while (j < func.locals_count) {
                    try i.pushOperand(u64, 0);
                }

                // TODO: not sure we want to call this recursively. Could we replace i.window?
                // THOUGHT: the label is exactly what lets us do this without recursion
                // try i.interpretFunction(func.code);
                i.window = func.code;

                // return try i.popOperand(Result);
            },
            .Drop => _ = try i.popAnyOperand(),
            .LocalGet => {
                const frame = try i.peekNthControlFrame(0);
                const local_index = try instruction.readULEB128Mem(u32, &i.window);
                const local_value: u64 = frame.locals[local_index];
                try i.pushOperand(u64, local_value);
            },
            .I32Const => {
                const x = try instruction.readILEB128Mem(i32, &i.window);
                try i.pushOperand(i32, x);
            },
            .I32LtS => {
                const a = try i.popOperand(i32);
                const b = try i.popOperand(i32);
                std.debug.warn("b < a = {} < {} = {}\n", .{ b, a, b < a });
                try i.pushOperand(i32, @as(i32, if (b < a) 1 else 0));
            },
            .I32Add => {
                // TODO: does wasm wrap?
                const a = try i.popOperand(i32);
                const b = try i.popOperand(i32);
                std.debug.warn("a + b = {} + {} = {}\n", .{ a, b, a + b });
                try i.pushOperand(i32, a + b);
            },
            .I32Sub => {
                const a = try i.popOperand(i32);
                const b = try i.popOperand(i32);
                std.debug.warn("b - a = {} - {} = {}\n", .{ b, a, b - a });
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

    pub fn popAnyOperand(i: *Interpreter) !u64 {
        if (i.op_stack_size == 0) return error.OperandStackUnderflow;
        const value = i.op_stack[i.op_stack_size - 1];
        i.op_stack_size -= 1;
        return value;
    }

    // TODO: if the code is validated, do we need to know the params count
    //       i.e. can we get rid of the dependency on params so that we don't
    //       have to lookup a function (necessarily)
    pub fn pushControlFrame(self: *Interpreter, frame: ControlFrame, params_and_locals_count: usize) !void {
        if (self.ctrl_stack.len == self.ctrl_stack_mem.len) return error.ControlStackOverflow;
        self.ctrl_stack = self.ctrl_stack_mem[0 .. self.ctrl_stack.len + 1];

        const current_frame = &self.ctrl_stack[self.ctrl_stack.len - 1];
        current_frame.* = frame;
        current_frame.locals = self.op_stack[frame.locals_start .. frame.locals_start + params_and_locals_count];
        for (current_frame.locals) |local, i| {
            std.debug.warn("pushControlFrame.local[{}] = {}\n", .{ i, local });
        }
        // current_frame.locals_start = self.op_stack_size;
    }

    pub fn popControlFrame(self: *Interpreter) !ControlFrame {
        if (self.ctrl_stack.len == 0) return error.ControlStackUnderflow;
        const frame = self.ctrl_stack[self.ctrl_stack.len - 1];
        self.ctrl_stack = self.ctrl_stack[0 .. self.ctrl_stack.len - 1];
        return frame;
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
        const label = self.label_stack[self.label_stack.len - 1];
        self.label_stack = self.label_stack[0 .. self.label_stack.len - 1];
        return label;
    }

    // peekNthLabel
    //
    // Returns nth label on the Label stack relative to the top of the stack
    //
    fn peekNthLabel(self: *Interpreter, index: u32) !*Label {
        if (index + 1 > self.label_stack.len) return error.LabelStackUnderflow;
        return &self.label_stack[self.label_stack.len - index - 1];
    }

    pub const ControlFrame = struct {
        locals: []u64 = undefined, // TODO: we're in trouble if we move our stacks in memory
        locals_start: usize = 0,
        return_arity: usize = 0,
    };

    // Label
    //
    // - code: the code we should interpret after `end`
    pub const Label = struct {
        code: []const u8 = undefined,
        op_stack_start: usize, // u32?
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
