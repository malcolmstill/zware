const std = @import("std");
const mem = std.mem;
const math = std.math;
const ArrayList = std.ArrayList;
const Module = @import("module.zig").Module;
const ValueType = @import("module.zig").ValueType;
const Instance = @import("instance.zig").Instance;
const Instruction = @import("function.zig").Instruction;
const WasmError = @import("function.zig").WasmError;
const instruction = @import("instruction.zig");
const Opcode = @import("instruction.zig").Opcode;

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
    frame_stack_mem: []Frame = undefined,
    frame_stack: []Frame = undefined,
    label_stack_mem: []Label = undefined,
    label_stack: []Label = undefined,

    // continuation: []Instruction = undefined,
    inst: *Instance = undefined,
    // code: []Instruction = undefined,
    ip: usize = 0,
    continuation_end: usize = 0,
    function_start: usize = 0,

    pub fn init(op_stack_mem: []u64, frame_stack_mem: []Frame, label_stack_mem: []Label, inst: *Instance) Interpreter {
        return Interpreter{
            .op_stack_mem = op_stack_mem,
            .op_stack = op_stack_mem[0..0],
            .frame_stack_mem = frame_stack_mem,
            .frame_stack = frame_stack_mem[0..0],
            .label_stack_mem = label_stack_mem,
            .label_stack = label_stack_mem[0..0],
            .inst = inst,
        };
    }

    inline fn dispatch(self: *Interpreter, next_ip: usize, instr: Instruction, err: *?WasmError) void {
        const next_instr = self.inst.module.parsed_code.items[next_ip];
        // std.debug.warn("NEXT INSTR = {}\n", .{@as(Opcode, next_instr)});
        // self.continuation = self.continuation[1..];
        // self.ip += 1;
        return @call(.{ .modifier = .always_tail }, lookup(next_instr), .{ self, next_ip, next_instr, err });
    }

    fn impl_unreachable(self: *Interpreter, ip: usize, instr: Instruction, err: *?WasmError) void {
        //std.debug.warn("instr = {}, ip = {}\n", .{ @as(Opcode, instr), ip });
        err.* = error.TrapUnreachable;
    }

    fn impl_nop(self: *Interpreter, ip: usize, instr: Instruction, err: *?WasmError) void {
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, instr, err });
    }

    fn impl_block(self: *Interpreter, ip: usize, instr: Instruction, err: *?WasmError) void {
        //std.debug.warn("instr = {}, ip = {}\n", .{ @as(Opcode, instr), ip });
        const block = instr.block;

        self.pushLabel(Label{
            .return_arity = block.return_arity,
            .op_stack_len = self.op_stack.len - block.param_arity, // equivalent to pop and push
            // .continuation = self.inst.module.parsed_code.items[block.continuation.offset .. block.continuation.offset + block.continuation.count], // block.continuation,
            .ip_start = block.ip,
            .ip_end = block.ip_end,
        }) catch |e| {
            err.* = e;
            return;
        };

        // if (self.continuation.len == 0) return;
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, instr, err });
    }

    fn impl_loop(self: *Interpreter, ip: usize, instr: Instruction, err: *?WasmError) void {
        //std.debug.warn("instr = {}, ip = {}\n", .{ @as(Opcode, instr), ip });
        const block = instr.loop;

        self.pushLabel(Label{
            // note that we use block_params rather than block_returns for return arity:
            .return_arity = block.param_arity,
            .op_stack_len = self.op_stack.len - block.param_arity,
            // .continuation = self.inst.module.parsed_code.items[block.continuation.offset .. block.continuation.offset + block.continuation.count],
            .ip_start = block.ip,
            .ip_end = block.ip_end,
        }) catch |e| {
            err.* = e;
            return;
        };

        // if (self.continuation.len == 0) return;
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, instr, err });
    }

    fn impl_if(self: *Interpreter, ip: usize, instr: Instruction, err: *?WasmError) void {
        //std.debug.warn("instr = {}, ip = {}\n", .{ @as(Opcode, instr), ip });
        const block = instr.@"if";
        var next_ip = ip;

        const condition = self.popOperand(u32);
        // std.debug.warn("condition = {}, next_ip = {}\n", .{ condition, next_ip });

        if (condition == 0) {
            // std.debug.warn("IF == FALSE\n", .{});
            // We'll skip to end
            // self.continuation = self.inst.module.parsed_code.items[block.continuation.offset .. block.continuation.offset + block.continuation.count];
            // self.ip = block.ip - 1;
            // std.debug.warn("block.ip = {}\n", .{block.ip});
            next_ip = block.ip;
            self.continuation_end = block.ip_end;
            // unless we have an else branch

            // if (block.else_continuation) |else_continuation| {
            //     next_ip = block.else_ip;
            //     // self.continuation = self.inst.module.parsed_code.items[else_continuation.offset .. else_continuation.offset + else_continuation.count];
            //     // self.ip = block.else_ip;

            //     // We are inside the if branch
            //     self.pushLabel(Label{
            //         .return_arity = block.return_arity,
            //         .op_stack_len = self.op_stack.len - block.param_arity,
            //         // .continuation = self.inst.module.parsed_code.items[block.continuation.offset .. block.continuation.offset + block.continuation.count], // block.continuation,
            //         .ip_start = block.ip,
            //         .ip_end = block.ip_end,
            //     }) catch |e| {
            //         err.* = e;
            //         return;
            //     };
            // }
        } else {
            // std.debug.warn("IF == TRUE\n", .{});
            // We are inside the if branch
            next_ip = ip + 1;
            self.pushLabel(Label{
                .return_arity = block.return_arity,
                .op_stack_len = self.op_stack.len - block.param_arity,
                // .continuation = self.inst.module.parsed_code.items[block.continuation.offset .. block.continuation.offset + block.continuation.count], // block.continuation,
                .ip_start = block.ip,
                .ip_end = block.ip_end,
            }) catch |e| {
                err.* = e;
                return;
            };
        }

        // if (self.continuation.len == 0) return;
        // std.debug.warn("IF: next ip = {}\n", .{next_ip});
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, instr, err });
    }

    fn impl_end(self: *Interpreter, ip: usize, instr: Instruction, err: *?WasmError) void {
        std.debug.warn("impl_end: instr = {}, ip = {}\n", .{ @as(Opcode, instr), ip });
        const label = self.popLabel();
        var next_ip = ip + 1;

        // It seems like we need to special case end for a function call. This
        // doesn't seem quite right because the spec doesn't mention it. On
        // call we push a label containing a continuation which is the code to
        // resume after the call has returned. We want to use that if we've run
        // out of code in the current function, i.e. self.continuation is empty
        // if (self.continuation.len == 0) {
        if (self.ip == self.continuation_end) {
            const frame = self.peekNthFrame(0);
            const n = label.return_arity;
            var dst = self.op_stack[label.op_stack_len .. label.op_stack_len + n];
            const src = self.op_stack[self.op_stack.len - n ..];
            mem.copy(u64, dst, src);

            self.op_stack = self.op_stack[0 .. label.op_stack_len + n];
            self.label_stack = self.label_stack[0..frame.label_stack_len];
            // self.continuation = label.continuation;
            // self.ip = label.ip_start;
            next_ip = label.ip_start;
            self.continuation_end = label.ip_end;
            _ = self.popFrame();
            self.inst = frame.inst;
        }

        // if (self.continuation.len == 0) return;
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, instr, err });
    }

    fn impl_local_get(self: *Interpreter, ip: usize, instr: Instruction, err: *?WasmError) void {
        const local_index = instr.@"local.get";

        const frame = self.peekNthFrame(0);
        const local_value: u64 = frame.locals[local_index];
        // std.debug.warn("instr = {}, ip = {}, value = {}\n", .{ @as(Opcode, instr), ip, local_value });
        self.pushOperand(u64, local_value) catch |e| {
            err.* = e;
            return;
        };

        // if (self.continuation.len == 0) return;
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, instr, err });
    }

    fn impl_i32_const(self: *Interpreter, ip: usize, instr: Instruction, err: *?WasmError) void {
        //std.debug.warn("instr = {}, ip = {}\n", .{ @as(Opcode, instr), ip });
        const x = instr.@"i32.const";

        self.pushOperand2(i32, x);

        // if (self.continuation.len == 0) return;
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, instr, err });
    }

    fn impl_i32_eq(self: *Interpreter, ip: usize, instr: Instruction, err: *?WasmError) void {
        //std.debug.warn("instr = {}, ip = {}\n", .{ @as(Opcode, instr), ip });
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperand(u32, @as(u32, if (c1 == c2) 1 else 0)) catch |e| {
            err.* = e;
            return;
        };

        // if (self.continuation.len == 0) return;
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, instr, err });
    }

    fn impl_i32_sub(self: *Interpreter, ip: usize, instr: Instruction, err: *?WasmError) void {
        //std.debug.warn("instr = {}, ip = {}\n", .{ @as(Opcode, instr), ip });
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperand(u32, c1 -% c2) catch |e| {
            err.* = e;
            return;
        };

        // if (self.continuation.len == 0) return;
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, instr, err });
    }

    fn impl_i32_add(self: *Interpreter, ip: usize, instr: Instruction, err: *?WasmError) void {
        //std.debug.warn("instr = {}, ip = {}\n", .{ @as(Opcode, instr), ip });
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperand(u32, c1 +% c2) catch |e| {
            err.* = e;
            return;
        };

        // if (self.continuation.len == 0) return;
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, instr, err });
    }

    fn impl_return(self: *Interpreter, ip: usize, instr: Instruction, err: *?WasmError) void {
        //std.debug.warn("instr = {}, ip = {}\n", .{ @as(Opcode, instr), ip });
        const frame = self.peekNthFrame(0);

        const n = frame.return_arity;

        const label = self.label_stack[frame.label_stack_len];
        // self.continuation = label.continuation; FIX
        var next_ip = label.ip_start;
        self.continuation_end = label.ip_end;
        // std.debug.warn("return next_ip = {}, continuation-end = {}\n", .{ next_ip, self.continuation_end });

        // The mem copy is equivalent of popping n operands, doing everything
        // up to and including popFrame and then repushing the n operands
        var dst = self.op_stack[label.op_stack_len .. label.op_stack_len + n];
        const src = self.op_stack[self.op_stack.len - n ..];
        mem.copy(u64, dst, src);

        self.op_stack = self.op_stack[0 .. label.op_stack_len + n];
        self.label_stack = self.label_stack[0..frame.label_stack_len];

        _ = self.popFrame();
        self.inst = frame.inst;

        if (self.frame_stack.len == 0) return;
        // if (self.continuation.len == 0) return;
        // if (next_ip == self.function_start) return; // if our label is the
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, instr, err });
    }

    fn impl_call(self: *Interpreter, ip: usize, instr: Instruction, err: *?WasmError) void {
        //std.debug.warn("instr = {}, ip = {}\n", .{ @as(Opcode, instr), ip });
        const function_index = instr.call;
        const function = self.inst.getFunc(function_index) catch |e| {
            err.* = e;
            return;
        };
        var next_ip = ip;

        switch (function) {
            .function => |f| {
                // Make space for locals (again, params already on stack)
                var j: usize = 0;
                while (j < f.locals_count) : (j += 1) {
                    self.pushOperand(u64, 0) catch |e| {
                        err.* = e;
                        return;
                    };
                }

                // Consume parameters from the stack
                self.pushFrame(Frame{
                    .op_stack_len = self.op_stack.len - f.params.len - f.locals_count,
                    .label_stack_len = self.label_stack.len,
                    .return_arity = f.results.len,
                    .inst = self.inst,
                }, f.locals_count + f.params.len) catch |e| {
                    err.* = e;
                    return;
                };

                // Our continuation is the code after call
                self.pushLabel(Label{
                    .return_arity = f.results.len,
                    .op_stack_len = self.op_stack.len - f.params.len - f.locals_count,
                    // .continuation = self.continuation,
                    .ip_start = ip + 1,
                    .ip_end = self.continuation_end,
                }) catch |e| {
                    err.* = e;
                    return;
                };

                // self.continuation = f.code;
                next_ip = f.ip_start;
                self.continuation_end = f.ip_end;

                self.inst = self.inst.store.instance(f.instance) catch |e| {
                    err.* = e;
                    return;
                };
            },
            .host_function => |hf| {
                hf.func(self) catch |e| {
                    err.* = e;
                    return;
                };
            },
        }

        // std.debug.warn("\n", .{});
        // if (self.continuation.len == 0) return;
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, instr, err });
    }

    const InstructionFunction = fn (*Interpreter, usize, Instruction, *?WasmError) void;

    // inline fn lookup(instr: Instruction) InstructionFunction {
    //     return switch (instr) {
    //         .@"unreachable" => impl_unreachable,
    //         .nop => impl_nop,
    //         .block => impl_block,
    //         .loop => impl_loop,
    //         .@"if" => impl_if,
    //         .end => impl_end,
    //         .@"local.get" => impl_local_get,
    //         .@"i32.const" => impl_i32_const,
    //         .@"return" => impl_return,
    //         .@"i32.eq" => impl_i32_eq,
    //         .@"i32.sub" => impl_i32_sub,
    //         .@"i32.add" => impl_i32_add,
    //         .call => impl_call,
    //         else => unreachable,
    //     };
    // }

    pub fn invoke(self: *Interpreter, ip: usize) !void {
        // self.continuation = code;
        // const instr = self.continuation[0];
        const instr = self.inst.module.parsed_code.items[ip];
        // self.continuation = self.continuation[1..];

        var err: ?WasmError = null;

        // @call(.{}, dispatj(instr), .{ self, instr, &err });
        @call(.{}, lookup(instr), .{ self, ip, instr, &err });
        if (err) |e| return e;
    }

    fn debug(self: *Interpreter, opcode: Instruction) void {
        std.debug.warn("\n=====================================================\n", .{});
        std.debug.warn("after: {}\n", .{opcode});
        var i: usize = 0;
        while (i < self.op_stack.len) : (i += 1) {
            std.debug.warn("stack[{}] = {}\n", .{ i, self.op_stack[i] });
        }
        std.debug.warn("\n", .{});

        i = 0;
        while (i < self.label_stack.len) : (i += 1) {
            std.debug.warn("label_stack[{}] = [{}, {}, {x}]\n", .{ i, self.label_stack[i].return_arity, self.label_stack[i].op_stack_len, self.label_stack[i].continuation });
        }
        std.debug.warn("\n", .{});

        i = 0;
        while (i < self.frame_stack.len) : (i += 1) {
            std.debug.warn("frame_stack[{}] = [{}, {}, {}]\n", .{ i, self.frame_stack[i].return_arity, self.frame_stack[i].op_stack_len, self.frame_stack[i].label_stack_len });
        }
        std.debug.warn("=====================================================\n", .{});
    }

    // https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-br-l
    pub fn branch(self: *Interpreter, target: u32) void {
        const label = self.peekNthLabel(target);
        const n = label.return_arity;

        var dest = self.op_stack[label.op_stack_len .. label.op_stack_len + n];
        const src = self.op_stack[self.op_stack.len - n ..];

        mem.copy(u64, dest, src);

        self.op_stack = self.op_stack[0 .. label.op_stack_len + n];
        _ = self.popLabels(target);

        self.continuation = label.continuation;
    }

    pub fn pushOperand2(self: *Interpreter, comptime T: type, value: T) void {
        // if (self.op_stack.len == self.op_stack_mem.len) return error.OperandStackOverflow;

        self.op_stack = self.op_stack_mem[0 .. self.op_stack.len + 1];

        self.op_stack[self.op_stack.len - 1] = switch (T) {
            i32 => @as(u64, @bitCast(u32, value)),
            i64 => @bitCast(u64, value),
            f32 => @as(u64, @bitCast(u32, value)),
            f64 => @bitCast(u64, value),
            u32 => @as(u64, value), // TODO: figure out types
            u64 => value,
            else => |t| @compileError("Unsupported operand type: " ++ @typeName(t)),
        };
    }

    pub fn pushOperand(self: *Interpreter, comptime T: type, value: T) !void {
        if (self.op_stack.len == self.op_stack_mem.len) return error.OperandStackOverflow;

        self.op_stack = self.op_stack_mem[0 .. self.op_stack.len + 1];

        self.op_stack[self.op_stack.len - 1] = switch (T) {
            i32 => @as(u64, @bitCast(u32, value)),
            i64 => @bitCast(u64, value),
            f32 => @as(u64, @bitCast(u32, value)),
            f64 => @bitCast(u64, value),
            u32 => @as(u64, value), // TODO: figure out types
            u64 => value,
            else => |t| @compileError("Unsupported operand type: " ++ @typeName(t)),
        };
    }

    pub fn popOperand(self: *Interpreter, comptime T: type) T {
        // TODO: if we've validated the wasm, do we need to perform this check:
        // if (self.op_stack.len == 0) return error.OperandStackUnderflow;
        defer self.op_stack = self.op_stack[0 .. self.op_stack.len - 1];

        const value = self.op_stack[self.op_stack.len - 1];
        return switch (T) {
            i32 => @bitCast(i32, @truncate(u32, value)),
            i64 => @bitCast(i64, value),
            f32 => @bitCast(f32, @truncate(u32, value)),
            f64 => @bitCast(f64, value),
            u32 => @truncate(u32, value), // TODO: figure out types
            u64 => value,
            else => |t| @compileError("Unsupported operand type: " ++ @typeName(t)),
        };
    }

    pub fn pushAnyOperand(self: *Interpreter, value: u64) !void {
        if (self.op_stack.len == self.op_stack_mem.len) return error.OperandStackOverflow;

        self.op_stack = self.op_stack_mem[0 .. self.op_stack.len + 1];

        self.op_stack[self.op_stack.len - 1] = value;
    }

    pub fn popAnyOperand(self: *Interpreter) u64 {
        // if (self.op_stack.len == 0) return error.OperandStackUnderflow;
        defer self.op_stack = self.op_stack[0 .. self.op_stack.len - 1];

        return self.op_stack[self.op_stack.len - 1];
    }

    fn peekNthOperand(self: *Interpreter, index: u32) u64 {
        // if (index + 1 > self.op_stack.len) return error.OperandStackUnderflow;
        return self.op_stack[self.op_stack.len - index - 1];
    }

    // TODO: if the code is validated, do we need to know the params count
    //       i.e. can we get rid of the dependency on params so that we don't
    //       have to lookup a function (necessarily)
    pub fn pushFrame(self: *Interpreter, frame: Frame, params_and_locals_count: usize) !void {
        if (self.frame_stack.len == self.frame_stack_mem.len) return error.ControlStackOverflow;
        self.frame_stack = self.frame_stack_mem[0 .. self.frame_stack.len + 1];

        const current_frame = &self.frame_stack[self.frame_stack.len - 1];
        current_frame.* = frame;
        // TODO: index out of bounds (error if we've run out of operand stack space):
        current_frame.locals = self.op_stack[frame.op_stack_len .. frame.op_stack_len + params_and_locals_count];
    }

    pub fn popFrame(self: *Interpreter) Frame {
        // if (self.frame_stack.len == 0) return error.ControlStackUnderflow;
        defer self.frame_stack = self.frame_stack[0 .. self.frame_stack.len - 1];

        return self.frame_stack[self.frame_stack.len - 1];
    }

    // peekNthFrame
    //
    // Returns nth label on the Label stack relative to the top of the stack
    //
    fn peekNthFrame(self: *Interpreter, index: u32) *Frame {
        // if (index + 1 > self.frame_stack.len) return error.ControlStackUnderflow;
        return &self.frame_stack[self.frame_stack.len - index - 1];
    }

    pub fn pushLabel(self: *Interpreter, label: Label) !void {
        if (self.label_stack.len == self.label_stack_mem.len) return error.LabelStackOverflow;
        self.label_stack = self.label_stack_mem[0 .. self.label_stack.len + 1];
        const current_label = self.peekNthLabel(0);
        current_label.* = label;
    }

    pub fn popLabel(self: *Interpreter) Label {
        // if (self.label_stack.len == 0) return error.ControlStackUnderflow;
        defer self.label_stack = self.label_stack[0 .. self.label_stack.len - 1];

        return self.label_stack[self.label_stack.len - 1];
    }

    // peekNthLabel
    //
    // Returns nth label on the Label stack relative to the top of the stack
    //
    fn peekNthLabel(self: *Interpreter, index: u32) *Label {
        // if (index + 1 > self.label_stack.len) return error.LabelStackUnderflow;
        return &self.label_stack[self.label_stack.len - index - 1];
    }

    // popLabels
    //
    // target: branch target (relative to current scope which is 0)
    //
    // popLabels pops labels up to and including `target`. Returns the
    // the label at `target`.
    pub fn popLabels(self: *Interpreter, target: u32) Label {
        // if (target >= self.label_stack.len) return error.LabelStackUnderflow;
        const target_label = self.label_stack[self.label_stack.len - target - 1];

        self.label_stack = self.label_stack[0 .. self.label_stack.len - target - 1];

        // Return target_label so the caller can get the continuation
        return target_label;
    }

    pub const Frame = struct {
        locals: []u64 = undefined, // TODO: we're in trouble if we move our stacks in memory
        return_arity: usize = 0,
        op_stack_len: usize,
        label_stack_len: usize,
        inst: *Instance,
    };

    // Label
    //
    // - code: the code we should interpret after `end`
    pub const Label = struct {
        return_arity: usize = 0,
        // continuation: []Instruction = undefined,
        ip_start: usize = 0,
        ip_end: usize = 0,
        op_stack_len: usize, // u32?
    };
};

const testing = std.testing;

test "operand push / pop test" {
    var op_stack: [6]u64 = [_]u64{0} ** 6;
    var frame_stack_mem: [1024]Interpreter.Frame = [_]Interpreter.Frame{undefined} ** 1024;
    var label_stack_mem: [1024]Interpreter.Label = [_]Interpreter.Label{undefined} ** 1024;

    var inst: Instance = undefined;

    var i = Interpreter.init(op_stack[0..], frame_stack_mem[0..], label_stack_mem[0..], &inst);

    try i.pushOperand(i32, 22);
    try i.pushOperand(i32, -23);
    try i.pushOperand(i64, 44);
    try i.pushOperand(i64, -43);
    try i.pushOperand(f32, 22.07);
    try i.pushOperand(f64, 43.07);

    // stack overflow:
    if (i.pushOperand(i32, 0)) |_| {
        return error.TestExpectedError;
    } else |err| {
        if (err != error.OperandStackOverflow) return error.TestUnexpectedError;
    }

    try testing.expectEqual(@as(f64, 43.07), try i.popOperand(f64));
    try testing.expectEqual(@as(f32, 22.07), try i.popOperand(f32));
    try testing.expectEqual(@as(i64, -43), try i.popOperand(i64));
    try testing.expectEqual(@as(i64, 44), try i.popOperand(i64));
    try testing.expectEqual(@as(i32, -23), try i.popOperand(i32));
    try testing.expectEqual(@as(i32, 22), try i.popOperand(i32));

    // stack underflow:
    if (i.popOperand(i32)) |_| {
        return error.TestExpectedError;
    } else |err| {
        if (err != error.OperandStackUnderflow) return error.TestUnexpectedError;
    }
}

test "simple interpret tests" {
    var op_stack: [6]u64 = [_]u64{0} ** 6;
    var frame_stack: [1024]Interpreter.Frame = [_]Interpreter.Frame{undefined} ** 1024;
    var label_stack_mem: [1024]Interpreter.Label = [_]Interpreter.Label{undefined} ** 1024;

    var inst: Instance = undefined;
    var i = Interpreter.init(op_stack[0..], frame_stack[0..], label_stack_mem[0..], &inst);

    try i.pushOperand(i32, 22);
    try i.pushOperand(i32, -23);

    var code = [_]Instruction{Instruction.@"i32.add"};

    try i.invoke(code[0..]);

    try testing.expectEqual(@as(i32, -1), try i.popOperand(i32));
}
