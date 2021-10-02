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

pub const Interpreter = struct {
    stack: []u64 = undefined,
    sp: usize = 0,
    lp: usize = 0,
    fp: usize = 0,

    inst: *Instance = undefined,
    ip: usize = 0,
    continuation_end: usize = 0,
    function_start: usize = 0,

    pub fn init(stack: []u64, inst: *Instance) Interpreter {
        return Interpreter{
            .stack = stack,
            .inst = inst,
        };
    }

    fn print_stack(stack: []u64, sp: usize) void {
        std.debug.warn("=stack===============\n", .{});
        for (stack[0..sp]) |s, i| {
            std.debug.warn("{}: {x}\n", .{ i, s });
        }
        std.debug.warn("=====================\n\n", .{});
    }

    inline fn dispatch(self: *Interpreter, next_ip: usize, code: []Instruction, fp: usize, lp: usize, sp: usize, stack: []u64, err: *?WasmError) void {
        const next_instr = code[next_ip];

        if (std.builtin.mode == .Debug) {
            std.debug.warn("fp = {}, lp = {}, sp = {}\n", .{ fp, lp, sp });
            print_stack(stack, sp);
            const stdin = std.io.getStdIn().reader();
            const stdout = std.io.getStdOut().writer();

            var buf: [10]u8 = undefined;

            stdout.print("Continue:", .{}) catch |e| return;
            _ = stdin.readUntilDelimiterOrEof(buf[0..], '\n') catch |e| return;

            std.debug.warn("next instruction = {}\n", .{next_instr});
        }

        return @call(.{ .modifier = .always_tail }, lookup[@enumToInt(next_instr)], .{ self, next_ip, code, fp, lp, sp, stack, err });
    }

    fn impl_unreachable(self: *Interpreter, ip: usize, code: []Instruction, fp: usize, lp: usize, sp: usize, stack: []u64, err: *?WasmError) void {
        err.* = error.TrapUnreachable;
    }

    fn impl_nop(self: *Interpreter, ip: usize, code: []Instruction, fp: usize, lp: usize, sp: usize, stack: []u64, err: *?WasmError) void {
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, fp, lp, sp, stack, err });
    }

    fn impl_block(self: *Interpreter, ip: usize, code: []Instruction, fp: usize, lp: usize, sp: usize, stack: []u64, err: *?WasmError) void {
        const block = code[ip].block;

        const new_sp = pushLabelInternal(stack, lp, sp, block.return_arity, block.ip, block.ip_end) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, fp, lp, sp, stack, err });
    }

    fn impl_loop(self: *Interpreter, ip: usize, code: []Instruction, fp: usize, lp: usize, sp: usize, stack: []u64, err: *?WasmError) void {
        const loop = code[ip].loop;

        const new_sp = pushLabelInternal(stack, lp, sp, loop.param_arity, loop.ip, loop.ip_end) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, fp, lp, sp, stack, err });
    }

    fn impl_if(self: *Interpreter, ip: usize, code: []Instruction, fp: usize, lp: usize, sp: usize, stack: []u64, err: *?WasmError) void {
        const block = code[ip].@"if";
        var next_ip = ip;
        var next_sp = sp;
        var next_lp = lp;

        const condition = stack[next_sp - 1];
        next_sp -= 1;

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
            next_ip = ip + 1;
            next_lp = sp;
            next_sp = pushLabelInternal(stack, lp, next_sp, block.return_arity, block.ip, block.ip_end) catch |e| {
                err.* = e;
                return;
            };
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, code, fp, next_lp, next_sp, stack, err });
    }

    fn impl_end(self: *Interpreter, ip: usize, code: []Instruction, fp: usize, lp: usize, sp: usize, stack: []u64, err: *?WasmError) void {
        const label = peekLabel(stack, lp);
        var next_ip = ip + 1;
        var next_sp = sp;
        var next_lp = lp;
        var next_fp = fp;

        // It seems like we need to special case end for a function call. This
        // doesn't seem quite right because the spec doesn't mention it. On
        // call we push a label containing a continuation which is the code to
        // resume after the call has returned. We want to use that if we've run
        // out of code in the current function, i.e. self.continuation is empty
        // if (self.continuation.len == 0) {
        if (self.ip == self.continuation_end) {
            // const frame = self.peekNthFrame(0);
            const frame = peekFrame(stack, fp);
            const n = label.return_arity;
            var dst = stack[fp .. fp + n];
            const src = stack[sp - n .. sp];
            mem.copy(u64, dst, src);

            // self.op_stack = self.op_stack[0 .. label.op_stack_len + n];
            next_sp = fp + n;
            next_fp = frame.previous_fp;
            next_lp = label.previous_lp;
            next_ip = label.ip_start;
            self.continuation_end = label.ip_end;
            // _ = self.popFrame();
            self.inst = frame.instance;
        } else {}

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, code, fp, lp, sp, stack, err });
    }

    fn impl_local_get(self: *Interpreter, ip: usize, code: []Instruction, fp: usize, lp: usize, sp: usize, stack: []u64, err: *?WasmError) void {
        const local_index = code[ip].@"local.get";

        // std.debug.warn("fp = {}, lp = {}, sp = {}\n", .{ fp, lp, sp });
        // print_stack(stack, sp);

        const frame = peekFrame(stack, fp);
        const local_value: u64 = stack[fp - frame.locals_count + local_index]; //frame.locals[local_index];

        _ = pushOperand3(u64, stack, sp, local_value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, fp, lp, sp + 1, stack, err });
    }

    fn impl_i32_const(self: *Interpreter, ip: usize, code: []Instruction, fp: usize, lp: usize, sp: usize, stack: []u64, err: *?WasmError) void {
        const instr = code[ip];

        _ = pushOperand3(i32, stack, sp, instr.@"i32.const") catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, fp, lp, sp + 1, stack, err });
    }

    fn impl_i32_eq(self: *Interpreter, ip: usize, code: []Instruction, fp: usize, lp: usize, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = stack[sp - 1];
        const c1 = stack[sp - 2];

        stack[sp - 2] = @as(u32, if (c1 == c2) 1 else 0);
        // sp -= 1;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, fp, lp, sp - 1, stack, err });
    }

    fn impl_i32_sub(self: *Interpreter, ip: usize, code: []Instruction, fp: usize, lp: usize, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = stack[sp - 1];
        const c1 = stack[sp - 2];

        stack[sp - 2] = c1 -% c2;
        // sp -= 1;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, fp, lp, sp - 1, stack, err });
    }

    fn impl_i32_add(self: *Interpreter, ip: usize, code: []Instruction, fp: usize, lp: usize, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = stack[sp - 1];
        const c1 = stack[sp - 2];

        stack[sp - 2] = c1 +% c2;
        // sp -= 1;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, fp, lp, sp - 1, stack, err });
    }

    fn impl_return(self: *Interpreter, ip: usize, code: []Instruction, fp: usize, lp: usize, sp: usize, stack: []u64, err: *?WasmError) void {
        const instr = code[ip];
        var next_ip = ip;
        var next_sp = sp;
        var next_lp = lp;
        var next_fp = fp;

        const frame = peekFrame(stack, fp);
        const label = peekLabel(stack, fp + 4);
        const n = label.return_arity;
        var dst = stack[fp - frame.locals_count .. fp - frame.locals_count + n];
        const src = stack[sp - n .. sp];
        mem.copy(u64, dst, src);

        next_sp = fp - frame.locals_count + n;
        next_fp = frame.previous_fp;
        next_lp = label.previous_lp;
        next_ip = label.ip_start;
        self.continuation_end = label.ip_end;

        if (frame.previous_fp == 0xFFFFFFFFFFFFFFFF) {
            self.sp = sp;
            return;
        }
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, code, next_fp, lp, next_sp, stack, err });
    }

    fn impl_call(self: *Interpreter, ip: usize, code: []Instruction, fp: usize, lp: usize, sp: usize, stack: []u64, err: *?WasmError) void {
        const instr = code[ip];

        const function_index = instr.call;
        const function = self.inst.getFunc(function_index) catch |e| {
            err.* = e;
            return;
        };
        var next_ip = ip;
        var next_sp = sp;
        var next_lp = lp;
        var next_fp = fp;

        switch (function) {
            .function => |f| {
                // self.pushNOperands(u64, f.locals_count, 0) catch |e| {
                //     err.* = e;
                //     return;
                // };
                next_sp += f.locals_count;

                // Consume parameters from the stack
                next_fp = next_sp;
                next_sp = pushFrameInternal(stack, fp, next_sp, f.params.len + f.locals_count, f.results.len, self.inst) catch |e| {
                    err.* = e;
                    return;
                };

                // Our continuation is the code after call
                next_sp = pushLabelInternal(stack, next_lp, next_sp, f.results.len, ip + 1, self.continuation_end) catch |e| {
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

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, code, next_fp, lp, next_sp, stack, err });
    }

    const InstructionFunction = fn (*Interpreter, usize, []Instruction, usize, usize, usize, []u64, *?WasmError) void;

    const lookup = [256]InstructionFunction{
        impl_unreachable, impl_nop,       impl_block, impl_loop, impl_if,  impl_nop, impl_nop,    impl_nop, impl_nop, impl_nop, impl_nop,     impl_end,     impl_nop, impl_nop, impl_nop, impl_return,
        impl_call,        impl_nop,       impl_nop,   impl_nop,  impl_nop, impl_nop, impl_nop,    impl_nop, impl_nop, impl_nop, impl_nop,     impl_nop,     impl_nop, impl_nop, impl_nop, impl_nop,
        impl_local_get,   impl_nop,       impl_nop,   impl_nop,  impl_nop, impl_nop, impl_nop,    impl_nop, impl_nop, impl_nop, impl_nop,     impl_nop,     impl_nop, impl_nop, impl_nop, impl_nop,
        impl_nop,         impl_nop,       impl_nop,   impl_nop,  impl_nop, impl_nop, impl_nop,    impl_nop, impl_nop, impl_nop, impl_nop,     impl_nop,     impl_nop, impl_nop, impl_nop, impl_nop,
        impl_nop,         impl_i32_const, impl_nop,   impl_nop,  impl_nop, impl_nop, impl_i32_eq, impl_nop, impl_nop, impl_nop, impl_nop,     impl_nop,     impl_nop, impl_nop, impl_nop, impl_nop,
        impl_nop,         impl_nop,       impl_nop,   impl_nop,  impl_nop, impl_nop, impl_nop,    impl_nop, impl_nop, impl_nop, impl_nop,     impl_nop,     impl_nop, impl_nop, impl_nop, impl_nop,
        impl_nop,         impl_nop,       impl_nop,   impl_nop,  impl_nop, impl_nop, impl_nop,    impl_nop, impl_nop, impl_nop, impl_i32_add, impl_i32_sub, impl_nop, impl_nop, impl_nop, impl_nop,
        impl_nop,         impl_nop,       impl_nop,   impl_nop,  impl_nop, impl_nop, impl_nop,    impl_nop, impl_nop, impl_nop, impl_nop,     impl_nop,     impl_nop, impl_nop, impl_nop, impl_nop,
        impl_nop,         impl_nop,       impl_nop,   impl_nop,  impl_nop, impl_nop, impl_nop,    impl_nop, impl_nop, impl_nop, impl_nop,     impl_nop,     impl_nop, impl_nop, impl_nop, impl_nop,
        impl_nop,         impl_nop,       impl_nop,   impl_nop,  impl_nop, impl_nop, impl_nop,    impl_nop, impl_nop, impl_nop, impl_nop,     impl_nop,     impl_nop, impl_nop, impl_nop, impl_nop,
        impl_nop,         impl_nop,       impl_nop,   impl_nop,  impl_nop, impl_nop, impl_nop,    impl_nop, impl_nop, impl_nop, impl_nop,     impl_nop,     impl_nop, impl_nop, impl_nop, impl_nop,
        impl_nop,         impl_nop,       impl_nop,   impl_nop,  impl_nop, impl_nop, impl_nop,    impl_nop, impl_nop, impl_nop, impl_nop,     impl_nop,     impl_nop, impl_nop, impl_nop, impl_nop,
        impl_nop,         impl_nop,       impl_nop,   impl_nop,  impl_nop, impl_nop, impl_nop,    impl_nop, impl_nop, impl_nop, impl_nop,     impl_nop,     impl_nop, impl_nop, impl_nop, impl_nop,
        impl_nop,         impl_nop,       impl_nop,   impl_nop,  impl_nop, impl_nop, impl_nop,    impl_nop, impl_nop, impl_nop, impl_nop,     impl_nop,     impl_nop, impl_nop, impl_nop, impl_nop,
        impl_nop,         impl_nop,       impl_nop,   impl_nop,  impl_nop, impl_nop, impl_nop,    impl_nop, impl_nop, impl_nop, impl_nop,     impl_nop,     impl_nop, impl_nop, impl_nop, impl_nop,
        impl_nop,         impl_nop,       impl_nop,   impl_nop,  impl_nop, impl_nop, impl_nop,    impl_nop, impl_nop, impl_nop, impl_nop,     impl_nop,     impl_nop, impl_nop, impl_nop, impl_nop,
    };

    pub fn invoke(self: *Interpreter, ip: usize) !void {
        const instr = self.inst.module.parsed_code.items[ip];
        var err: ?WasmError = null;

        std.debug.warn("fp = {}, lp = {}, sp = {}\n", .{ self.fp, self.lp, self.sp });
        print_stack(self.stack, self.sp);

        @call(.{}, lookup[@enumToInt(instr)], .{ self, ip, self.inst.module.parsed_code.items, self.fp, self.lp, self.sp, self.stack, &err });
        if (err) |e| return e;
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

    pub fn pushOperand(self: *Interpreter, comptime T: type, value: T) !void {
        if (self.sp == self.stack.len) return error.OperandStackOverflow;

        // self.op_stack = self.op_stack[0 .. self.op_stack.len + 1];
        self.sp += 1;

        self.stack[self.sp - 1] = switch (T) {
            i32 => @as(u64, @bitCast(u32, value)),
            i64 => @bitCast(u64, value),
            f32 => @as(u64, @bitCast(u32, value)),
            f64 => @bitCast(u64, value),
            u32 => @as(u64, value), // TODO: figure out types
            u64 => value,
            else => |t| @compileError("Unsupported operand type: " ++ @typeName(t)),
        };
    }

    pub fn pushOperand3(comptime T: type, stack: []u64, sp: usize, value: T) !usize {
        if (sp == stack.len) return error.StackOverflow;

        stack[sp] = switch (T) {
            i32 => @as(u64, @bitCast(u32, value)),
            i64 => @bitCast(u64, value),
            f32 => @as(u64, @bitCast(u32, value)),
            f64 => @bitCast(u64, value),
            u32 => @as(u64, value), // TODO: figure out types
            u64 => value,
            else => |t| @compileError("Unsupported operand type: " ++ @typeName(t)),
        };

        return sp + 1;
    }

    pub fn pushNOperands(self: *Interpreter, comptime T: type, count: usize) !void {
        if (self.sp + count > self.op_stack.len) return error.OperandStackOverflow;

        self.sp += count;
    }

    // pub fn pushOperand2(self: *Interpreter, comptime T: type, sp: usize, value: T) !usize {
    //     if (sp == self.op_stack.len) return error.OperandStackOverflow;

    //     // self.op_stack = self.op_stack[0 .. self.op_stack.len + 1];

    //     self.op_stack[sp] = switch (T) {
    //         i32 => @as(u64, @bitCast(u32, value)),
    //         i64 => @bitCast(u64, value),
    //         f32 => @as(u64, @bitCast(u32, value)),
    //         f64 => @bitCast(u64, value),
    //         u32 => @as(u64, value), // TODO: figure out types
    //         u64 => value,
    //         else => |t| @compileError("Unsupported operand type: " ++ @typeName(t)),
    //     };
    //     return sp + 1;
    // }

    pub fn popOperand(self: *Interpreter, comptime T: type) T {
        // TODO: if we've validated the wasm, do we need to perform this check:
        // if (self.op_stack.len == 0) return error.OperandStackUnderflow;
        // defer self.op_stack = self.op_stack[0 .. self.op_stack.len - 1];
        defer self.sp -= 1;
        std.debug.warn("popOperand sp = {}\n", .{self.sp});

        const value = self.stack[self.sp - 1];
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
        if (self.op_stack.len == self.op_stack.len) return error.OperandStackOverflow;

        self.op_stack = self.op_stack[0 .. self.op_stack.len + 1];

        self.op_stack[self.op_stack.len - 1] = value;
    }

    // pub fn popAnyOperand(self: *Interpreter) u64 {
    //     defer self.sp -= 1;

    //     return self.op_stack[self.sp - 1];
    // }

    // pub fn peekAnyOperand(stack: []u64, sp: usize) u64 {
    //     return stack
    // }

    fn peekNthOperand(self: *Interpreter, index: u32) u64 {
        return self.op_stack[self.op_stack.len - index - 1];
    }

    pub fn pushFrame(self: *Interpreter, locals_count: usize, return_arity: usize, inst: *Instance) !void {
        self.fp = self.sp;
        self.sp = try pushFrameInternal(self.stack, 0xFFFFFFFFFFFFFFFF, self.sp, locals_count, return_arity, inst);
    }

    fn pushFrameInternal(stack: []u64, fp: usize, sp: usize, locals_count: usize, return_arity: usize, inst: *Instance) !usize {
        var new_sp = sp;

        // 1. Push previous frame pointer
        if (new_sp == stack.len) return error.StackOverflow;
        new_sp += 1;
        stack[new_sp - 1] = fp;

        // 2. Push locals count (params + locals) for this function
        if (new_sp == stack.len) return error.StackOverflow;
        new_sp += 1;
        stack[new_sp - 1] = locals_count;

        // 3. Push return arity for this function
        if (new_sp == stack.len) return error.StackOverflow;
        new_sp += 1;
        stack[new_sp - 1] = return_arity;

        // 4. Push instance for this function
        if (new_sp == stack.len) return error.StackOverflow;
        new_sp += 1;
        stack[new_sp - 1] = @ptrToInt(inst); // TODO: make inst an index into a list of instances?

        return new_sp;
    }

    pub fn popFrame(self: *Interpreter) Frame {
        defer self.frame_ptr -= 1;

        return self.frame_stack_mem[self.frame_ptr - 1];
    }

    // peekNthFrame
    //
    // Returns nth label on the Label stack relative to the top of the stack
    //
    fn peekNthFrame(self: *Interpreter, index: u32) *Frame {
        return &self.frame_stack_mem[self.frame_ptr - index - 1];
    }

    pub fn pushLabel(self: *Interpreter, return_arity: usize, ip_start: usize, ip_end: usize) !void {
        const new_lp = self.sp;
        self.sp = try pushLabelInternal(self.stack, self.lp, self.sp, return_arity, ip_start, ip_end);
        self.lp = new_lp;
    }

    pub fn pushLabelInternal(stack: []u64, lp: usize, sp: usize, return_arity: usize, ip_start: usize, ip_end: usize) !usize {
        var new_sp = sp;

        // 1. Push previous label pointer
        if (new_sp == stack.len) return error.StackOverflow;
        new_sp += 1;
        stack[new_sp - 1] = lp;

        // 2. Push return arity of label
        if (new_sp == stack.len) return error.StackOverflow;
        new_sp += 1;
        stack[new_sp - 1] = return_arity;

        // 3. Push ip start
        if (new_sp == stack.len) return error.StackOverflow;
        new_sp += 1;
        stack[new_sp - 1] = ip_start;

        // 4. Push ip end
        if (new_sp == stack.len) return error.StackOverflow;
        new_sp += 1;
        stack[new_sp - 1] = ip_end;

        return new_sp;
    }

    pub fn peekFrame(stack: []u64, fp: usize) Frame {
        return Frame{
            .previous_fp = stack[fp],
            .locals_count = stack[fp + 1],
            .return_arity = stack[fp + 2],
            .instance = @intToPtr(*Instance, stack[fp + 3]),
        };
    }

    pub fn peekLabel(stack: []u64, lp: usize) Label {
        return Label{
            .previous_lp = stack[lp],
            .return_arity = stack[lp + 1],
            .ip_start = stack[lp + 2],
            .ip_end = stack[lp + 3],
        };
    }

    pub fn popLabel(stack: []u64, lp: usize, sp: usize) Label {
        defer self.label_ptr -= 1;

        return self.label_stack_mem[self.label_ptr - 1];
    }

    // peekNthLabel
    //
    // Returns nth label on the Label stack relative to the top of the stack
    //
    fn peekNthLabel(self: *Interpreter, index: u32) *Label {
        // if (index + 1 > self.label_stack.len) return error.LabelStackUnderflow;
        return &self.label_stack_mem[self.label_ptr - index - 1];
    }

    // popLabels
    //
    // target: branch target (relative to current scope which is 0)
    //
    // popLabels pops labels up to and including `target`. Returns the
    // the label at `target`.
    pub fn popLabels(self: *Interpreter, target: u32) Label {
        const target_label = self.label_stack[self.label_stack.len - target - 1];

        self.label_stack = self.label_stack[0 .. self.label_stack.len - target - 1];

        // Return target_label so the caller can get the continuation
        return target_label;
    }

    const Frame = struct {
        previous_fp: usize,
        locals_count: usize, // params + locals
        return_arity: usize,
        instance: *Instance,
    };

    const Label = struct {
        previous_lp: usize,
        return_arity: usize,
        ip_start: usize,
        ip_end: usize,
    };
};
