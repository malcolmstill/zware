const std = @import("std");
const mem = std.mem;
const math = std.math;
const ArrayList = std.ArrayList;
const Module = @import("module.zig").Module;
const ValueType = @import("module.zig").ValueType;
const Instance = @import("instance.zig").Instance;
const WasmError = @import("function.zig").WasmError;
const instruction = @import("instruction.zig");
const Instruction = instruction.Instruction;
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
    op_ptr: usize = 0,
    frame_stack: []Frame = undefined,
    frame_ptr: usize = 0,
    label_stack: []Label = undefined,
    label_ptr: usize = 0,

    inst: *Instance = undefined,
    ip: usize = 0,

    pub fn init(op_stack: []u64, frame_stack: []Frame, label_stack: []Label, inst: *Instance) Interpreter {
        return Interpreter{
            .op_stack = op_stack,
            .frame_stack = frame_stack,
            .label_stack = label_stack,
            .inst = inst,
        };
    }

    fn debug(self: *Interpreter, opcode: Instruction) void {
        // std.debug.warn("{}\n", .{opcode});
        std.debug.warn("\n=====================================================\n", .{});
        std.debug.warn("before: {}\n", .{opcode});
        var i: usize = 0;
        while (i < self.op_ptr) : (i += 1) {
            std.debug.warn("stack[{}] = {}\n", .{ i, self.op_stack[i] });
        }
        std.debug.warn("\n", .{});

        i = 0;
        while (i < self.label_ptr) : (i += 1) {
            std.debug.warn("label_stack[{}] = [ret_ari: {}, ops_start: {}, break: {x}]\n", .{ i, self.label_stack[i].return_arity, self.label_stack[i].op_stack_len, self.label_stack[i].branch_target });
        }
        std.debug.warn("\n", .{});

        i = 0;
        while (i < self.frame_ptr) : (i += 1) {
            std.debug.warn("frame_stack[{}] = [ret_ari: {}, ops_start: {}, label_start: {}]\n", .{ i, self.frame_stack[i].return_arity, self.frame_stack[i].op_stack_len, self.frame_stack[i].label_stack_len });
        }
        std.debug.warn("=====================================================\n", .{});
    }

    inline fn dispatch(self: *Interpreter, next_ip: usize, code: []Instruction, err: *?WasmError) void {
        const next_instr = code[next_ip];

        return @call(.{ .modifier = .always_tail }, lookup[@enumToInt(next_instr)], .{ self, next_ip, code, err });
    }

    fn impl_ni(_: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        std.debug.warn("not implemented: {any}\n", .{code[ip]});
        err.* = error.NotImplemented;
    }

    fn @"unreachable"(_: *Interpreter, _: usize, _: []Instruction, err: *?WasmError) void {
        err.* = error.TrapUnreachable;
    }

    fn nop(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn block(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const meta = code[ip].block;

        self.pushLabel(Label{
            .return_arity = meta.return_arity,
            .op_stack_len = self.op_ptr - meta.param_arity,
            .branch_target = meta.branch_target,
        }) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn loop(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const meta = code[ip].loop;

        self.pushLabel(Label{
            // note that we use block_params rather than block_returns for return arity:
            .return_arity = meta.param_arity,
            .op_stack_len = self.op_ptr - meta.param_arity,
            .branch_target = meta.branch_target,
        }) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"if"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const meta = code[ip].@"if";
        const condition = self.popOperand(u32);

        self.pushLabel(Label{
            .return_arity = meta.return_arity,
            .op_stack_len = self.op_ptr - meta.param_arity,
            .branch_target = meta.branch_target,
        }) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, if (condition == 0) meta.else_ip else ip + 1, code, err });
    }

    fn @"else"(self: *Interpreter, _: usize, code: []Instruction, err: *?WasmError) void {
        const label = self.popLabel();

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, label.branch_target, code, err });
    }

    fn if_no_else(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const meta = code[ip].if_no_else;
        const condition = self.popOperand(u32);

        if (condition == 0) {
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, meta.branch_target, code, err });
        } else {
            // We are inside the if branch
            self.pushLabel(Label{
                .return_arity = meta.return_arity,
                .op_stack_len = self.op_ptr - meta.param_arity,
                .branch_target = meta.branch_target,
            }) catch |e| {
                err.* = e;
                return;
            };
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
        }
    }

    fn end(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        _ = self.popLabel();

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn br(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const next_ip = self.branch(code[ip].br);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, code, err });
    }

    fn br_if(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const condition = self.popOperand(u32);

        const next_ip = if (condition == 0) ip + 1 else self.branch(code[ip].br_if);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, code, err });
    }

    fn br_table(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const meta = code[ip].br_table;

        const i = self.popOperand(u32);
        const ls = self.inst.module.br_table_indices.items[meta.ls.offset .. meta.ls.offset + meta.ls.count];

        const next_ip = if (i >= ls.len) self.branch(meta.ln) else self.branch(ls[i]);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, code, err });
    }

    fn @"return"(self: *Interpreter, _: usize, _: []Instruction, err: *?WasmError) void {
        const frame = self.peekFrame();
        const n = frame.return_arity;

        const label = self.label_stack[frame.label_stack_len];

        // The mem copy is equivalent of popping n operands, doing everything
        // up to and including popFrame and then repushing the n operands
        var dst = self.op_stack[label.op_stack_len .. label.op_stack_len + n];
        const src = self.op_stack[self.op_ptr - n .. self.op_ptr];
        mem.copy(u64, dst, src);

        self.op_ptr = label.op_stack_len + n;
        self.label_ptr = frame.label_stack_len;

        _ = self.popFrame();

        if (self.frame_ptr == 0) return; // If this is the last frame on the stack we're done invoking

        // We potentially change instance when returning from a function, so restore the inst
        const previous_frame = self.peekFrame();
        self.inst = previous_frame.inst;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, label.branch_target, previous_frame.inst.module.parsed_code.items, err });
    }

    fn call(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const function_index = code[ip].call;

        const function = self.inst.getFunc(function_index) catch |e| {
            err.* = e;
            return;
        };
        var next_ip = ip;

        switch (function) {
            .function => |f| {
                // Check we have enough stack space
                self.checkStackSpace(f.required_stack_space + f.locals_count) catch |e| {
                    err.* = e;
                    return;
                };

                // Make space for locals (again, params already on stack)
                self.op_ptr += f.locals_count;

                self.inst = self.inst.store.instance(f.instance) catch |e| {
                    err.* = e;
                    return;
                };

                // Consume parameters from the stack
                self.pushFrame(Frame{
                    .op_stack_len = self.op_ptr - f.params.len - f.locals_count,
                    .label_stack_len = self.label_ptr,
                    .return_arity = f.results.len,
                    .inst = self.inst,
                }, f.locals_count + f.params.len) catch |e| {
                    err.* = e;
                    return;
                };

                // Our continuation is the code after call
                self.pushLabel(Label{
                    .return_arity = f.results.len,
                    .op_stack_len = self.op_ptr - f.params.len - f.locals_count,
                    .branch_target = ip + 1,
                }) catch |e| {
                    err.* = e;
                    return;
                };

                next_ip = f.start;
            },
            .host_function => |hf| {
                hf.func(self) catch |e| {
                    err.* = e;
                    return;
                };
                next_ip = ip + 1;
            },
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, self.inst.module.parsed_code.items, err });
    }

    fn call_indirect(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const call_indirect_instruction = code[ip].call_indirect;
        var module = self.inst.module;

        const op_func_type_index = call_indirect_instruction.@"type";
        const table_index = call_indirect_instruction.table;

        // Read lookup index from stack
        const lookup_index = self.popOperand(u32);
        const table = self.inst.getTable(table_index) catch |e| {
            err.* = e;
            return;
        };
        const function_handle = table.lookup(lookup_index) catch |e| {
            err.* = e;
            return;
        };
        const function = self.inst.store.function(function_handle) catch |e| {
            err.* = e;
            return;
        };

        var next_ip = ip;

        switch (function) {
            .function => |func| {
                // Check that signatures match
                const call_indirect_func_type = module.types.list.items[op_func_type_index];
                if (!Module.signaturesEqual(func.params, func.results, call_indirect_func_type)) {
                    err.* = error.IndirectCallTypeMismatch;
                    return;
                }

                // Check we have enough stack space
                self.checkStackSpace(func.required_stack_space + func.locals_count) catch |e| {
                    err.* = e;
                    return;
                };

                // Make space for locals (again, params already on stack)
                self.op_ptr += func.locals_count;

                self.inst = self.inst.store.instance(func.instance) catch |e| {
                    err.* = e;
                    return;
                };

                // Consume parameters from the stack
                self.pushFrame(Frame{
                    .op_stack_len = self.op_ptr - func.params.len - func.locals_count,
                    .label_stack_len = self.label_ptr,
                    .return_arity = func.results.len,
                    .inst = self.inst,
                }, func.locals_count + func.params.len) catch |e| {
                    err.* = e;
                    return;
                };

                // Our continuation is the code after call
                self.pushLabel(Label{
                    .return_arity = func.results.len,
                    .op_stack_len = self.op_ptr - func.params.len - func.locals_count,
                    .branch_target = ip + 1,
                }) catch |e| {
                    err.* = e;
                    return;
                };

                next_ip = func.start;
            },
            .host_function => |host_func| {
                const call_indirect_func_type = module.types.list.items[op_func_type_index];
                if (!Module.signaturesEqual(host_func.params, host_func.results, call_indirect_func_type)) {
                    err.* = error.IndirectCallTypeMismatch;
                    return;
                }

                host_func.func(self) catch |e| {
                    err.* = e;
                    return;
                };

                next_ip = ip + 1;
            },
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, self.inst.module.parsed_code.items, err });
    }

    fn fast_call(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const f = code[ip].fast_call;

        // Check we have enough stack space
        self.checkStackSpace(f.required_stack_space + f.locals) catch |e| {
            err.* = e;
            return;
        };

        // Make space for locals (again, params already on stack)
        self.op_ptr += f.locals;

        // Consume parameters from the stack
        self.pushFrame(Frame{
            .op_stack_len = self.op_ptr - f.params - f.locals,
            .label_stack_len = self.label_ptr,
            .return_arity = f.results,
            .inst = self.inst,
        }, f.locals + f.params) catch |e| {
            err.* = e;
            return;
        };

        // Our continuation is the code after call
        self.pushLabel(Label{
            .return_arity = f.results,
            .op_stack_len = self.op_ptr - f.params - f.locals,
            .branch_target = ip + 1,
        }) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, f.start, code, err });
    }

    fn drop(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        _ = self.popAnyOperand();
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn select(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const condition = self.popOperand(u32);
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        if (condition != 0) {
            self.pushOperandNoCheck(u64, c1);
        } else {
            self.pushOperandNoCheck(u64, c2);
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"local.get"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const local_index = code[ip].@"local.get";

        const frame = self.peekFrame();

        self.pushOperandNoCheck(u64, frame.locals[local_index]);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"local.set"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const local_index = code[ip].@"local.set";

        const frame = self.peekFrame();
        frame.locals[local_index] = self.popOperand(u64);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"local.tee"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const local_index = code[ip].@"local.tee";

        const frame = self.peekFrame();
        frame.locals[local_index] = self.peekOperand();

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"global.get"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const global_index = code[ip].@"global.get";

        const global = self.inst.getGlobal(global_index) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(u64, global.value);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"global.set"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const global_index = code[ip].@"global.set";
        const value = self.popAnyOperand();

        const global = self.inst.getGlobal(global_index) catch |e| {
            err.* = e;
            return;
        };

        global.value = value;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.load"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i32.load";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = self.popOperand(u32);

        const value = memory.read(u32, offset, address) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(u32, value);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.load"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i64.load";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = self.popOperand(u32);

        const value = memory.read(u64, offset, address) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(u64, value);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.load"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"f32.load";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = self.popOperand(u32);

        const value = memory.read(f32, offset, address) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(f32, value);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.load"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"f64.load";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = self.popOperand(u32);

        const value = memory.read(f64, offset, address) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(f64, value);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.load8_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i32.load8_s";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = self.popOperand(u32);

        const value = memory.read(i8, offset, address) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(i32, value);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.load8_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i32.load8_u";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = self.popOperand(u32);

        const value = memory.read(u8, offset, address) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(u32, value);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.load16_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i32.load16_s";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = self.popOperand(u32);

        const value = memory.read(i16, offset, address) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(i32, value);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.load16_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i32.load16_u";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = self.popOperand(u32);

        const value = memory.read(u16, offset, address) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(u32, value);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.load8_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i64.load8_s";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = self.popOperand(u32);

        const value = memory.read(i8, offset, address) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(i64, value);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.load8_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i64.load8_u";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = self.popOperand(u32);

        const value = memory.read(u8, offset, address) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(u64, value);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.load16_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i64.load16_s";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = self.popOperand(u32);

        const value = memory.read(i16, offset, address) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(i64, value);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.load16_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i64.load16_u";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = self.popOperand(u32);

        const value = memory.read(u16, offset, address) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(u64, value);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.load32_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i64.load32_s";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = self.popOperand(u32);

        const value = memory.read(i32, offset, address) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(i64, value);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.load32_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i64.load32_u";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = self.popOperand(u32);

        const value = memory.read(u32, offset, address) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(u64, value);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.store"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i32.store";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = self.popOperand(u32);
        const address = self.popOperand(u32);

        memory.write(u32, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.store"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i64.store";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = self.popOperand(u64);
        const address = self.popOperand(u32);

        memory.write(u64, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.store"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"f32.store";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = self.popOperand(f32);
        const address = self.popOperand(u32);

        memory.write(f32, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.store"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"f64.store";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = self.popOperand(f64);
        const address = self.popOperand(u32);

        memory.write(f64, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.store8"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i32.store8";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = @truncate(u8, self.popOperand(u32));
        const address = self.popOperand(u32);

        memory.write(u8, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.store16"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i32.store16";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = @truncate(u16, self.popOperand(u32));
        const address = self.popOperand(u32);

        memory.write(u16, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.store8"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i64.store8";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = @truncate(u8, self.popOperand(u64));
        const address = self.popOperand(u32);

        memory.write(u8, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.store16"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i64.store16";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = @truncate(u16, self.popOperand(u64));
        const address = self.popOperand(u32);

        memory.write(u16, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.store32"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const load_data = code[ip].@"i64.store32";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = @truncate(u32, self.popOperand(u64));
        const address = self.popOperand(u32);

        memory.write(u32, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"memory.size"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(u32, @intCast(u32, memory.data.items.len));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"memory.grow"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const num_pages = self.popOperand(u32);
        if (memory.grow(num_pages)) |old_size| {
            self.pushOperandNoCheck(u32, @intCast(u32, old_size));
        } else |_| {
            self.pushOperandNoCheck(i32, @as(i32, -1));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.const"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const instr = code[ip];

        self.pushOperandNoCheck(i32, instr.@"i32.const");

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.const"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const instr = code[ip];

        self.pushOperandNoCheck(i64, instr.@"i64.const");

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.const"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const instr = code[ip];

        self.pushOperandNoCheck(f32, instr.@"f32.const");

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.const"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const instr = code[ip];

        self.pushOperandNoCheck(f64, instr.@"f64.const");

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.eqz"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 == 0) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.eq"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 == c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.ne"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 != c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.lt_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(i32);
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 < c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.lt_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 < c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.gt_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(i32);
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 > c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.gt_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 > c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.le_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(i32);
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 <= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.le_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 <= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.ge_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(i32);
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 >= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.ge_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 >= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.eqz"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 == 0) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.eq"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 == c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.ne"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 != c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.lt_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(i64);
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 < c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.lt_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 < c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.gt_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(i64);
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 > c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.gt_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 > c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.le_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(i64);
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 <= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.le_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 <= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.ge_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(i64);
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 >= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.ge_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 >= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.eq"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 == c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.ne"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 != c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.lt"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 < c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.gt"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 > c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.le"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 <= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.ge"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 >= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.eq"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 == c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.ne"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 != c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.lt"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 < c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.gt"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 > c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.le"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 <= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.ge"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 >= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.clz"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(u32);
        self.pushOperandNoCheck(u32, @clz(u32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.ctz"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(u32);
        self.pushOperandNoCheck(u32, @ctz(u32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.popcnt"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(u32);
        self.pushOperandNoCheck(u32, @popCount(u32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.add"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, c1 +% c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.sub"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, c1 -% c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.mul"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, c1 *% c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.div_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(i32);
        const c1 = self.popOperand(i32);

        const div = math.divTrunc(i32, c1, c2) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(i32, div);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.div_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        const div = math.divTrunc(u32, c1, c2) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(u32, div);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.rem_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(i32);
        const c1 = self.popOperand(i32);

        const abs = math.absInt(c2) catch |e| {
            err.* = e;
            return;
        };

        const rem = math.rem(i32, c1, abs) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(i32, rem);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.rem_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        const rem = math.rem(u32, c1, c2) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(u32, rem);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.and"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, c1 & c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.or"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, c1 | c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.xor"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, c1 ^ c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.shl"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, math.shl(u32, c1, c2 % 32));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.shr_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(i32);
        const c1 = self.popOperand(i32);

        const mod = math.mod(i32, c2, 32) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(i32, math.shr(i32, c1, mod));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.shr_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, math.shr(u32, c1, c2 % 32));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.rotl"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, math.rotl(u32, c1, c2 % 32));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.rotr"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, math.rotr(u32, c1, c2 % 32));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.clz"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(u64);
        self.pushOperandNoCheck(u64, @clz(u64, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.ctz"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(u64);
        self.pushOperandNoCheck(u64, @ctz(u64, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.popcnt"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(u64);
        self.pushOperandNoCheck(u64, @popCount(u64, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.add"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, c1 +% c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.sub"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, c1 -% c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.mul"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, c1 *% c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.div_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(i64);
        const c1 = self.popOperand(i64);

        const div = math.divTrunc(i64, c1, c2) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(i64, div);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.div_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        const div = math.divTrunc(u64, c1, c2) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(u64, div);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.rem_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(i64);
        const c1 = self.popOperand(i64);

        const abs = math.absInt(c2) catch |e| {
            err.* = e;
            return;
        };

        const rem = math.rem(i64, c1, abs) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(i64, rem);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.rem_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        const rem = math.rem(u64, c1, c2) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(u64, rem);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.and"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, c1 & c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.or"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, c1 | c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.xor"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, c1 ^ c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.shl"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, math.shl(u64, c1, c2 % 64));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.shr_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(i64);
        const c1 = self.popOperand(i64);

        const mod = math.mod(i64, c2, 64) catch |e| {
            err.* = e;
            return;
        };

        self.pushOperandNoCheck(i64, math.shr(i64, c1, mod));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.shr_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, math.shr(u64, c1, c2 % 64));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.rotl"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, math.rotl(u64, c1, c2 % 64));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.rotr"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, math.rotr(u64, c1, c2 % 64));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.abs"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, math.fabs(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.neg"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, -c1);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.ceil"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, @ceil(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.floor"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, @floor(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.trunc"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, @trunc(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.nearest"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f32);
        const floor = @floor(c1);
        const ceil = @ceil(c1);

        if (ceil - c1 == c1 - floor) {
            if (@mod(ceil, 2) == 0) {
                self.pushOperandNoCheck(f32, ceil);
            } else {
                self.pushOperandNoCheck(f32, floor);
            }
        } else {
            self.pushOperandNoCheck(f32, @round(c1));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.sqrt"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, math.sqrt(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.add"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, c1 + c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.sub"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, c1 - c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.mul"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, c1 * c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.div"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, c1 / c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.min"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        if (math.isNan(c1)) {
            self.pushOperandNoCheck(f32, math.nan_f32);
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
        }
        if (math.isNan(c2)) {
            self.pushOperandNoCheck(f32, math.nan_f32);
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
        }

        if (c1 == 0.0 and c2 == 0.0) {
            if (math.signbit(c1)) {
                self.pushOperandNoCheck(f32, c1);
            } else {
                self.pushOperandNoCheck(f32, c2);
            }
        } else {
            self.pushOperandNoCheck(f32, math.min(c1, c2));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.max"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        if (math.isNan(c1)) {
            self.pushOperandNoCheck(f32, math.nan_f32);
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
        }
        if (math.isNan(c2)) {
            self.pushOperandNoCheck(f32, math.nan_f32);
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
        }

        if (c1 == 0.0 and c2 == 0.0) {
            if (math.signbit(c1)) {
                self.pushOperandNoCheck(f32, c2);
            } else {
                self.pushOperandNoCheck(f32, c1);
            }
        } else {
            self.pushOperandNoCheck(f32, math.max(c1, c2));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.copysign"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        if (math.signbit(c2)) {
            self.pushOperandNoCheck(f32, -math.fabs(c1));
        } else {
            self.pushOperandNoCheck(f32, math.fabs(c1));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.abs"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, math.fabs(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.neg"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, -c1);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.ceil"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, @ceil(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.floor"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, @floor(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.trunc"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, @trunc(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.nearest"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f64);
        const floor = @floor(c1);
        const ceil = @ceil(c1);

        if (ceil - c1 == c1 - floor) {
            if (@mod(ceil, 2) == 0) {
                self.pushOperandNoCheck(f64, ceil);
            } else {
                self.pushOperandNoCheck(f64, floor);
            }
        } else {
            self.pushOperandNoCheck(f64, @round(c1));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.sqrt"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, math.sqrt(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.add"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, c1 + c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.sub"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, c1 - c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.mul"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, c1 * c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.div"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, c1 / c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.min"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        if (math.isNan(c1)) {
            self.pushOperandNoCheck(f64, math.nan_f64);
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
        }
        if (math.isNan(c2)) {
            self.pushOperandNoCheck(f64, math.nan_f64);
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
        }

        if (c1 == 0.0 and c2 == 0.0) {
            if (math.signbit(c1)) {
                self.pushOperandNoCheck(f64, c1);
            } else {
                self.pushOperandNoCheck(f64, c2);
            }
        } else {
            self.pushOperandNoCheck(f64, math.min(c1, c2));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.max"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        if (math.isNan(c1)) {
            self.pushOperandNoCheck(f64, math.nan_f64);
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
        }
        if (math.isNan(c2)) {
            self.pushOperandNoCheck(f64, math.nan_f64);
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
        }

        if (c1 == 0.0 and c2 == 0.0) {
            if (math.signbit(c1)) {
                self.pushOperandNoCheck(f64, c2);
            } else {
                self.pushOperandNoCheck(f64, c1);
            }
        } else {
            self.pushOperandNoCheck(f64, math.max(c1, c2));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.copysign"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        if (math.signbit(c2)) {
            self.pushOperandNoCheck(f64, -math.fabs(c1));
        } else {
            self.pushOperandNoCheck(f64, math.fabs(c1));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.wrap_i64"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(i32, @truncate(i32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.trunc_f32_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f32);

        if (math.isNan(c1)) {
            err.* = error.InvalidConversion;
            return;
        }

        const trunc = @trunc(c1);

        if (trunc >= @intToFloat(f32, std.math.maxInt(i32))) {
            err.* = error.Overflow;
            return;
        }

        if (trunc < @intToFloat(f32, std.math.minInt(i32))) {
            err.* = error.Overflow;
            return;
        }

        self.pushOperandNoCheck(i32, @floatToInt(i32, trunc));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.trunc_f32_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f32);

        if (math.isNan(c1)) {
            err.* = error.InvalidConversion;
            return;
        }

        const trunc = @trunc(c1);

        if (trunc >= @intToFloat(f32, std.math.maxInt(u32))) {
            err.* = error.Overflow;
            return;
        }

        if (trunc < @intToFloat(f32, std.math.minInt(u32))) {
            err.* = error.Overflow;
            return;
        }

        self.pushOperandNoCheck(u32, @floatToInt(u32, trunc));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.trunc_f64_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f64);

        if (math.isNan(c1)) {
            err.* = error.InvalidConversion;
            return;
        }

        const trunc = @trunc(c1);

        if (trunc > @intToFloat(f64, std.math.maxInt(i32))) {
            err.* = error.Overflow;
            return;
        }

        if (trunc < @intToFloat(f64, std.math.minInt(i32))) {
            err.* = error.Overflow;
            return;
        }

        self.pushOperandNoCheck(i32, @floatToInt(i32, trunc));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.trunc_f64_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f64);

        if (math.isNan(c1)) {
            err.* = error.InvalidConversion;
            return;
        }

        const trunc = @trunc(c1);

        if (trunc > @intToFloat(f64, std.math.maxInt(u32))) {
            err.* = error.Overflow;
            return;
        }

        if (trunc < @intToFloat(f64, std.math.minInt(u32))) {
            err.* = error.Overflow;
            return;
        }

        self.pushOperandNoCheck(u32, @floatToInt(u32, trunc));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.extend_i32_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(i64, @truncate(i32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.extend_i32_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, @truncate(u32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.trunc_f32_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f32);

        if (math.isNan(c1)) {
            err.* = error.InvalidConversion;
            return;
        }

        const trunc = @trunc(c1);

        if (trunc >= @intToFloat(f32, std.math.maxInt(i64))) {
            err.* = error.Overflow;
            return;
        }

        if (trunc < @intToFloat(f32, std.math.minInt(i64))) {
            err.* = error.Overflow;
            return;
        }

        self.pushOperandNoCheck(i64, @floatToInt(i64, trunc));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.trunc_f32_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f32);

        if (math.isNan(c1)) {
            err.* = error.InvalidConversion;
            return;
        }

        const trunc = @trunc(c1);

        if (trunc >= @intToFloat(f32, std.math.maxInt(u64))) {
            err.* = error.Overflow;
            return;
        }

        if (trunc < @intToFloat(f32, std.math.minInt(u64))) {
            err.* = error.Overflow;
            return;
        }

        self.pushOperandNoCheck(u64, @floatToInt(u64, trunc));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.trunc_f64_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f64);

        if (math.isNan(c1)) {
            err.* = error.InvalidConversion;
            return;
        }

        const trunc = @trunc(c1);

        if (trunc >= @intToFloat(f64, std.math.maxInt(i64))) {
            err.* = error.Overflow;
            return;
        }

        if (trunc < @intToFloat(f64, std.math.minInt(i64))) {
            err.* = error.Overflow;
            return;
        }

        self.pushOperandNoCheck(i64, @floatToInt(i64, trunc));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.trunc_f64_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f64);

        if (math.isNan(c1)) {
            err.* = error.InvalidConversion;
            return;
        }

        const trunc = @trunc(c1);

        if (trunc >= @intToFloat(f64, std.math.maxInt(u64))) {
            err.* = error.Overflow;
            return;
        }

        if (trunc < @intToFloat(f64, std.math.minInt(u64))) {
            err.* = error.Overflow;
            return;
        }

        self.pushOperandNoCheck(u64, @floatToInt(u64, trunc));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.convert_i32_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(f32, @intToFloat(f32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.convert_i32_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(f32, @intToFloat(f32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.convert_i64_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(f32, @intToFloat(f32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.convert_i64_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(f32, @intToFloat(f32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.demote_f64"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f32, @floatCast(f32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.convert_i32_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(f64, @intToFloat(f64, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.convert_i32_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(f64, @intToFloat(f64, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.convert_i64_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(f64, @intToFloat(f64, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.convert_i64_u"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(f64, @intToFloat(f64, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.promote_f32"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f64, @floatCast(f64, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.reinterpret_f32"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(i32, @bitCast(i32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.reinterpret_f64"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(i64, @bitCast(i64, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f32.reinterpret_i32"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(f32, @bitCast(f32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"f64.reinterpret_i64"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(f64, @bitCast(f64, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.extend8_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(i32, @truncate(i8, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i32.extend16_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(i32, @truncate(i16, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.extend8_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(i64, @truncate(i8, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.extend16_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(i64, @truncate(i16, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn @"i64.extend32_s"(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(i64, @truncate(i32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
    }

    fn trunc_sat(self: *Interpreter, ip: usize, code: []Instruction, err: *?WasmError) void {
        const meta = code[ip].trunc_sat;

        switch (meta) {
            0 => {
                const c1 = self.popOperand(f32);
                const trunc = @trunc(c1);

                if (math.isNan(c1)) {
                    self.pushOperandNoCheck(i32, 0);
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }

                if (trunc >= @intToFloat(f32, std.math.maxInt(i32))) {
                    self.pushOperandNoCheck(i32, @bitCast(i32, @as(u32, 0x7fffffff)));
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }
                if (trunc < @intToFloat(f32, std.math.minInt(i32))) {
                    self.pushOperandNoCheck(i32, @bitCast(i32, @as(u32, 0x80000000)));
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }

                self.pushOperandNoCheck(i32, @floatToInt(i32, trunc));
                return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
            },
            1 => {
                const c1 = self.popOperand(f32);
                const trunc = @trunc(c1);

                if (math.isNan(c1)) {
                    self.pushOperandNoCheck(u32, 0);
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }

                if (trunc >= @intToFloat(f32, std.math.maxInt(u32))) {
                    self.pushOperandNoCheck(u32, @bitCast(u32, @as(u32, 0xffffffff)));
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }
                if (trunc < @intToFloat(f32, std.math.minInt(u32))) {
                    self.pushOperandNoCheck(u32, @bitCast(u32, @as(u32, 0x00000000)));
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }

                self.pushOperandNoCheck(u32, @floatToInt(u32, trunc));
                return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
            },
            2 => {
                const c1 = self.popOperand(f64);
                const trunc = @trunc(c1);

                if (math.isNan(c1)) {
                    self.pushOperandNoCheck(i32, 0);
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }

                if (trunc >= @intToFloat(f64, std.math.maxInt(i32))) {
                    self.pushOperandNoCheck(i32, @bitCast(i32, @as(u32, 0x7fffffff)));
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }
                if (trunc < @intToFloat(f64, std.math.minInt(i32))) {
                    self.pushOperandNoCheck(i32, @bitCast(i32, @as(u32, 0x80000000)));
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }

                self.pushOperandNoCheck(i32, @floatToInt(i32, trunc));
                return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
            },
            3 => {
                const c1 = self.popOperand(f64);
                const trunc = @trunc(c1);

                if (math.isNan(c1)) {
                    self.pushOperandNoCheck(u32, 0);
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }

                if (trunc >= @intToFloat(f64, std.math.maxInt(u32))) {
                    self.pushOperandNoCheck(u32, @bitCast(u32, @as(u32, 0xffffffff)));
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }
                if (trunc < @intToFloat(f64, std.math.minInt(u32))) {
                    self.pushOperandNoCheck(u32, @bitCast(u32, @as(u32, 0x00000000)));
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }

                self.pushOperandNoCheck(u32, @floatToInt(u32, trunc));
                return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
            },
            4 => {
                const c1 = self.popOperand(f32);
                const trunc = @trunc(c1);

                if (math.isNan(c1)) {
                    self.pushOperandNoCheck(i64, 0);
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }

                if (trunc >= @intToFloat(f32, std.math.maxInt(i64))) {
                    self.pushOperandNoCheck(i64, @bitCast(i64, @as(u64, 0x7fffffffffffffff)));
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }
                if (trunc < @intToFloat(f32, std.math.minInt(i64))) {
                    self.pushOperandNoCheck(i64, @bitCast(i64, @as(u64, 0x8000000000000000)));
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }

                self.pushOperandNoCheck(i64, @floatToInt(i64, trunc));
                return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
            },
            5 => {
                const c1 = self.popOperand(f32);
                const trunc = @trunc(c1);

                if (math.isNan(c1)) {
                    self.pushOperandNoCheck(u64, 0);
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }

                if (trunc >= @intToFloat(f32, std.math.maxInt(u64))) {
                    self.pushOperandNoCheck(u64, @bitCast(u64, @as(u64, 0xffffffffffffffff)));
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }
                if (trunc < @intToFloat(f32, std.math.minInt(u64))) {
                    self.pushOperandNoCheck(u64, @bitCast(u64, @as(u64, 0x0000000000000000)));
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }

                self.pushOperandNoCheck(u64, @floatToInt(u64, trunc));
                return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
            },
            6 => {
                const c1 = self.popOperand(f64);
                const trunc = @trunc(c1);

                if (math.isNan(c1)) {
                    self.pushOperandNoCheck(i64, 0);
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }

                if (trunc >= @intToFloat(f64, std.math.maxInt(i64))) {
                    self.pushOperandNoCheck(i64, @bitCast(i64, @as(u64, 0x7fffffffffffffff)));
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }
                if (trunc < @intToFloat(f64, std.math.minInt(i64))) {
                    self.pushOperandNoCheck(i64, @bitCast(i64, @as(u64, 0x8000000000000000)));
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }

                self.pushOperandNoCheck(i64, @floatToInt(i64, trunc));
                return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
            },
            7 => {
                const c1 = self.popOperand(f64);
                const trunc = @trunc(c1);

                if (math.isNan(c1)) {
                    self.pushOperandNoCheck(u64, 0);
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }

                if (trunc >= @intToFloat(f64, std.math.maxInt(u64))) {
                    self.pushOperandNoCheck(u64, @bitCast(u64, @as(u64, 0xffffffffffffffff)));
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }
                if (trunc < @intToFloat(f64, std.math.minInt(u64))) {
                    self.pushOperandNoCheck(u64, @bitCast(u64, @as(u64, 0x0000000000000000)));
                    return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
                }

                self.pushOperandNoCheck(u64, @floatToInt(u64, trunc));
                return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, err });
            },
            else => {
                err.* = error.Trap;
                return;
            },
        }
    }

    const InstructionFunction = fn (*Interpreter, usize, []Instruction, *?WasmError) void;

    const lookup = [256]InstructionFunction{
        @"unreachable",     nop,                block,                loop,                 @"if",                @"else",              if_no_else,        impl_ni,              impl_ni,              impl_ni,              impl_ni,              end,                br,                     br_if,                  br_table,               @"return",
        call,               call_indirect,      fast_call,            impl_ni,              impl_ni,              impl_ni,              impl_ni,           impl_ni,              impl_ni,              impl_ni,              drop,                 select,             impl_ni,                impl_ni,                impl_ni,                impl_ni,
        @"local.get",       @"local.set",       @"local.tee",         @"global.get",        @"global.set",        impl_ni,              impl_ni,           impl_ni,              @"i32.load",          @"i64.load",          @"f32.load",          @"f64.load",        @"i32.load8_s",         @"i32.load8_u",         @"i32.load16_s",        @"i32.load16_u",
        @"i64.load8_s",     @"i64.load8_u",     @"i64.load16_s",      @"i64.load16_u",      @"i64.load32_s",      @"i64.load32_u",      @"i32.store",      @"i64.store",         @"f32.store",         @"f64.store",         @"i32.store8",        @"i32.store16",     @"i64.store8",          @"i64.store16",         @"i64.store32",         @"memory.size",
        @"memory.grow",     @"i32.const",       @"i64.const",         @"f32.const",         @"f64.const",         @"i32.eqz",           @"i32.eq",         @"i32.ne",            @"i32.lt_s",          @"i32.lt_u",          @"i32.gt_s",          @"i32.gt_u",        @"i32.le_s",            @"i32.le_u",            @"i32.ge_s",            @"i32.ge_u",
        @"i64.eqz",         @"i64.eq",          @"i64.ne",            @"i64.lt_s",          @"i64.lt_u",          @"i64.gt_s",          @"i64.gt_u",       @"i64.le_s",          @"i64.le_u",          @"i64.ge_s",          @"i64.ge_u",          @"f32.eq",          @"f32.ne",              @"f32.lt",              @"f32.gt",              @"f32.le",
        @"f32.ge",          @"f64.eq",          @"f64.ne",            @"f64.lt",            @"f64.gt",            @"f64.le",            @"f64.ge",         @"i32.clz",           @"i32.ctz",           @"i32.popcnt",        @"i32.add",           @"i32.sub",         @"i32.mul",             @"i32.div_s",           @"i32.div_u",           @"i32.rem_s",
        @"i32.rem_u",       @"i32.and",         @"i32.or",            @"i32.xor",           @"i32.shl",           @"i32.shr_s",         @"i32.shr_u",      @"i32.rotl",          @"i32.rotr",          @"i64.clz",           @"i64.ctz",           @"i64.popcnt",      @"i64.add",             @"i64.sub",             @"i64.mul",             @"i64.div_s",
        @"i64.div_u",       @"i64.rem_s",       @"i64.rem_u",         @"i64.and",           @"i64.or",            @"i64.xor",           @"i64.shl",        @"i64.shr_s",         @"i64.shr_u",         @"i64.rotl",          @"i64.rotr",          @"f32.abs",         @"f32.neg",             @"f32.ceil",            @"f32.floor",           @"f32.trunc",
        @"f32.nearest",     @"f32.sqrt",        @"f32.add",           @"f32.sub",           @"f32.mul",           @"f32.div",           @"f32.min",        @"f32.max",           @"f32.copysign",      @"f64.abs",           @"f64.neg",           @"f64.ceil",        @"f64.floor",           @"f64.trunc",           @"f64.nearest",         @"f64.sqrt",
        @"f64.add",         @"f64.sub",         @"f64.mul",           @"f64.div",           @"f64.min",           @"f64.max",           @"f64.copysign",   @"i32.wrap_i64",      @"i32.trunc_f32_s",   @"i32.trunc_f32_u",   @"i32.trunc_f64_s",   @"i32.trunc_f64_u", @"i64.extend_i32_s",    @"i64.extend_i32_u",    @"i64.trunc_f32_s",     @"i64.trunc_f32_u",
        @"i64.trunc_f64_s", @"i64.trunc_f64_u", @"f32.convert_i32_s", @"f32.convert_i32_u", @"f32.convert_i64_s", @"f32.convert_i64_u", @"f32.demote_f64", @"f64.convert_i32_s", @"f64.convert_i32_u", @"f64.convert_i64_s", @"f64.convert_i64_u", @"f64.promote_f32", @"i32.reinterpret_f32", @"i64.reinterpret_f64", @"f32.reinterpret_i32", @"f64.reinterpret_i64",
        @"i32.extend8_s",   @"i32.extend16_s",  @"i64.extend8_s",     @"i64.extend16_s",    @"i64.extend32_s",    impl_ni,              impl_ni,           impl_ni,              impl_ni,              impl_ni,              impl_ni,              impl_ni,            impl_ni,                impl_ni,                impl_ni,                impl_ni,
        impl_ni,            impl_ni,            impl_ni,              impl_ni,              impl_ni,              impl_ni,              impl_ni,           impl_ni,              impl_ni,              impl_ni,              impl_ni,              impl_ni,            impl_ni,                impl_ni,                impl_ni,                impl_ni,
        impl_ni,            impl_ni,            impl_ni,              impl_ni,              impl_ni,              impl_ni,              impl_ni,           impl_ni,              impl_ni,              impl_ni,              impl_ni,              impl_ni,            impl_ni,                impl_ni,                impl_ni,                impl_ni,
        impl_ni,            impl_ni,            impl_ni,              impl_ni,              impl_ni,              impl_ni,              impl_ni,           impl_ni,              impl_ni,              impl_ni,              impl_ni,              impl_ni,            trunc_sat,              impl_ni,                impl_ni,                impl_ni,
    };

    pub fn invoke(self: *Interpreter, ip: usize) !void {
        const instr = self.inst.module.parsed_code.items[ip];

        var err: ?WasmError = null;
        @call(.{}, lookup[@enumToInt(instr)], .{ self, ip, self.inst.module.parsed_code.items, &err });
        if (err) |e| return e;
    }

    // https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-br-l
    pub fn branch(self: *Interpreter, target: u32) usize {
        const label = self.peekNthLabel(target);
        const n = label.return_arity;

        // var dest = self.op_stack[label.op_stack_len .. label.op_stack_len + n];
        // const src = self.op_stack[self.op_stack.len - n ..];

        var i: usize = 0;
        while (i < n) : (i += 1) {
            self.op_stack[label.op_stack_len + i] = self.op_stack[self.op_ptr - n + i];
        }

        // mem.copy(u64, dest, src);

        // self.op_stack = self.op_stack[0 .. label.op_stack_len + n];
        self.op_ptr = label.op_stack_len + n;
        _ = self.popLabels(target);

        return label.branch_target;
    }

    pub fn pushOperand(self: *Interpreter, comptime T: type, value: T) !void {
        if (self.op_ptr == self.op_stack.len) return error.OperandStackOverflow;

        self.op_ptr += 1;

        self.op_stack[self.op_ptr - 1] = switch (T) {
            i32 => @as(u64, @bitCast(u32, value)),
            i64 => @bitCast(u64, value),
            f32 => @as(u64, @bitCast(u32, value)),
            f64 => @bitCast(u64, value),
            u32 => @as(u64, value), // TODO: figure out types
            u64 => value,
            else => |t| @compileError("Unsupported operand type: " ++ @typeName(t)),
        };
    }

    pub fn checkStackSpace(self: *Interpreter, n: usize) !void {
        if (self.op_ptr + n > self.op_stack.len) return error.CheckStackSpace;
    }

    pub fn pushOperandNoCheck(self: *Interpreter, comptime T: type, value: T) void {
        self.op_ptr += 1;

        self.op_stack[self.op_ptr - 1] = switch (T) {
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
        defer self.op_ptr -= 1;

        const value = self.op_stack[self.op_ptr - 1];
        return switch (T) {
            i32 => @bitCast(i32, @truncate(u32, value)),
            i64 => @bitCast(i64, value),
            f32 => @bitCast(f32, @truncate(u32, value)),
            f64 => @bitCast(f64, value),
            u32 => @truncate(u32, value),
            u64 => value,
            else => |t| @compileError("Unsupported operand type: " ++ @typeName(t)),
        };
    }

    pub fn popAnyOperand(self: *Interpreter) u64 {
        defer self.op_ptr -= 1;

        return self.op_stack[self.op_ptr - 1];
    }

    fn peekOperand(self: *Interpreter) u64 {
        return self.op_stack[self.op_ptr - 1];
    }

    fn peekNthOperand(self: *Interpreter, index: u32) u64 {
        return self.op_stack[self.op_ptr - index - 1];
    }

    // TODO: if the code is validated, do we need to know the params count
    //       i.e. can we get rid of the dependency on params so that we don't
    //       have to lookup a function (necessarily)
    pub fn pushFrame(self: *Interpreter, frame: Frame, params_and_locals_count: usize) !void {
        if (self.frame_ptr == self.frame_stack.len) return error.ControlStackOverflow;
        self.frame_ptr += 1;

        const current_frame = &self.frame_stack[self.frame_ptr - 1];
        current_frame.* = frame;
        // TODO: index out of bounds (error if we've run out of operand stack space):
        current_frame.locals = self.op_stack[frame.op_stack_len .. frame.op_stack_len + params_and_locals_count];
    }

    pub fn popFrame(self: *Interpreter) Frame {
        defer self.frame_ptr -= 1;

        return self.frame_stack[self.frame_ptr - 1];
    }

    fn peekFrame(self: *Interpreter) *Frame {
        return &self.frame_stack[self.frame_ptr - 1];
    }

    pub fn pushLabel(self: *Interpreter, label: Label) !void {
        if (self.label_ptr == self.label_stack.len) return error.LabelStackOverflow;

        self.label_ptr += 1;
        const current_label = self.peekNthLabel(0);
        current_label.* = label;
    }

    pub fn popLabel(self: *Interpreter) Label {
        defer self.label_ptr -= 1;

        return self.label_stack[self.label_ptr - 1];
    }

    // peekNthLabel
    //
    // Returns nth label on the Label stack relative to the top of the stack
    //
    fn peekNthLabel(self: *Interpreter, index: u32) *Label {
        return &self.label_stack[self.label_ptr - index - 1];
    }

    // popLabels
    //
    // target: branch target (relative to current scope which is 0)
    //
    // popLabels pops labels up to and including `target`. Returns the
    // the label at `target`.
    pub fn popLabels(self: *Interpreter, target: u32) Label {
        defer self.label_ptr = self.label_ptr - target - 1;

        return self.label_stack[self.label_stack.len - target - 1];
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
        branch_target: usize = 0,
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

    try testing.expectEqual(@as(f64, 43.07), i.popOperand(f64));
    try testing.expectEqual(@as(f32, 22.07), i.popOperand(f32));
    try testing.expectEqual(@as(i64, -43), i.popOperand(i64));
    try testing.expectEqual(@as(i64, 44), i.popOperand(i64));
    try testing.expectEqual(@as(i32, -23), i.popOperand(i32));
    try testing.expectEqual(@as(i32, 22), i.popOperand(i32));
}

// TODO: reinstate this. I think we need to build up a valid instance with module + code
// test "simple interpret tests" {
//     var op_stack: [6]u64 = [_]u64{0} ** 6;
//     var frame_stack: [1024]Interpreter.Frame = [_]Interpreter.Frame{undefined} ** 1024;
//     var label_stack_mem: [1024]Interpreter.Label = [_]Interpreter.Label{undefined} ** 1024;

//     var inst: Instance = undefined;
//     var i = Interpreter.init(op_stack[0..], frame_stack[0..], label_stack_mem[0..], &inst);

//     try i.pushOperand(i32, 22);
//     try i.pushOperand(i32, -23);

//     var code = [_]Instruction{Instruction.@"i32.add"};

//     try i.invoke(0);

//     try testing.expectEqual(@as(i32, -1), i.popOperand(i32));
// }
