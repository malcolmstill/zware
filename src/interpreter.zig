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
    function_start: usize = 0,

    pub fn init(stack: []u64, inst: *Instance) Interpreter {
        return Interpreter{
            .stack = stack,
            .inst = inst,
        };
    }

    fn print_stack(stack: []u64, sp: usize) void {
        std.debug.warn("======= stack =======\n", .{});
        for (stack[0..sp]) |s, i| {
            std.debug.warn("{}:\t{x}\t{}\n", .{ i, s, s });
        }
        std.debug.warn("=====================\n\n", .{});
    }

    inline fn dispatch(self: *Interpreter, next_ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const next_instr = code[next_ip];

        // if (std.builtin.mode == .Debug) {
        //     std.debug.warn("next_ip = {}, fp = {}, lp = {}, sp = {}\n", .{ next_ip, self.fp, self.lp, sp });
        //     print_stack(stack, sp);
        //     const stdin = std.io.getStdIn().reader();
        //     const stdout = std.io.getStdOut().writer();

        //     var buf: [10]u8 = undefined;

        //     // stdout.print("Continue:", .{}) catch |e| return;
        //     // _ = stdin.readUntilDelimiterOrEof(buf[0..], '\n') catch |e| return;

        //     std.debug.warn("next instruction = {}\n", .{next_instr});
        // }

        return @call(.{ .modifier = .always_tail }, lookup[@enumToInt(next_instr)], .{ self, next_ip, code, sp, stack, err });
    }

    fn impl_ni(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        std.debug.warn("not implemented: {any}\n", .{code[ip]});
        err.* = error.NotImplemented;
    }

    fn @"unreachable"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        err.* = error.TrapUnreachable;
    }

    fn nop(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn block(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const meta = code[ip].block;
        std.debug.warn("block = {}\n", .{block});

        // 1. Check we have space
        if (sp + 3 >= stack.len) {
            err.* = error.StackOverflow;
            return;
        }

        // 2. Copy block.params_arity operands from where they are
        var i: usize = 0;
        while (i < meta.param_arity) : (i += 1) {
            stack[sp - meta.param_arity + i + 3] = stack[sp - meta.param_arity + i];
        }

        // 3. Insert label
        const new_lp = sp - meta.param_arity;
        _ = putLabelInternal(stack, self.lp, sp - meta.param_arity, meta.return_arity, meta.break_target);
        self.lp = new_lp;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp + 3, stack, err });
    }

    fn loop(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const meta = code[ip].loop;

        // 1. Check we have space
        if (sp + 3 >= stack.len) {
            err.* = error.StackOverflow;
            return;
        }

        // 2. Copy block.params_arity operands from where they are
        var i: usize = 0;
        while (i < meta.param_arity) : (i += 1) {
            stack[sp - meta.param_arity + i + 3] = stack[sp - meta.param_arity + i];
        }

        // 3. Insert label
        const new_lp = sp - meta.param_arity;
        _ = putLabelInternal(stack, self.lp, sp - meta.param_arity, meta.return_arity, meta.break_target);
        self.lp = new_lp;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp + 3, stack, err });
    }

    fn @"if"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const meta = code[ip].@"if";
        var next_ip = ip;
        var next_sp = sp;
        var next_lp = self.lp;

        const condition = stack[next_sp - 1];
        next_sp -= 1;

        if (condition == 0) {
            next_ip = meta.break_target;

            // unless we have an else branch
            if (meta.else_ip) |else_ip| {
                next_ip = else_ip;

                // We are inside the if branch
                const lp = self.lp;
                self.lp = sp - 1;
                next_sp = pushLabelInternal(stack, lp, next_sp, meta.return_arity, meta.break_target) catch |e| {
                    err.* = e;
                    return;
                };
            }
        } else {
            next_ip = ip + 1;
            const lp = self.lp;
            self.lp = sp - 1;
            next_sp = pushLabelInternal(stack, lp, next_sp, meta.return_arity, meta.break_target) catch |e| {
                err.* = e;
                return;
            };
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, code, next_sp, stack, err });
    }

    fn @"else"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        var next_ip = ip + 1;
        var next_sp = sp;

        const lp = self.lp;
        const label = peekLabel(stack, lp);
        const n = label.return_arity;

        // Copy return values
        var i: usize = 0;
        while (i < n) : (i += 1) {
            stack[lp + i] = stack[sp - n + i];
        }

        self.lp = label.previous_lp;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, label.break_target, code, lp + n, stack, err });
    }

    fn end(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        var next_ip = ip + 1;
        var next_sp = sp;

        const lp = self.lp;
        const label = peekLabel(stack, lp);
        const n = label.return_arity;

        // Copy return values
        var i: usize = 0;
        while (i < n) : (i += 1) {
            stack[lp + i] = stack[sp - n + i];
        }

        self.lp = label.previous_lp;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, code, lp + n, stack, err });
    }

    fn br(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const target = code[ip].br;

        const lp = self.lp;
        const lp_target = labelOffset(stack, lp, target);
        std.debug.warn("lp_target = {}\n", .{lp_target});
        const label = peekLabel(stack, lp_target);
        const n = label.return_arity;

        // Copy return values
        var i: usize = 0;
        while (i < n) : (i += 1) {
            stack[lp_target + i] = stack[sp - n + i];
        }

        self.lp = label.previous_lp;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, label.break_target, code, lp_target + n, stack, err });
    }

    fn br_if(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const condition = peekOperand(u32, stack, sp, 0);
        const new_sp = sp - 1;

        if (condition == 0) {
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, new_sp, stack, err });
        } else {
            const target = code[ip].br_if;

            const lp = self.lp;
            const lp_target = labelOffset(stack, lp, target);
            std.debug.warn("lp_target = {}\n", .{lp_target});
            const label = peekLabel(stack, lp_target);
            const n = label.return_arity;

            // Copy return values
            var i: usize = 0;
            while (i < n) : (i += 1) {
                stack[lp_target + i] = stack[new_sp - n + i];
            }

            self.lp = label.previous_lp;

            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, label.break_target, code, lp_target + n, stack, err });
        }
    }

    fn br_table(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const meta = code[ip].br_table;

        const j = peekOperand(u32, stack, sp, 0);
        const new_sp = sp - 1;
        const ls = self.inst.module.br_table_indices.items[meta.ls.offset .. meta.ls.offset + meta.ls.count];

        const target = if (j >= ls.len) meta.ln else ls[j];
        std.debug.warn("br_table target = {}\n", .{target});

        const lp = self.lp;
        const lp_target = labelOffset(stack, lp, target);
        const label = peekLabel(stack, lp_target);
        const n = label.return_arity;

        // Copy return values
        var i: usize = 0;
        while (i < n) : (i += 1) {
            stack[lp_target + i] = stack[new_sp - n + i];
        }

        self.lp = label.previous_lp;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, label.break_target, code, lp_target + n, stack, err });
    }

    fn @"return"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const frame = peekFrame(stack, self.fp);
        const label = peekLabel(stack, self.fp + 4);
        const n = label.return_arity;

        var i: usize = 0;
        while (i < n) : (i += 1) {
            stack[self.fp - frame.locals_count + i] = stack[sp - n + i];
        }

        const next_sp = self.fp - frame.locals_count + n;
        self.fp = frame.previous_fp;
        self.lp = label.previous_lp;
        const next_ip = label.break_target;

        if (frame.previous_fp == 0xFFFFFFFFFFFFFFFF) {
            self.sp = sp;
            return;
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, code, next_sp, stack, err });
    }

    fn call(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const instr = code[ip];

        const function_index = instr.call;
        const function = self.inst.getFunc(function_index) catch |e| {
            err.* = e;
            return;
        };
        var next_ip = ip;
        var next_sp = sp;

        switch (function) {
            .function => |f| {
                // self.pushNOperands(u64, f.locals_count, 0) catch |e| {
                //     err.* = e;
                //     return;
                // };
                next_sp += f.locals_count;

                // Consume parameters from the stack
                const previous_fp = self.fp;
                self.fp = next_sp;
                next_sp = pushFrameInternal(stack, previous_fp, next_sp, f.params.len + f.locals_count, f.results.len, self.inst) catch |e| {
                    err.* = e;
                    return;
                };

                // Our continuation is the code after call
                const next_lp = next_sp;
                next_sp = pushLabelInternal(stack, self.lp, next_sp, f.results.len, ip + 1) catch |e| {
                    err.* = e;
                    return;
                };
                self.lp = next_lp;
                next_ip = f.ip_start;

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

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, code, next_sp, stack, err });
    }

    fn call_indirect(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const instr = code[ip].call_indirect;
        const op_func_type_index = instr.@"type";
        const table_index = instr.table;

        // Read lookup index from stack
        const lookup_index = peekOperand(u32, stack, sp, 0);
        var next_sp = sp - 1;
        var next_ip = ip;

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

        switch (function) {
            .function => |f| {
                // Check that signatures match
                // const func_type = module.types.list.items[function.typeidx];
                const call_indirect_func_type = self.inst.module.types.list.items[op_func_type_index];
                if (!Module.signaturesEqual(f.params, f.results, call_indirect_func_type)) {
                    err.* = error.IndirectCallTypeMismatch;
                    return;
                }

                next_sp += f.locals_count;

                // Consume parameters from the stack
                const previous_fp = self.fp;
                self.fp = next_sp;
                next_sp = pushFrameInternal(stack, previous_fp, next_sp, f.params.len + f.locals_count, f.results.len, self.inst) catch |e| {
                    err.* = e;
                    return;
                };

                // Our continuation is the code after call
                next_sp = pushLabelInternal(stack, self.lp, next_sp, f.results.len, ip + 1) catch |e| {
                    err.* = e;
                    return;
                };

                next_ip = f.ip_start;

                self.inst = self.inst.store.instance(f.instance) catch |e| {
                    err.* = e;
                    return;
                };
            },
            .host_function => |host_func| {
                const call_indirect_func_type = self.inst.module.types.list.items[op_func_type_index];
                if (!Module.signaturesEqual(host_func.params, host_func.results, call_indirect_func_type)) {
                    err.* = error.IndirectCallTypeMismatch;
                    return;
                }

                host_func.func(self) catch |e| {
                    err.* = e;
                    return;
                };
            },
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, next_ip, code, next_sp, stack, err });
    }

    fn drop(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn select(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const condition = peekOperand(u32, stack, sp, 0);
        const c2 = peekOperand(u64, stack, sp, 1);
        const c1 = peekOperand(u64, stack, sp, 2);

        if (condition != 0) {
            putOperand(u64, stack, sp, 2, c1);
        } else {
            putOperand(u64, stack, sp, 2, c2);
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 2, stack, err });
    }

    fn @"local.get"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const local_index = code[ip].@"local.get";

        const frame = peekFrame(stack, self.fp);
        const local_value: u64 = stack[self.fp - frame.locals_count + local_index];

        _ = pushOperand3(u64, stack, sp, local_value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp + 1, stack, err });
    }

    fn @"local.set"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const local_index = code[ip].@"local.set";
        const frame = peekFrame(stack, self.fp);

        stack[self.fp - frame.locals_count + local_index] = peekOperand(u64, stack, sp, 0);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"local.tee"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const local_index = code[ip].@"local.tee";

        const value = peekOperand(u64, stack, sp, 0);
        const frame = peekFrame(stack, self.fp);

        stack[self.fp - frame.locals_count + local_index] = value;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"global.get"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const global_index = code[ip].@"global.get";

        const global = self.inst.getGlobal(global_index) catch |e| {
            err.* = e;
            return;
        };

        _ = pushOperand3(u64, stack, sp, global.value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp + 1, stack, err });
    }

    fn @"global.set"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const global_index = code[ip].@"global.set";
        const value = peekOperand(u64, stack, sp, 0);

        const global = self.inst.getGlobal(global_index) catch |e| {
            err.* = e;
            return;
        };

        global.value = value;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.load"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i32.load";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = peekOperand(u32, stack, sp, 0);

        const value = memory.read(u32, offset, address) catch |e| {
            err.* = e;
            return;
        };

        stack[sp - 1] = value;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i64.load"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i64.load";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = peekOperand(u32, stack, sp, 0);

        const value = memory.read(u64, offset, address) catch |e| {
            err.* = e;
            return;
        };

        stack[sp - 1] = value;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"f32.load"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"f32.load";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = peekOperand(u32, stack, sp, 0);

        const value = memory.read(f32, offset, address) catch |e| {
            err.* = e;
            return;
        };

        putOperand(f32, stack, sp, 0, value);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"f64.load"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"f64.load";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = peekOperand(u32, stack, sp, 0);

        const value = memory.read(f64, offset, address) catch |e| {
            err.* = e;
            return;
        };

        putOperand(f64, stack, sp, 0, value);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i32.load8_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i32.load8_s";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = peekOperand(u32, stack, sp, 0);

        const value = memory.read(u8, offset, address) catch |e| {
            err.* = e;
            return;
        };

        stack[sp - 1] = value;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i32.load8_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i32.load8_u";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = peekOperand(u32, stack, sp, 0);

        const value = memory.read(u8, offset, address) catch |e| {
            err.* = e;
            return;
        };

        stack[sp - 1] = value;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i32.load16_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i32.load16_s";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = peekOperand(u32, stack, sp, 0);

        const value = memory.read(u16, offset, address) catch |e| {
            err.* = e;
            return;
        };

        stack[sp - 1] = value;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i32.load16_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i32.load16_u";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = peekOperand(u32, stack, sp, 0);

        const value = memory.read(u16, offset, address) catch |e| {
            err.* = e;
            return;
        };

        stack[sp - 1] = value;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i64.load8_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i64.load8_s";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = peekOperand(u32, stack, sp, 0);

        const value = memory.read(u8, offset, address) catch |e| {
            err.* = e;
            return;
        };

        stack[sp - 1] = value;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i64.load8_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i64.load8_u";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = peekOperand(u32, stack, sp, 0);

        const value = memory.read(u8, offset, address) catch |e| {
            err.* = e;
            return;
        };

        stack[sp - 1] = value;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i64.load16_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i64.load16_s";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = peekOperand(u32, stack, sp, 0);

        const value = memory.read(u16, offset, address) catch |e| {
            err.* = e;
            return;
        };

        stack[sp - 1] = value;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i64.load16_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i64.load16_u";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = peekOperand(u32, stack, sp, 0);

        const value = memory.read(u16, offset, address) catch |e| {
            err.* = e;
            return;
        };

        stack[sp - 1] = value;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i64.load32_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i64.load32_s";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = peekOperand(u32, stack, sp, 0);

        const value = memory.read(u32, offset, address) catch |e| {
            err.* = e;
            return;
        };

        stack[sp - 1] = value;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i64.load32_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i64.load32_u";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const address = peekOperand(u32, stack, sp, 0);

        const value = memory.read(u32, offset, address) catch |e| {
            err.* = e;
            return;
        };

        stack[sp - 1] = value;

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i32.store"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i32.store";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = peekOperand(i32, stack, sp, 0);
        const address = peekOperand(u32, stack, sp, 1);

        memory.write(i32, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 2, stack, err });
    }

    fn @"i64.store"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i64.store";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = peekOperand(i64, stack, sp, 0);
        const address = peekOperand(u32, stack, sp, 1);

        memory.write(i64, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 2, stack, err });
    }

    fn @"f32.store"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"f32.store";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = peekOperand(f32, stack, sp, 0);
        const address = peekOperand(u32, stack, sp, 1);

        memory.write(f32, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 2, stack, err });
    }

    fn @"f64.store"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"f64.store";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = peekOperand(f64, stack, sp, 0);
        const address = peekOperand(u32, stack, sp, 1);

        memory.write(f64, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 2, stack, err });
    }

    fn @"i32.store8"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i32.store8";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = peekOperand(i32, stack, sp, 0);
        const address = peekOperand(u32, stack, sp, 1);

        memory.write(i32, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 2, stack, err });
    }

    fn @"i32.store16"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i32.store16";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = peekOperand(i32, stack, sp, 0);
        const address = peekOperand(u32, stack, sp, 1);

        memory.write(i32, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 2, stack, err });
    }

    fn @"i64.store8"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i64.store8";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = peekOperand(i64, stack, sp, 0);
        const address = peekOperand(u32, stack, sp, 1);

        memory.write(i64, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 2, stack, err });
    }

    fn @"i64.store16"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i64.store16";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = peekOperand(i64, stack, sp, 0);
        const address = peekOperand(u32, stack, sp, 1);

        memory.write(i64, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 2, stack, err });
    }

    fn @"i64.store32"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const load_data = code[ip].@"i64.store32";

        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const offset = load_data.offset;
        const value = peekOperand(i64, stack, sp, 0);
        const address = peekOperand(u32, stack, sp, 1);

        memory.write(i64, offset, address, value) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 2, stack, err });
    }

    fn @"memory.size"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        _ = pushOperand3(u32, stack, sp, @intCast(u32, memory.data.items.len)) catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 2, stack, err });
    }

    fn @"memory.grow"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const memory = self.inst.getMemory(0) catch |e| {
            err.* = e;
            return;
        };

        const num_pages = peekOperand(u32, stack, sp, 0);
        if (memory.grow(num_pages)) |old_size| {
            putOperand(u32, stack, sp, 0, @intCast(u32, old_size));
        } else |_| {
            putOperand(i32, stack, sp, 0, @as(i32, -1));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i32.const"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const instr = code[ip];

        _ = pushOperand3(i32, stack, sp, instr.@"i32.const") catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp + 1, stack, err });
    }

    fn @"i64.const"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const instr = code[ip];

        _ = pushOperand3(i64, stack, sp, instr.@"i64.const") catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp + 1, stack, err });
    }

    fn @"f32.const"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const instr = code[ip];

        _ = pushOperand3(f32, stack, sp, instr.@"f32.const") catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp + 1, stack, err });
    }

    fn @"f64.const"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const instr = code[ip];

        _ = pushOperand3(f64, stack, sp, instr.@"f64.const") catch |e| {
            err.* = e;
            return;
        };

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp + 1, stack, err });
    }

    fn @"i32.eqz"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(u32, stack, sp, 0);

        putOperand(u32, stack, sp, 0, @as(u32, if (c1 == 0) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i32.eq"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, @as(u32, if (c1 == c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.ne"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, @as(u32, if (c1 != c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.lt_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(i32, stack, sp, 0);
        const c1 = peekOperand(i32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, @as(u32, if (c1 < c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.lt_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, @as(u32, if (c1 < c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.gt_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(i32, stack, sp, 0);
        const c1 = peekOperand(i32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, @as(u32, if (c1 > c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.gt_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, @as(u32, if (c1 > c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.le_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(i32, stack, sp, 0);
        const c1 = peekOperand(i32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, @as(u32, if (c1 <= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.le_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, @as(u32, if (c1 <= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.ge_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(i32, stack, sp, 0);
        const c1 = peekOperand(i32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, @as(u32, if (c1 >= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.ge_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, @as(u32, if (c1 >= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.eqz"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(u64, stack, sp, 0);

        putOperand(u64, stack, sp, 0, @as(u64, if (c1 == 0) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i64.eq"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        putOperand(u64, stack, sp, 0, @as(u64, if (c1 == c1) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.ne"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        putOperand(u64, stack, sp, 0, @as(u64, if (c1 != c1) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.lt_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(i64, stack, sp, 0);
        const c1 = peekOperand(i64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 < c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.lt_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 < c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.gt_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(i64, stack, sp, 0);
        const c1 = peekOperand(i64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 > c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.gt_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 > c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.le_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(i64, stack, sp, 0);
        const c1 = peekOperand(i64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 <= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.le_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 <= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.ge_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(i64, stack, sp, 0);
        const c1 = peekOperand(i64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 >= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.ge_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 >= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f32.eq"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f32, stack, sp, 0);
        const c1 = peekOperand(f32, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 == c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f32.ne"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f32, stack, sp, 0);
        const c1 = peekOperand(f32, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 != c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f32.lt"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f32, stack, sp, 0);
        const c1 = peekOperand(f32, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 < c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f32.gt"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f32, stack, sp, 0);
        const c1 = peekOperand(f32, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 > c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f32.le"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f32, stack, sp, 0);
        const c1 = peekOperand(f32, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 <= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f32.ge"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f32, stack, sp, 0);
        const c1 = peekOperand(f32, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 >= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f64.eq"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f64, stack, sp, 0);
        const c1 = peekOperand(f64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 == c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f64.ne"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f64, stack, sp, 0);
        const c1 = peekOperand(f64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 != c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f64.lt"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f64, stack, sp, 0);
        const c1 = peekOperand(f64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 < c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f64.gt"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f64, stack, sp, 0);
        const c1 = peekOperand(f64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 > c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f64.le"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f64, stack, sp, 0);
        const c1 = peekOperand(f64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 <= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f64.ge"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f64, stack, sp, 0);
        const c1 = peekOperand(f64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, @as(u64, if (c1 >= c2) 1 else 0));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.clz"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(u32, stack, sp, 0);
        putOperand(u32, stack, sp, 0, @clz(u32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i32.ctz"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(u32, stack, sp, 0);
        putOperand(u32, stack, sp, 0, @ctz(u32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i32.popcnt"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(u32, stack, sp, 0);
        putOperand(u32, stack, sp, 0, @popCount(u32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i32.add"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, c1 +% c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.sub"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, c1 -% c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.mul"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, c1 *% c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.div_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(i32, stack, sp, 0);
        const c1 = peekOperand(i32, stack, sp, 1);

        const div = math.divTrunc(i32, c1, c2) catch |e| {
            err.* = e;
            return;
        };

        putOperand(i32, stack, sp, 1, div);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.div_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        const div = math.divTrunc(u32, c1, c2) catch |e| {
            err.* = e;
            return;
        };

        putOperand(u32, stack, sp, 1, div);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.rem_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(i32, stack, sp, 0);
        const c1 = peekOperand(i32, stack, sp, 1);

        const abs = math.absInt(c2) catch |e| {
            err.* = e;
            return;
        };

        const rem = math.rem(i32, c1, abs) catch |e| {
            err.* = e;
            return;
        };

        putOperand(i32, stack, sp, 1, rem);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.rem_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        const rem = math.rem(u32, c1, c2) catch |e| {
            err.* = e;
            return;
        };

        putOperand(u32, stack, sp, 1, rem);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.and"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, c1 & c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.or"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, c1 | c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.xor"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, c1 ^ c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.shl"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, math.shl(u32, c1, c2 % 32));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.shr_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(i32, stack, sp, 0);
        const c1 = peekOperand(i32, stack, sp, 1);

        const mod = math.mod(i32, c2, 32) catch |e| {
            err.* = e;
            return;
        };

        putOperand(i32, stack, sp, 1, math.shr(i32, c1, mod));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.shr_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, math.shr(u32, c1, c2 % 32));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.rotl"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, math.rotl(u32, c1, c2 % 32));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.rotr"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u32, stack, sp, 0);
        const c1 = peekOperand(u32, stack, sp, 1);

        putOperand(u32, stack, sp, 1, math.rotr(u32, c1, c2 % 32));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.clz"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(u64, stack, sp, 0);
        putOperand(u64, stack, sp, 0, @clz(u64, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i64.ctz"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(u64, stack, sp, 0);
        putOperand(u64, stack, sp, 0, @ctz(u64, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i64.popcnt"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(u64, stack, sp, 0);
        putOperand(u64, stack, sp, 0, @popCount(u64, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i64.add"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, c1 +% c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.sub"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, c1 -% c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.mul"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, c1 *% c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.div_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(i64, stack, sp, 0);
        const c1 = peekOperand(i64, stack, sp, 1);

        const div = math.divTrunc(i64, c1, c2) catch |e| {
            err.* = e;
            return;
        };

        putOperand(i64, stack, sp, 1, div);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.div_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        const div = math.divTrunc(u64, c1, c2) catch |e| {
            err.* = e;
            return;
        };

        putOperand(u64, stack, sp, 1, div);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.rem_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(i64, stack, sp, 0);
        const c1 = peekOperand(i64, stack, sp, 1);

        const abs = math.absInt(c2) catch |e| {
            err.* = e;
            return;
        };

        const rem = math.rem(i64, c1, abs) catch |e| {
            err.* = e;
            return;
        };

        putOperand(i64, stack, sp, 1, rem);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.rem_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        const rem = math.rem(u64, c1, c2) catch |e| {
            err.* = e;
            return;
        };

        putOperand(u64, stack, sp, 1, rem);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.and"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, c1 & c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.or"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, c1 | c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.xor"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, c1 ^ c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.shl"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, math.shl(u64, c1, c2 % 64));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.shr_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(i64, stack, sp, 0);
        const c1 = peekOperand(i64, stack, sp, 1);

        const mod = math.mod(i64, c2, 64) catch |e| {
            err.* = e;
            return;
        };

        putOperand(i64, stack, sp, 1, math.shr(i64, c1, mod));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.shr_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, math.shr(u64, c1, c2 % 64));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.rotl"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, math.rotl(u64, c1, c2 % 64));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i64.rotr"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(u64, stack, sp, 0);
        const c1 = peekOperand(u64, stack, sp, 1);

        putOperand(u64, stack, sp, 1, math.rotr(u64, c1, c2 % 64));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f32.abs"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f32, stack, sp, 0);

        putOperand(f32, stack, sp, 1, math.fabs(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"f32.neg"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f32, stack, sp, 0);

        putOperand(f32, stack, sp, 1, -c1);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"f32.ceil"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f32, stack, sp, 0);

        putOperand(f32, stack, sp, 1, @ceil(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"f32.floor"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f32, stack, sp, 0);

        putOperand(f32, stack, sp, 1, @floor(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"f32.trunc"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f32, stack, sp, 0);

        putOperand(f32, stack, sp, 1, @trunc(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"f32.nearest"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f32, stack, sp, 0);
        const floor = @floor(c1);
        const ceil = @ceil(c1);

        if (ceil - c1 == c1 - floor) {
            if (@mod(ceil, 2) == 0) {
                putOperand(f32, stack, sp, 1, ceil);
            } else {
                putOperand(f32, stack, sp, 1, floor);
            }
        } else {
            putOperand(f32, stack, sp, 1, @round(c1));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"f32.sqrt"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f32, stack, sp, 0);

        putOperand(f32, stack, sp, 1, math.sqrt(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"f32.add"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f32, stack, sp, 0);
        const c1 = peekOperand(f32, stack, sp, 1);

        putOperand(f32, stack, sp, 1, c1 + c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f32.sub"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f32, stack, sp, 0);
        const c1 = peekOperand(f32, stack, sp, 1);

        putOperand(f32, stack, sp, 1, c1 - c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f32.mul"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f32, stack, sp, 0);
        const c1 = peekOperand(f32, stack, sp, 1);

        putOperand(f32, stack, sp, 1, c1 * c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f32.div"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f32, stack, sp, 0);
        const c1 = peekOperand(f32, stack, sp, 1);

        putOperand(f32, stack, sp, 1, c1 / c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f32.min"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f32, stack, sp, 0);
        const c1 = peekOperand(f32, stack, sp, 1);

        if (math.isNan(c1)) {
            putOperand(f32, stack, sp, 1, math.nan_f32);
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
        }
        if (math.isNan(c2)) {
            putOperand(f32, stack, sp, 1, math.nan_f32);
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
        }

        if (c1 == 0.0 and c2 == 0.0) {
            if (math.signbit(c1)) {
                putOperand(f32, stack, sp, 1, c1);
            } else {
                putOperand(f32, stack, sp, 1, c2);
            }
        } else {
            putOperand(f32, stack, sp, 1, math.min(c1, c2));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f32.max"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f32, stack, sp, 0);
        const c1 = peekOperand(f32, stack, sp, 1);

        if (math.isNan(c1)) {
            putOperand(f32, stack, sp, 1, math.nan_f32);
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
        }
        if (math.isNan(c2)) {
            putOperand(f32, stack, sp, 1, math.nan_f32);
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
        }

        if (c1 == 0.0 and c2 == 0.0) {
            if (math.signbit(c1)) {
                putOperand(f32, stack, sp, 1, c2);
            } else {
                putOperand(f32, stack, sp, 1, c1);
            }
        } else {
            putOperand(f32, stack, sp, 1, math.max(c1, c2));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f32.copysign"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f32, stack, sp, 0);
        const c1 = peekOperand(f32, stack, sp, 1);

        if (math.signbit(c2)) {
            putOperand(f32, stack, sp, 1, -math.fabs(c1));
        } else {
            putOperand(f32, stack, sp, 1, math.fabs(c1));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f64.abs"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f64, stack, sp, 0);

        putOperand(f64, stack, sp, 1, math.fabs(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"f64.neg"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f64, stack, sp, 0);

        putOperand(f64, stack, sp, 1, -c1);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"f64.ceil"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f64, stack, sp, 0);

        putOperand(f64, stack, sp, 1, @ceil(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"f64.floor"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f64, stack, sp, 0);

        putOperand(f64, stack, sp, 1, @floor(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"f64.trunc"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f64, stack, sp, 0);

        putOperand(f64, stack, sp, 1, @trunc(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"f64.nearest"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f64, stack, sp, 0);
        const floor = @floor(c1);
        const ceil = @ceil(c1);

        if (ceil - c1 == c1 - floor) {
            if (@mod(ceil, 2) == 0) {
                putOperand(f64, stack, sp, 1, ceil);
            } else {
                putOperand(f64, stack, sp, 1, floor);
            }
        } else {
            putOperand(f64, stack, sp, 1, @round(c1));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"f64.sqrt"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f64, stack, sp, 0);

        putOperand(f64, stack, sp, 1, math.sqrt(c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"f64.add"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f64, stack, sp, 0);
        const c1 = peekOperand(f64, stack, sp, 1);

        putOperand(f64, stack, sp, 1, c1 + c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f64.sub"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f64, stack, sp, 0);
        const c1 = peekOperand(f64, stack, sp, 1);

        putOperand(f64, stack, sp, 1, c1 - c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f64.mul"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f64, stack, sp, 0);
        const c1 = peekOperand(f64, stack, sp, 1);

        putOperand(f64, stack, sp, 1, c1 * c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f64.div"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f64, stack, sp, 0);
        const c1 = peekOperand(f64, stack, sp, 1);

        putOperand(f64, stack, sp, 1, c1 / c2);

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f64.min"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f64, stack, sp, 0);
        const c1 = peekOperand(f64, stack, sp, 1);

        if (math.isNan(c1)) {
            putOperand(f64, stack, sp, 1, math.nan_f64);
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
        }
        if (math.isNan(c2)) {
            putOperand(f64, stack, sp, 1, math.nan_f64);
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
        }

        if (c1 == 0.0 and c2 == 0.0) {
            if (math.signbit(c1)) {
                putOperand(f64, stack, sp, 1, c1);
            } else {
                putOperand(f64, stack, sp, 1, c2);
            }
        } else {
            putOperand(f64, stack, sp, 1, math.min(c1, c2));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f64.max"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f64, stack, sp, 0);
        const c1 = peekOperand(f64, stack, sp, 1);

        if (math.isNan(c1)) {
            putOperand(f64, stack, sp, 1, math.nan_f64);
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
        }
        if (math.isNan(c2)) {
            putOperand(f64, stack, sp, 1, math.nan_f64);
            return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
        }

        if (c1 == 0.0 and c2 == 0.0) {
            if (math.signbit(c1)) {
                putOperand(f64, stack, sp, 1, c2);
            } else {
                putOperand(f64, stack, sp, 1, c1);
            }
        } else {
            putOperand(f64, stack, sp, 1, math.max(c1, c2));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"f64.copysign"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c2 = peekOperand(f64, stack, sp, 0);
        const c1 = peekOperand(f64, stack, sp, 1);

        if (math.signbit(c2)) {
            putOperand(f64, stack, sp, 1, -math.fabs(c1));
        } else {
            putOperand(f64, stack, sp, 1, math.fabs(c1));
        }

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp - 1, stack, err });
    }

    fn @"i32.wrap_i64"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(i64, stack, sp, 0);

        putOperand(i32, stack, sp, 0, @truncate(i32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i32.trunc_f32_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f32, stack, sp, 0);

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

        putOperand(i32, stack, sp, 0, @floatToInt(i32, trunc));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i32.trunc_f32_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f32, stack, sp, 0);

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

        putOperand(u32, stack, sp, 0, @floatToInt(u32, trunc));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i32.trunc_f64_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f64, stack, sp, 0);

        if (math.isNan(c1)) {
            err.* = error.InvalidConversion;
            return;
        }

        const trunc = @trunc(c1);

        if (trunc >= @intToFloat(f64, std.math.maxInt(i32))) {
            err.* = error.Overflow;
            return;
        }

        if (trunc < @intToFloat(f64, std.math.minInt(i32))) {
            err.* = error.Overflow;
            return;
        }

        putOperand(i32, stack, sp, 0, @floatToInt(i32, trunc));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i32.trunc_f64_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f64, stack, sp, 0);

        if (math.isNan(c1)) {
            err.* = error.InvalidConversion;
            return;
        }

        const trunc = @trunc(c1);

        if (trunc >= @intToFloat(f64, std.math.maxInt(u32))) {
            err.* = error.Overflow;
            return;
        }

        if (trunc < @intToFloat(f64, std.math.minInt(u32))) {
            err.* = error.Overflow;
            return;
        }

        putOperand(u32, stack, sp, 0, @floatToInt(u32, trunc));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i64.extend_i32_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(i64, stack, sp, 0);

        putOperand(i64, stack, sp, 0, @truncate(i32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i64.extend_i32_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(u64, stack, sp, 0);

        putOperand(u64, stack, sp, 0, @truncate(u32, c1));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i64.trunc_f32_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f32, stack, sp, 0);

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

        putOperand(i64, stack, sp, 0, @floatToInt(i64, trunc));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i64.trunc_f32_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f32, stack, sp, 0);

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

        putOperand(u64, stack, sp, 0, @floatToInt(u64, trunc));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i64.trunc_f64_s"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f64, stack, sp, 0);

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

        putOperand(i64, stack, sp, 0, @floatToInt(i64, trunc));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    fn @"i64.trunc_f64_u"(self: *Interpreter, ip: usize, code: []Instruction, sp: usize, stack: []u64, err: *?WasmError) void {
        const c1 = peekOperand(f64, stack, sp, 0);

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

        putOperand(u64, stack, sp, 0, @floatToInt(u64, trunc));

        return @call(.{ .modifier = .always_tail }, dispatch, .{ self, ip + 1, code, sp, stack, err });
    }

    const InstructionFunction = fn (*Interpreter, usize, []Instruction, usize, []u64, *?WasmError) void;

    const lookup = [256]InstructionFunction{
        @"unreachable",     nop,                block,                loop,                 @"if",                @"else",              impl_ni,           impl_ni,              impl_ni,              impl_ni,              impl_ni,              end,                br,                     br_if,                  br_table,               @"return",
        call,               call_indirect,      impl_ni,              impl_ni,              impl_ni,              impl_ni,              impl_ni,           impl_ni,              impl_ni,              impl_ni,              drop,                 select,             impl_ni,                impl_ni,                impl_ni,                impl_ni,
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
        std.debug.warn("INVOKE, ip = {}\n", .{ip});
        const instr = self.inst.module.parsed_code.items[ip];
        var err: ?WasmError = null;

        // std.debug.warn("fp = {}, lp = {}, sp = {}\n", .{ self.fp, self.lp, self.sp });
        // print_stack(self.stack, self.sp);

        @call(.{}, lookup[@enumToInt(instr)], .{ self, ip, self.inst.module.parsed_code.items, self.sp, self.stack, &err });
        if (err) |e| return e;
    }

    // https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-br-l
    pub fn branch(stack: []u64, sp: usize, lp: usize, target: u32) void {
        const lp_target = labelOffset(stack, lp, target);
        const label = peekLabel(stack, lp);
        const n = label.return_arity;

        // var dest = self.op_stack[label.op_stack_len .. label.op_stack_len + n];
        // const src = self.op_stack[self.op_stack.len - n ..];

        // mem.copy(u64, dest, src);
        var i: usize = 0;
        while (i < n) : (i += 1) {
            stack[lp + i] = stack[sp - n + i];
        }

        // self.op_stack = self.op_stack[0 .. label.op_stack_len + n];
        // _ = self.popLabels(target);

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

    pub fn pushOperand4(comptime T: type, stack: []u64, sp: usize, value: T) usize {
        // if (sp == stack.len) return error.StackOverflow;

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

    pub fn popOperand(self: *Interpreter, comptime T: type) WasmError!T {
        // TODO: if we've validated the wasm, do we need to perform this check:
        if (self.stack.len == 0) return error.StackUnderflow;
        // defer self.op_stack = self.op_stack[0 .. self.op_stack.len - 1];
        defer self.sp -= 1;

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

    // fn peekNthOperand(self: *Interpreter, index: u32) u64 {
    // return self.op_stack[self.op_stack.len - index - 1];
    // }

    pub fn peekOperand(comptime T: type, stack: []u64, sp: usize, n: usize) T {
        const value = stack[sp - 1 - n];
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

    pub fn putOperand(comptime T: type, stack: []u64, sp: usize, n: usize, value: T) void {
        stack[sp - 1 - n] = switch (T) {
            i32 => @as(u64, @bitCast(u32, value)),
            i64 => @bitCast(u64, value),
            f32 => @as(u64, @bitCast(u32, value)),
            f64 => @bitCast(u64, value),
            u32 => @as(u64, value), // TODO: figure out types
            u64 => value,
            else => |t| @compileError("Unsupported operand type: " ++ @typeName(t)),
        };
    }

    pub fn pushFrame(self: *Interpreter, locals_count: usize, return_arity: usize, inst: *Instance) !void {
        self.fp = self.sp;
        self.sp = try pushFrameInternal(self.stack, 0xFFFFFFFFFFFFFFFF, self.sp, locals_count, return_arity, inst);
    }

    fn pushFrameInternal(stack: []u64, fp: usize, sp: usize, locals_count: usize, return_arity: usize, inst: *Instance) !usize {
        if (sp + 4 > stack.len) return error.StackOverflow;
        stack[sp] = fp;
        stack[sp + 1] = locals_count;
        stack[sp + 2] = return_arity;
        stack[sp + 3] = @ptrToInt(inst);

        return sp + 4;
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

    pub fn pushLabel(self: *Interpreter, return_arity: usize, break_target: usize) !void {
        const new_lp = self.sp;
        self.sp = try pushLabelInternal(self.stack, self.lp, self.sp, return_arity, break_target);
        self.lp = new_lp;
    }

    pub fn pushLabelInternal(stack: []u64, lp: usize, sp: usize, return_arity: usize, break_target: usize) !usize {
        if (sp + 3 > stack.len) return error.StackOverflow;
        stack[sp] = lp;
        stack[sp + 1] = return_arity;
        stack[sp + 2] = break_target;

        return sp + 3;
    }

    pub fn putLabelInternal(stack: []u64, lp: usize, sp: usize, return_arity: usize, break_target: usize) usize {
        stack[sp] = lp;
        stack[sp + 1] = return_arity;
        stack[sp + 2] = break_target;

        return sp + 3;
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
            .break_target = stack[lp + 2],
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

    fn labelOffset(stack: []u64, lp: usize, target: usize) usize {
        if (target == 0) return lp;

        var previous_lp = peekLabel(stack, lp).previous_lp;
        var i: usize = 1;
        while (i < target) : (i += 1) {
            previous_lp = peekLabel(stack, previous_lp).previous_lp;
        }

        return previous_lp;
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
        break_target: usize,
    };
};
