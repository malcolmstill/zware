const std = @import("std");
const os = std.os;
const mem = std.mem;
const math = std.math;
const wasi = std.os.wasi;
const ArrayList = std.ArrayList;
const Module = @import("../module.zig").Module;
const ValType = @import("../module.zig").ValType;
const Instance = @import("../instance.zig").Instance;
const WasiPreopen = @import("../instance.zig").WasiPreopen;
const Rr = @import("../rr.zig").Rr;

// VirtualMachine:
//
// The VirtualMachine interprets WebAssembly bytecode, i.e. it
// is the engine of execution and the whole reason we're here.
//
// Whilst executing code, the VirtualMachine maintains three stacks.
// An operand stack, a control stack and a label stack.
// The WebAssembly spec models execution as a single stack where operands,
// activation frames, and labels are all interleaved. Here we split
// those out for convenience.
//
// Note: I had considered four stacks (separating out the params / locals) to
// there own stack, but I don't think that's necessary.
//
pub const VirtualMachine = struct {
    op_stack: []u64 = undefined,
    op_ptr: usize = 0,
    frame_stack: []Frame = undefined,
    frame_ptr: usize = 0,
    label_stack: []Label = undefined,
    label_ptr: usize = 0,

    inst: *Instance = undefined,
    ip: usize = 0,

    // wasi support
    //
    // These fields match the types in Instance but are
    // instead pointers. These will point to the Instance
    // that initialises the VirtualMachine
    wasi_preopens: *std.AutoHashMap(wasi.fd_t, WasiPreopen),
    wasi_args: *std.ArrayList([:0]u8),
    wasi_env: *std.StringHashMap([]const u8),

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

    pub fn init(op_stack: []u64, frame_stack: []Frame, label_stack: []Label, inst: *Instance) VirtualMachine {
        return VirtualMachine{
            .op_stack = op_stack,
            .frame_stack = frame_stack,
            .label_stack = label_stack,
            .inst = inst,
            .wasi_preopens = &inst.wasi_preopens,
            .wasi_args = &inst.wasi_args,
            .wasi_env = &inst.wasi_env,
        };
    }

    pub fn lookupWasiPreopen(self: *VirtualMachine, wasi_fd: os.wasi.fd_t) ?WasiPreopen {
        return self.wasi_preopens.get(wasi_fd);
    }

    pub fn getHostFd(self: *VirtualMachine, wasi_fd: wasi.fd_t) os.fd_t {
        const preopen = self.lookupWasiPreopen(wasi_fd) orelse return wasi_fd;

        return preopen.host_fd;
    }

    pub fn invoke(self: *VirtualMachine, ip: usize) !void {
        const instr = self.inst.module.instructions.items[ip];
        const imm = self.inst.module.immediates_offset.items[ip];

        try @call(.auto, instr, .{ self, ip, imm, @as([]Instruction, @ptrCast(self.inst.module.instructions.items)), self.inst.module.immediates.items });
    }

    // To avoid a recursive definition, define similar function pointer type we will cast to / from
    pub const Instruction = *const fn (*VirtualMachine, usize, usize, []*void, []u32) WasmError!void;
    pub const InstructionFunction = *const fn (*VirtualMachine, usize, usize, []Instruction, []u32) WasmError!void;

    pub const lookup = [256]InstructionFunction{
        @"unreachable",     nop,                block,                loop,                 @"if",                @"else",              if_with_else,      impl_ni,              impl_ni,              impl_ni,              impl_ni,              end,                br,                     br_if,                  br_table,               @"return",
        call,               call_indirect,      fast_call,            impl_ni,              impl_ni,              impl_ni,              impl_ni,           impl_ni,              impl_ni,              impl_ni,              drop,                 select,             select,                 impl_ni,                impl_ni,                impl_ni,
        @"local.get",       @"local.set",       @"local.tee",         @"global.get",        @"global.set",        @"table.get",         @"table.set",      impl_ni,              @"i32.load",          @"i64.load",          @"f32.load",          @"f64.load",        @"i32.load8_s",         @"i32.load8_u",         @"i32.load16_s",        @"i32.load16_u",
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
        @"ref.null",        @"ref.is_null",     @"ref.func",          impl_ni,              impl_ni,              impl_ni,              impl_ni,           impl_ni,              impl_ni,              impl_ni,              impl_ni,              impl_ni,            impl_ni,                impl_ni,                impl_ni,                impl_ni,
        impl_ni,            impl_ni,            impl_ni,              impl_ni,              impl_ni,              impl_ni,              impl_ni,           impl_ni,              impl_ni,              impl_ni,              impl_ni,              impl_ni,            impl_ni,                impl_ni,                impl_ni,                impl_ni,
        impl_ni,            impl_ni,            impl_ni,              impl_ni,              impl_ni,              impl_ni,              impl_ni,           impl_ni,              impl_ni,              impl_ni,              impl_ni,              impl_ni,            misc,                   impl_ni,                impl_ni,                impl_ni,
    };

    inline fn dispatch(self: *VirtualMachine, next_ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const next_fn = instructions[next_ip];

        return try @call(.always_tail, @as(InstructionFunction, @ptrCast(next_fn)), .{ self, next_ip, imm, instructions, immediates });
    }

    pub const REF_NULL: u64 = 0xFFFF_FFFF_FFFF_FFFF;

    pub fn impl_ni(_: *VirtualMachine, _: usize, _: usize, _: []Instruction, _: []u32) WasmError!void {
        return error.NotImplemented;
    }

    pub fn @"unreachable"(_: *VirtualMachine, _: usize, _: usize, _: []Instruction, _: []u32) WasmError!void {
        return error.TrapUnreachable;
    }

    pub fn nop(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn block(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        // const meta = code[ip].block;
        const param_arity = immediates[imm];
        const return_arity = immediates[imm + 1];
        const branch_target = immediates[imm + 2];

        try self.pushLabel(Label{
            .return_arity = return_arity,
            .op_stack_len = self.op_ptr - param_arity,
            .branch_target = branch_target,
        });

        return dispatch(self, ip + 1, imm + 3, instructions, immediates);
    }

    pub fn loop(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const param_arity = immediates[imm];
        const return_arity = immediates[imm + 1];
        _ = return_arity;
        const branch_target = immediates[imm + 2];

        try self.pushLabel(Label{
            // note that we use block_params rather than block_returns for return arity:
            .return_arity = param_arity,
            .op_stack_len = self.op_ptr - param_arity,
            .branch_target = branch_target,
        });

        return dispatch(self, ip + 1, imm + 3, instructions, immediates);
    }

    pub fn if_with_else(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const param_arity = immediates[imm];
        const return_arity = immediates[imm + 1];
        const branch_target = immediates[imm + 2];
        const else_ip = immediates[imm + 3];

        const condition = self.popOperand(u32);

        try self.pushLabel(Label{
            .return_arity = return_arity,
            .op_stack_len = self.op_ptr - param_arity,
            .branch_target = branch_target,
        });

        return dispatch(self, if (condition == 0) else_ip else ip + 1, imm + 4, instructions, immediates);
    }

    pub fn @"else"(self: *VirtualMachine, _: usize, _: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const label = self.popLabel();

        return dispatch(self, label.branch_target, @panic("lookup offset array"), instructions, immediates);
    }

    // if_no_else
    pub fn @"if"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        // const meta = code[ip].if_no_else;
        const param_arity = immediates[imm];
        const return_arity = immediates[imm + 1];
        const branch_target = immediates[imm + 2];
        _ = immediates[imm + 3];

        const condition = self.popOperand(u32);

        if (condition == 0) {
            const next_imm = self.inst.module.immediates_offset.items[branch_target];
            return dispatch(self, branch_target, next_imm, instructions, immediates);
        } else {
            // We are inside the if branch
            try self.pushLabel(Label{
                .return_arity = return_arity,
                .op_stack_len = self.op_ptr - param_arity,
                .branch_target = branch_target,
            });

            return dispatch(self, ip + 1, imm + 4, instructions, immediates);
        }
    }

    pub fn end(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        _ = self.popLabel();

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn br(self: *VirtualMachine, _: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const next_ip = self.branch(immediates[imm]);

        return dispatch(self, next_ip, imm + 1, instructions, immediates);
    }

    pub fn br_if(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const condition = self.popOperand(u32);

        const next_ip = if (condition == 0) ip + 1 else self.branch(immediates[imm]);
        const next_offset = if (condition == 0) imm + 1 else self.branch(@panic("Lookup offset for new instruction"));

        return dispatch(self, next_ip, next_offset, instructions, immediates);
    }

    pub fn br_table(self: *VirtualMachine, _: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const ls_ptr = immediates[imm];
        const ls_len = immediates[imm + 1];
        const ln = immediates[imm + 2];

        const i = self.popOperand(u32);
        const ls = self.inst.module.br_table_indices.items[ls_ptr .. ls_ptr + ls_len];

        const next_ip = if (i >= ls.len) self.branch(ln) else self.branch(ls[i]);
        const next_imm = if (true) @panic("Lookup correct offset");

        return dispatch(self, next_ip, next_imm, instructions, immediates);
    }

    pub fn @"return"(self: *VirtualMachine, _: usize, _: usize, _: []Instruction, immediates: []u32) WasmError!void {
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

        // FIXME: probably reference previous frame
        const branch_target_immediate_offset = self.inst.module.immediates_offset.items[label.branch_target];
        return dispatch(self, label.branch_target, branch_target_immediate_offset, @as([]Instruction, @ptrCast(previous_frame.inst.module.instructions.items)), immediates);
    }

    pub fn call(self: *VirtualMachine, ip: usize, imm: usize, _: []Instruction, immediates: []u32) WasmError!void {
        // const funcidx = code[ip].call;
        const funcidx = immediates[imm];

        const function = try self.inst.getFunc(funcidx);
        var next_ip = ip;
        var next_imm = imm;

        switch (function.subtype) {
            .function => |f| {
                // Check we have enough stack space
                try self.checkStackSpace(f.required_stack_space + f.locals_count);

                // Make space for locals (again, params already on stack)
                self.op_ptr += f.locals_count;

                self.inst = f.instance;

                // Consume parameters from the stack
                try self.pushFrame(Frame{
                    .op_stack_len = self.op_ptr - function.params.len - f.locals_count,
                    .label_stack_len = self.label_ptr,
                    .return_arity = function.results.len,
                    .inst = self.inst,
                }, f.locals_count + function.params.len);

                // Our continuation is the code after call
                try self.pushLabel(Label{
                    .return_arity = function.results.len,
                    .op_stack_len = self.op_ptr - function.params.len - f.locals_count,
                    .branch_target = ip + 1,
                });

                next_ip = f.start;
                next_imm = self.inst.module.immediates_offset.items[f.start];
            },
            .host_function => |hf| {
                try hf.func(self);
                next_ip = ip + 1;
                next_imm = imm + 1;
            },
        }

        // FIXME:
        return dispatch(self, next_ip, next_imm, @as([]Instruction, @ptrCast(self.inst.module.instructions.items)), immediates);
    }

    pub fn call_indirect(self: *VirtualMachine, ip: usize, imm: usize, _: []Instruction, immediates: []u32) WasmError!void {
        // const call_indirect_instruction = code[ip].call_indirect;
        var module = self.inst.module;

        const typeidx = immediates[imm];
        const tableidx = immediates[imm + 1];

        // Read lookup index from stack
        const lookup_index = self.popOperand(u32);
        const table = try self.inst.getTable(tableidx);
        const funcaddr = try table.lookup(lookup_index);
        const function = try self.inst.store.function(funcaddr);

        // Check that signatures match
        const call_indirect_func_type = module.types.list.items[typeidx];
        try function.checkSignatures(call_indirect_func_type);

        var next_ip = ip;
        var next_imm = imm;

        switch (function.subtype) {
            .function => |func| {
                // Check we have enough stack space
                try self.checkStackSpace(func.required_stack_space + func.locals_count);

                // Make space for locals (again, params already on stack)
                self.op_ptr += func.locals_count;

                self.inst = func.instance;

                // Consume parameters from the stack
                try self.pushFrame(Frame{
                    .op_stack_len = self.op_ptr - function.params.len - func.locals_count,
                    .label_stack_len = self.label_ptr,
                    .return_arity = function.results.len,
                    .inst = self.inst,
                }, func.locals_count + function.params.len);

                // Our continuation is the code after call
                try self.pushLabel(Label{
                    .return_arity = function.results.len,
                    .op_stack_len = self.op_ptr - function.params.len - func.locals_count,
                    .branch_target = ip + 1,
                });

                next_ip = func.start;
            },
            .host_function => |host_func| {
                try host_func.func(self);

                next_ip = ip + 1;
                next_imm = imm + 1;
            },
        }

        return dispatch(self, next_ip, next_imm, @as([]Instruction, @ptrCast(self.inst.module.instructions.items)), immediates);
    }

    pub fn fast_call(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        // const f = code[ip].fast_call;
        const start = immediates[imm];
        const locals = immediates[imm + 1];
        const params = immediates[imm + 2];
        const results = immediates[imm + 3];
        const required_stack_space = immediates[imm + 4];

        // Check we have enough stack space
        try self.checkStackSpace(required_stack_space + locals);

        // Make space for locals (again, params already on stack)
        self.op_ptr += locals;

        // Consume parameters from the stack
        try self.pushFrame(Frame{
            .op_stack_len = self.op_ptr - params - locals,
            .label_stack_len = self.label_ptr,
            .return_arity = results,
            .inst = self.inst,
        }, locals + params);

        // Our continuation is the code after call
        try self.pushLabel(Label{
            .return_arity = results,
            .op_stack_len = self.op_ptr - params - locals,
            .branch_target = ip + 1,
        });

        return dispatch(self, start, imm + 5, instructions, immediates);
    }

    pub fn drop(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        _ = self.popAnyOperand();
        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn select(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const condition = self.popOperand(u32);
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        if (condition != 0) {
            self.pushOperandNoCheck(u64, c1);
        } else {
            self.pushOperandNoCheck(u64, c2);
        }

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"local.get"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const localidx = immediates[imm];

        const frame = self.peekFrame();

        self.pushOperandNoCheck(u64, frame.locals[localidx]);

        return dispatch(self, ip + 1, imm + 1, instructions, immediates);
    }

    pub fn @"local.set"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const localidx = immediates[imm];

        const frame = self.peekFrame();
        frame.locals[localidx] = self.popOperand(u64);

        return dispatch(self, ip + 1, imm + 1, instructions, immediates);
    }

    pub fn @"local.tee"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const localidx = immediates[imm];

        const frame = self.peekFrame();
        frame.locals[localidx] = self.peekOperand();

        return dispatch(self, ip + 1, imm + 1, instructions, immediates);
    }

    pub fn @"global.get"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const globalidx = immediates[imm];

        const global = try self.inst.getGlobal(globalidx);

        self.pushOperandNoCheck(u64, global.value);

        return dispatch(self, ip + 1, imm + 1, instructions, immediates);
    }

    pub fn @"global.set"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const globalidx = immediates[imm];
        const value = self.popAnyOperand();

        const global = try self.inst.getGlobal(globalidx);

        global.value = value;

        return dispatch(self, ip + 1, imm + 1, instructions, immediates);
    }

    pub fn @"table.get"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const tableidx = immediates[imm];
        const table = try self.inst.getTable(tableidx);

        const index = self.popOperand(u32);
        const ref = try table.get(index);

        if (ref) |r| {
            self.pushOperandNoCheck(u64, r);
        } else {
            self.pushOperandNoCheck(u64, REF_NULL);
        }

        return dispatch(self, ip + 1, imm + 1, instructions, immediates);
    }

    pub fn @"table.set"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const tableidx = immediates[imm];
        const table = try self.inst.getTable(tableidx);

        const ref = self.popOperand(u64);
        const index = self.popOperand(u32);

        try table.set(index, ref);

        return dispatch(self, ip + 1, imm + 1, instructions, immediates);
    }

    pub fn @"i32.load"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const address = self.popOperand(u32);
        const value = try memory.read(u32, offset, address);

        self.pushOperandNoCheck(u32, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i64.load"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const address = self.popOperand(u32);
        const value = try memory.read(u64, offset, address);

        self.pushOperandNoCheck(u64, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"f32.load"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const address = self.popOperand(u32);
        const value = try memory.read(f32, offset, address);

        self.pushOperandNoCheck(f32, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"f64.load"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const address = self.popOperand(u32);
        const value = try memory.read(f64, offset, address);

        self.pushOperandNoCheck(f64, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i32.load8_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const address = self.popOperand(u32);
        const value = try memory.read(i8, offset, address);

        self.pushOperandNoCheck(i32, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i32.load8_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const address = self.popOperand(u32);
        const value = try memory.read(u8, offset, address);

        self.pushOperandNoCheck(u32, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i32.load16_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const address = self.popOperand(u32);
        const value = try memory.read(i16, offset, address);

        self.pushOperandNoCheck(i32, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i32.load16_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const address = self.popOperand(u32);
        const value = try memory.read(u16, offset, address);

        self.pushOperandNoCheck(u32, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i64.load8_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const address = self.popOperand(u32);
        const value = try memory.read(i8, offset, address);

        self.pushOperandNoCheck(i64, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i64.load8_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const address = self.popOperand(u32);
        const value = try memory.read(u8, offset, address);

        self.pushOperandNoCheck(u64, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i64.load16_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const address = self.popOperand(u32);
        const value = try memory.read(i16, offset, address);

        self.pushOperandNoCheck(i64, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i64.load16_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const address = self.popOperand(u32);
        const value = try memory.read(u16, offset, address);

        self.pushOperandNoCheck(u64, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i64.load32_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const address = self.popOperand(u32);
        const value = try memory.read(i32, offset, address);

        self.pushOperandNoCheck(i64, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i64.load32_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const address = self.popOperand(u32);
        const value = try memory.read(u32, offset, address);

        self.pushOperandNoCheck(u64, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i32.store"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const value = self.popOperand(u32);
        const address = self.popOperand(u32);

        try memory.write(u32, offset, address, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i64.store"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const value = self.popOperand(u64);
        const address = self.popOperand(u32);

        try memory.write(u64, offset, address, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"f32.store"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const value = self.popOperand(f32);
        const address = self.popOperand(u32);

        try memory.write(f32, offset, address, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"f64.store"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const value = self.popOperand(f64);
        const address = self.popOperand(u32);

        try memory.write(f64, offset, address, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i32.store8"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const value: u8 = @truncate(self.popOperand(u32));
        const address = self.popOperand(u32);

        try memory.write(u8, offset, address, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i32.store16"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const value: u16 = @truncate(self.popOperand(u32));
        const address = self.popOperand(u32);

        try memory.write(u16, offset, address, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i64.store8"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const value: u8 = @truncate(self.popOperand(u64));
        const address = self.popOperand(u32);

        try memory.write(u8, offset, address, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i64.store16"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const value: u16 = @truncate(self.popOperand(u64));
        const address = self.popOperand(u32);

        try memory.write(u16, offset, address, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i64.store32"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const alignment = immediates[imm];
        _ = alignment;
        const offset = immediates[imm + 1];

        const memory = try self.inst.getMemory(0);
        const value: u32 = @truncate(self.popOperand(u64));
        const address = self.popOperand(u32);

        try memory.write(u32, offset, address, value);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"memory.size"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const memory = try self.inst.getMemory(0);

        self.pushOperandNoCheck(u32, @as(u32, @intCast(memory.size())));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"memory.grow"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const memory = try self.inst.getMemory(0);

        const num_pages = self.popOperand(u32);
        if (memory.grow(num_pages)) |old_size| {
            self.pushOperandNoCheck(u32, @as(u32, @intCast(old_size)));
        } else |_| {
            self.pushOperandNoCheck(i32, @as(i32, -1));
        }

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.const"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const literal = @as(i32, @intCast(immediates[imm]));

        self.pushOperandNoCheck(i32, literal);

        return dispatch(self, ip + 1, imm + 1, instructions, immediates);
    }

    pub fn @"i64.const"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const lower = immediates[imm];
        const upper = immediates[imm + 1];
        const literal = @as(i64, @intCast((@as(u64, upper) << 32) + lower));

        self.pushOperandNoCheck(i64, literal);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"f32.const"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const literal = @as(f32, @floatFromInt(immediates[imm]));

        self.pushOperandNoCheck(f32, literal);

        return dispatch(self, ip + 1, imm + 1, instructions, immediates);
    }

    pub fn @"f64.const"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const lower = immediates[imm];
        const upper = immediates[imm + 1];
        const literal = @as(f64, @floatFromInt((@as(u64, upper) << 32) + lower));

        self.pushOperandNoCheck(f64, literal);

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"i32.eqz"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 == 0) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.eq"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 == c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.ne"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 != c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.lt_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(i32);
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 < c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.lt_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 < c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.gt_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(i32);
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 > c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.gt_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 > c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.le_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(i32);
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 <= c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.le_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 <= c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.ge_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(i32);
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 >= c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.ge_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, @as(u32, if (c1 >= c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.eqz"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 == 0) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.eq"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 == c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.ne"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 != c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.lt_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(i64);
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 < c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.lt_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 < c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.gt_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(i64);
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 > c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.gt_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 > c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.le_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(i64);
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 <= c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.le_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 <= c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.ge_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(i64);
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 >= c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.ge_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 >= c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.eq"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 == c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.ne"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 != c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.lt"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 < c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.gt"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 > c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.le"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 <= c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.ge"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 >= c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.eq"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 == c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.ne"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 != c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.lt"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 < c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.gt"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 > c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.le"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 <= c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.ge"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(u64, @as(u64, if (c1 >= c2) 1 else 0));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.clz"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(u32);
        self.pushOperandNoCheck(u32, @clz(c1));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.ctz"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(u32);
        self.pushOperandNoCheck(u32, @ctz(c1));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.popcnt"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(u32);
        self.pushOperandNoCheck(u32, @popCount(c1));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.add"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, c1 +% c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.sub"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, c1 -% c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.mul"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, c1 *% c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.div_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(i32);
        const c1 = self.popOperand(i32);

        const div = try math.divTrunc(i32, c1, c2);

        self.pushOperandNoCheck(i32, div);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.div_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        const div = try math.divTrunc(u32, c1, c2);

        self.pushOperandNoCheck(u32, div);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.rem_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(i32);
        const c1 = self.popOperand(i32);

        const abs = try math.absInt(c2);
        const rem = try math.rem(i32, c1, abs);

        self.pushOperandNoCheck(i32, rem);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.rem_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        const rem = try math.rem(u32, c1, c2);

        self.pushOperandNoCheck(u32, rem);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.and"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, c1 & c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.or"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, c1 | c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.xor"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, c1 ^ c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.shl"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, math.shl(u32, c1, c2 % 32));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.shr_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(i32);
        const c1 = self.popOperand(i32);

        const mod = try math.mod(i32, c2, 32);

        self.pushOperandNoCheck(i32, math.shr(i32, c1, mod));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.shr_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, math.shr(u32, c1, c2 % 32));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.rotl"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, math.rotl(u32, c1, c2 % 32));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.rotr"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u32);
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(u32, math.rotr(u32, c1, c2 % 32));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.clz"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(u64);
        self.pushOperandNoCheck(u64, @clz(c1));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.ctz"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(u64);
        self.pushOperandNoCheck(u64, @ctz(c1));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.popcnt"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(u64);
        self.pushOperandNoCheck(u64, @popCount(c1));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.add"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, c1 +% c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.sub"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, c1 -% c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.mul"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, c1 *% c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.div_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(i64);
        const c1 = self.popOperand(i64);

        const div = try math.divTrunc(i64, c1, c2);

        self.pushOperandNoCheck(i64, div);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.div_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        const div = try math.divTrunc(u64, c1, c2);

        self.pushOperandNoCheck(u64, div);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.rem_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(i64);
        const c1 = self.popOperand(i64);

        const abs = try math.absInt(c2);
        const rem = try math.rem(i64, c1, abs);

        self.pushOperandNoCheck(i64, rem);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.rem_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        const rem = try math.rem(u64, c1, c2);

        self.pushOperandNoCheck(u64, rem);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.and"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, c1 & c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.or"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, c1 | c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.xor"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, c1 ^ c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.shl"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, math.shl(u64, c1, c2 % 64));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.shr_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(i64);
        const c1 = self.popOperand(i64);

        const mod = try math.mod(i64, c2, 64);

        self.pushOperandNoCheck(i64, math.shr(i64, c1, mod));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.shr_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, math.shr(u64, c1, c2 % 64));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.rotl"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, math.rotl(u64, c1, c2 % 64));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.rotr"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(u64);
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, math.rotr(u64, c1, c2 % 64));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.abs"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, math.fabs(c1));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.neg"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, -c1);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.ceil"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, @ceil(c1));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.floor"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, @floor(c1));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.trunc"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, @trunc(c1));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.nearest"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
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

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.sqrt"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, math.sqrt(c1));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.add"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, c1 + c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.sub"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, c1 - c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.mul"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, c1 * c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.div"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f32, c1 / c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.min"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        if (math.isNan(c1)) {
            self.pushOperandNoCheck(f32, math.nan_f32);
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }
        if (math.isNan(c2)) {
            self.pushOperandNoCheck(f32, math.nan_f32);
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        if (c1 == 0.0 and c2 == 0.0) {
            if (math.signbit(c1)) {
                self.pushOperandNoCheck(f32, c1);
            } else {
                self.pushOperandNoCheck(f32, c2);
            }
        } else {
            self.pushOperandNoCheck(f32, @min(c1, c2));
        }

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.max"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        if (math.isNan(c1)) {
            self.pushOperandNoCheck(f32, math.nan_f32);
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }
        if (math.isNan(c2)) {
            self.pushOperandNoCheck(f32, math.nan_f32);
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        if (c1 == 0.0 and c2 == 0.0) {
            if (math.signbit(c1)) {
                self.pushOperandNoCheck(f32, c2);
            } else {
                self.pushOperandNoCheck(f32, c1);
            }
        } else {
            self.pushOperandNoCheck(f32, @max(c1, c2));
        }

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.copysign"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f32);
        const c1 = self.popOperand(f32);

        if (math.signbit(c2)) {
            self.pushOperandNoCheck(f32, -math.fabs(c1));
        } else {
            self.pushOperandNoCheck(f32, math.fabs(c1));
        }

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.abs"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, math.fabs(c1));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.neg"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, -c1);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.ceil"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, @ceil(c1));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.floor"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, @floor(c1));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.trunc"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, @trunc(c1));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.nearest"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
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

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.sqrt"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, math.sqrt(c1));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.add"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, c1 + c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.sub"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, c1 - c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.mul"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, c1 * c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.div"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f64, c1 / c2);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.min"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        if (math.isNan(c1) or math.isNan(c2)) {
            self.pushOperandNoCheck(f64, math.nan_f64);
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        if (c1 == 0.0 and c2 == 0.0) {
            if (math.signbit(c1)) {
                self.pushOperandNoCheck(f64, c1);
            } else {
                self.pushOperandNoCheck(f64, c2);
            }
        } else {
            self.pushOperandNoCheck(f64, @min(c1, c2));
        }

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.max"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        if (math.isNan(c1) or math.isNan(c2)) {
            self.pushOperandNoCheck(f64, math.nan_f64);
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        if (c1 == 0.0 and c2 == 0.0) {
            if (math.signbit(c1)) {
                self.pushOperandNoCheck(f64, c2);
            } else {
                self.pushOperandNoCheck(f64, c1);
            }
        } else {
            self.pushOperandNoCheck(f64, @max(c1, c2));
        }

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.copysign"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c2 = self.popOperand(f64);
        const c1 = self.popOperand(f64);

        if (math.signbit(c2)) {
            self.pushOperandNoCheck(f64, -math.fabs(c1));
        } else {
            self.pushOperandNoCheck(f64, math.fabs(c1));
        }

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.wrap_i64"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(i32, @as(i32, @truncate(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.trunc_f32_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f32);

        if (math.isNan(c1)) return error.InvalidConversion;

        const trunc = @trunc(c1);

        if (trunc >= @as(f32, @floatFromInt(math.maxInt(i32)))) return error.Overflow;
        if (trunc < @as(f32, @floatFromInt(math.minInt(i32)))) return error.Overflow;

        self.pushOperandNoCheck(i32, @as(i32, @intFromFloat(trunc)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.trunc_f32_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f32);

        if (math.isNan(c1)) return error.InvalidConversion;

        const trunc = @trunc(c1);

        if (trunc >= @as(f32, @floatFromInt(math.maxInt(u32)))) return error.Overflow;
        if (trunc < @as(f32, @floatFromInt(math.minInt(u32)))) return error.Overflow;

        self.pushOperandNoCheck(u32, @as(u32, @intFromFloat(trunc)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.trunc_f64_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f64);

        if (math.isNan(c1)) return error.InvalidConversion;

        const trunc = @trunc(c1);

        if (trunc > @as(f64, @floatFromInt(math.maxInt(i32)))) return error.Overflow;
        if (trunc < @as(f64, @floatFromInt(math.minInt(i32)))) return error.Overflow;

        self.pushOperandNoCheck(i32, @as(i32, @intFromFloat(trunc)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.trunc_f64_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f64);

        if (math.isNan(c1)) return error.InvalidConversion;

        const trunc = @trunc(c1);

        if (trunc > @as(f64, @floatFromInt(math.maxInt(u32)))) return error.Overflow;
        if (trunc < @as(f64, @floatFromInt(math.minInt(u32)))) return error.Overflow;

        self.pushOperandNoCheck(u32, @as(u32, @intFromFloat(trunc)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.extend_i32_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(i64, @as(i32, @truncate(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.extend_i32_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(u64, @as(u32, @truncate(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.trunc_f32_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f32);

        if (math.isNan(c1)) return error.InvalidConversion;

        const trunc = @trunc(c1);

        if (trunc >= @as(f32, @floatFromInt(math.maxInt(i64)))) return error.Overflow;
        if (trunc < @as(f32, @floatFromInt(math.minInt(i64)))) return error.Overflow;

        self.pushOperandNoCheck(i64, @as(i64, @intFromFloat(trunc)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.trunc_f32_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f32);

        if (math.isNan(c1)) return error.InvalidConversion;

        const trunc = @trunc(c1);

        if (trunc >= @as(f32, @floatFromInt(math.maxInt(u64)))) return error.Overflow;
        if (trunc < @as(f32, @floatFromInt(math.minInt(u64)))) return error.Overflow;

        self.pushOperandNoCheck(u64, @as(u64, @intFromFloat(trunc)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.trunc_f64_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f64);

        if (math.isNan(c1)) return error.InvalidConversion;

        const trunc = @trunc(c1);

        if (trunc >= @as(f64, @floatFromInt(math.maxInt(i64)))) return error.Overflow;
        if (trunc < @as(f64, @floatFromInt(math.minInt(i64)))) return error.Overflow;

        self.pushOperandNoCheck(i64, @as(i64, @intFromFloat(trunc)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.trunc_f64_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f64);

        if (math.isNan(c1)) return error.InvalidConversion;

        const trunc = @trunc(c1);

        if (trunc >= @as(f64, @floatFromInt(math.maxInt(u64)))) return error.Overflow;
        if (trunc < @as(f64, @floatFromInt(math.minInt(u64)))) return error.Overflow;

        self.pushOperandNoCheck(u64, @as(u64, @intFromFloat(trunc)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.convert_i32_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(f32, @as(f32, @floatFromInt(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.convert_i32_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(f32, @as(f32, @floatFromInt(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.convert_i64_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(f32, @as(f32, @floatFromInt(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.convert_i64_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(f32, @as(f32, @floatFromInt(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.demote_f64"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(f32, @as(f32, @floatCast(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.convert_i32_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(f64, @as(f64, @floatFromInt(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.convert_i32_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(u32);

        self.pushOperandNoCheck(f64, @as(f64, @floatFromInt(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.convert_i64_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(f64, @as(f64, @floatFromInt(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.convert_i64_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(u64);

        self.pushOperandNoCheck(f64, @as(f64, @floatFromInt(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.promote_f32"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(f64, @as(f64, @floatCast(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.reinterpret_f32"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f32);

        self.pushOperandNoCheck(i32, @as(i32, @bitCast(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.reinterpret_f64"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f64);

        self.pushOperandNoCheck(i64, @as(i64, @bitCast(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f32.reinterpret_i32"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(f32, @as(f32, @bitCast(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"f64.reinterpret_i64"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(f64, @as(f64, @bitCast(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.extend8_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(i32, @as(i8, @truncate(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.extend16_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(i32);

        self.pushOperandNoCheck(i32, @as(i16, @truncate(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.extend8_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(i64, @as(i8, @truncate(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.extend16_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(i64, @as(i16, @truncate(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.extend32_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(i64);

        self.pushOperandNoCheck(i64, @as(i32, @truncate(c1)));

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"ref.null"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        self.pushOperandNoCheck(u64, REF_NULL);

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"ref.is_null"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const value = self.popOperand(u64);

        if (value == REF_NULL) {
            self.pushOperandNoCheck(u64, 1);
        } else {
            self.pushOperandNoCheck(u64, 0);
        }

        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"ref.func"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const funcidx = immediates[imm];

        const ref = self.inst.funcaddrs.items[funcidx]; // Not sure about this at all, this could still coincidentally be zero?

        self.pushOperandNoCheck(u64, ref);

        return dispatch(self, ip + 1, imm + 1, instructions, immediates);
    }

    pub fn misc(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        return miscDispatch(self, ip, imm, instructions, immediates);
    }

    const misc_lookup = [18]InstructionFunction{
        @"i32.trunc_sat_f32_s", @"i32.trunc_sat_f32_u", @"i32.trunc_sat_f64_s", @"i32.trunc_sat_f64_u", @"i64.trunc_sat_f32_s", @"i64.trunc_sat_f32_u", @"i64.trunc_sat_f64_s", @"i64.trunc_sat_f64_u", @"memory.init", @"data.drop", @"memory.copy", @"memory.fill", @"table.init", @"elem.drop", @"table.copy", @"table.grow",
        @"table.size",          @"table.fill",
    };

    inline fn miscDispatch(self: *VirtualMachine, next_ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        // const next_instr = code[next_ip].misc;
        const next_instr = immediates[imm];
        const next_offset = if (true) @panic("FIXME");

        return try @call(.always_tail, misc_lookup[@intFromEnum(next_instr)], .{ self, next_ip, next_offset, instructions, immediates });
    }

    pub fn @"i32.trunc_sat_f32_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f32);
        const trunc = @trunc(c1);

        if (math.isNan(c1)) {
            self.pushOperandNoCheck(i32, 0);
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        if (trunc >= @as(f32, @floatFromInt(math.maxInt(i32)))) {
            self.pushOperandNoCheck(i32, @as(i32, @bitCast(@as(u32, 0x7fffffff))));
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }
        if (trunc < @as(f32, @floatFromInt(math.minInt(i32)))) {
            self.pushOperandNoCheck(i32, @as(i32, @bitCast(@as(u32, 0x80000000))));
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        self.pushOperandNoCheck(i32, @as(i32, @intFromFloat(trunc)));
        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.trunc_sat_f32_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f32);
        const trunc = @trunc(c1);

        if (math.isNan(c1)) {
            self.pushOperandNoCheck(u32, 0);
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        if (trunc >= @as(f32, @floatFromInt(math.maxInt(u32)))) {
            self.pushOperandNoCheck(u32, @as(u32, @bitCast(@as(u32, 0xffffffff))));
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }
        if (trunc < @as(f32, @floatFromInt(math.minInt(u32)))) {
            self.pushOperandNoCheck(u32, @as(u32, @bitCast(@as(u32, 0x00000000))));
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        self.pushOperandNoCheck(u32, @as(u32, @intFromFloat(trunc)));
        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.trunc_sat_f64_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f64);
        const trunc = @trunc(c1);

        if (math.isNan(c1)) {
            self.pushOperandNoCheck(i32, 0);
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        if (trunc >= @as(f64, @floatFromInt(math.maxInt(i32)))) {
            self.pushOperandNoCheck(i32, @as(i32, @bitCast(@as(u32, 0x7fffffff))));
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }
        if (trunc < @as(f64, @floatFromInt(math.minInt(i32)))) {
            self.pushOperandNoCheck(i32, @as(i32, @bitCast(@as(u32, 0x80000000))));
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        self.pushOperandNoCheck(i32, @as(i32, @intFromFloat(trunc)));
        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i32.trunc_sat_f64_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f64);
        const trunc = @trunc(c1);

        if (math.isNan(c1)) {
            self.pushOperandNoCheck(u32, 0);
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        if (trunc >= @as(f64, @floatFromInt(math.maxInt(u32)))) {
            self.pushOperandNoCheck(u32, @as(u32, @bitCast(@as(u32, 0xffffffff))));
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }
        if (trunc < @as(f64, @floatFromInt(math.minInt(u32)))) {
            self.pushOperandNoCheck(u32, @as(u32, @bitCast(@as(u32, 0x00000000))));
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        self.pushOperandNoCheck(u32, @as(u32, @intFromFloat(trunc)));
        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.trunc_sat_f32_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f32);
        const trunc = @trunc(c1);

        if (math.isNan(c1)) {
            self.pushOperandNoCheck(i64, 0);
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        if (trunc >= @as(f32, @floatFromInt(math.maxInt(i64)))) {
            self.pushOperandNoCheck(i64, @as(i64, @bitCast(@as(u64, 0x7fffffffffffffff))));
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }
        if (trunc < @as(f32, @floatFromInt(math.minInt(i64)))) {
            self.pushOperandNoCheck(i64, @as(i64, @bitCast(@as(u64, 0x8000000000000000))));
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        self.pushOperandNoCheck(i64, @as(i64, @intFromFloat(trunc)));
        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.trunc_sat_f32_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f32);
        const trunc = @trunc(c1);

        if (math.isNan(c1)) {
            self.pushOperandNoCheck(u64, 0);
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        if (trunc >= @as(f32, @floatFromInt(math.maxInt(u64)))) {
            self.pushOperandNoCheck(u64, @as(u64, @bitCast(@as(u64, 0xffffffffffffffff))));
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }
        if (trunc < @as(f32, @floatFromInt(math.minInt(u64)))) {
            self.pushOperandNoCheck(u64, @as(u64, @bitCast(@as(u64, 0x0000000000000000))));
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        self.pushOperandNoCheck(u64, @as(u64, @intFromFloat(trunc)));
        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.trunc_sat_f64_s"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f64);
        const trunc = @trunc(c1);

        if (math.isNan(c1)) {
            self.pushOperandNoCheck(i64, 0);
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        if (trunc >= @as(f64, @floatFromInt(math.maxInt(i64)))) {
            self.pushOperandNoCheck(i64, @as(i64, @bitCast(@as(u64, 0x7fffffffffffffff))));
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }
        if (trunc < @as(f64, @floatFromInt(math.minInt(i64)))) {
            self.pushOperandNoCheck(i64, @as(i64, @bitCast(@as(u64, 0x8000000000000000))));
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        self.pushOperandNoCheck(i64, @as(i64, @intFromFloat(trunc)));
        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"i64.trunc_sat_f64_u"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const c1 = self.popOperand(f64);
        const trunc = @trunc(c1);

        if (math.isNan(c1)) {
            self.pushOperandNoCheck(u64, 0);
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        if (trunc >= @as(f64, @floatFromInt(math.maxInt(u64)))) {
            self.pushOperandNoCheck(u64, @as(u64, @bitCast(@as(u64, 0xffffffffffffffff))));
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }
        if (trunc < @as(f64, @floatFromInt(math.minInt(u64)))) {
            self.pushOperandNoCheck(u64, @as(u64, @bitCast(@as(u64, 0x0000000000000000))));
            return dispatch(self, ip + 1, imm, instructions, immediates);
        }

        self.pushOperandNoCheck(u64, @as(u64, @intFromFloat(trunc)));
        return dispatch(self, ip + 1, imm, instructions, immediates);
    }

    pub fn @"memory.init"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        // const meta = code[ip].misc.@"memory.init";
        const memidx = immediates[imm];
        const dataidx = immediates[imm + 1];

        const n = self.popOperand(u32);
        const src = self.popOperand(u32);
        const dest = self.popOperand(u32);

        const memory = try self.inst.getMemory(memidx);
        const mem_size = memory.sizeBytes();
        const data = try self.inst.getData(dataidx);

        if (@as(u33, src) + @as(u33, n) > data.data.len) return error.OutOfBoundsMemoryAccess;
        if (@as(u33, dest) + @as(u33, n) > mem_size) return error.OutOfBoundsMemoryAccess;
        if (n == 0) {
            return dispatch(self, ip + 1, imm + 2, instructions, immediates);
        }

        if (data.dropped) return error.OutOfBoundsMemoryAccess;

        var i: u32 = 0;
        while (i < n) : (i += 1) {
            try memory.write(u8, 0, dest + i, data.data[src + i]);
        }

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"data.drop"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        // const dataidx = code[ip].misc.@"data.drop";
        const dataidx = immediates[imm];
        const data = try self.inst.getData(dataidx);
        data.dropped = true;

        return dispatch(self, ip + 1, imm + 1, instructions, immediates);
    }

    pub fn @"memory.copy"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        // FIXME: use these when we support multiple memories
        const src_memidx = immediates[imm];
        _ = src_memidx;
        const dst_memidx = immediates[imm + 1];
        _ = dst_memidx;

        const n = self.popOperand(u32);
        const src = self.popOperand(u32);
        const dest = self.popOperand(u32);

        const memory = try self.inst.getMemory(0);
        const mem_size = memory.sizeBytes();

        if (@as(u33, src) + @as(u33, n) > mem_size) return error.OutOfBoundsMemoryAccess;
        if (@as(u33, dest) + @as(u33, n) > mem_size) return error.OutOfBoundsMemoryAccess;

        if (n == 0) {
            return dispatch(self, ip + 1, imm + 2, instructions, immediates);
        }

        // FIXME: move initial bounds check into Memory implementation
        const data = memory.memory();
        if (dest <= src) {
            memory.uncheckedCopy(dest, data[src .. src + n]);
        } else {
            memory.uncheckedCopyBackwards(dest, data[src .. src + n]);
        }

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"memory.fill"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        _ = immediates[imm];

        const n = self.popOperand(u32);
        const value = self.popOperand(u32);
        const dest = self.popOperand(u32);

        const memory = try self.inst.getMemory(0);
        const mem_size = memory.sizeBytes();

        if (@as(u33, dest) + @as(u33, n) > mem_size) return error.OutOfBoundsMemoryAccess;
        if (n == 0) {
            return dispatch(self, ip + 1, imm + 1, instructions, immediates);
        }

        memory.uncheckedFill(dest, n, @as(u8, @truncate(value)));

        return dispatch(self, ip + 1, imm + 1, instructions, immediates);
    }

    pub fn @"table.init"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        // const meta = code[ip].misc.@"table.init";
        const tableidx = immediates[imm];
        const elemidx = immediates[imm + 1];

        const table = try self.inst.getTable(tableidx);
        const elem = try self.inst.getElem(elemidx);

        const n = self.popOperand(u32);
        const s = self.popOperand(u32);
        const d = self.popOperand(u32);

        _ = math.add(u32, s, n) catch return error.Trap;
        _ = math.add(u32, d, n) catch return error.Trap;

        if (s + n > elem.elem.len) return error.Trap;
        if (d + n > table.size()) return error.Trap;
        if (n == 0) return;

        if (elem.dropped) return error.Trap;

        var i: u32 = 0;
        while (i < n) : (i += 1) {
            try table.set(d + i, elem.elem[s + i]);
        }

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"elem.drop"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        // const meta = code[ip].misc.@"elem.drop";
        const elemidx = immediates[imm];
        const elem = try self.inst.getElem(elemidx);
        elem.dropped = true;

        return dispatch(self, ip + 1, imm + 1, instructions, immediates);
    }

    pub fn @"table.copy"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        // const meta = code[ip].misc.@"table.copy";
        // const dest_tableidx = meta.dest_tableidx;
        // const src_tableidx = meta.src_tableidx;
        const dst_tableidx = immediates[imm];
        const src_tableidx = immediates[imm + 1];

        const dest_table = try self.inst.getTable(dst_tableidx);
        const src_table = try self.inst.getTable(src_tableidx);

        const n = self.popOperand(u32);
        const s = self.popOperand(u32);
        const d = self.popOperand(u32);

        _ = math.add(u32, s, n) catch return error.Trap;
        _ = math.add(u32, d, n) catch return error.Trap;

        if (s + n > src_table.size()) return error.Trap;
        if (d + n > dest_table.size()) return error.Trap;
        if (n == 0) return;

        if (d <= s) {
            var i: u32 = 0;
            while (i < n) : (i += 1) {
                try dest_table.set(d + i, try src_table.get(s + i));
            }
        } else {
            var i: u32 = 0;
            while (i < n) : (i += 1) {
                try dest_table.set(d + n - 1 - i, try src_table.get(s + n - 1 - i));
            }
        }

        return dispatch(self, ip + 1, imm + 2, instructions, immediates);
    }

    pub fn @"table.grow"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const tableidx = immediates[imm];

        const table = try self.inst.getTable(tableidx);

        const n = self.popOperand(u32);
        const ref = self.popOperand(u64);

        if (table.grow(n)) |old_size| {
            self.pushOperandNoCheck(u32, old_size);
            var i: u32 = 0;
            while (i < n) : (i += 1) {
                try table.set(old_size + i, ref);
            }
        } else |_| {
            self.pushOperandNoCheck(i32, @as(i32, -1));
        }

        return dispatch(self, ip + 1, imm + 1, instructions, immediates);
    }

    pub fn @"table.size"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const tableidx = immediates[imm];

        const table = try self.inst.getTable(tableidx);

        self.pushOperandNoCheck(u32, @as(u32, @intCast(table.size())));

        return dispatch(self, ip + 1, imm + 1, instructions, immediates);
    }

    pub fn @"table.fill"(self: *VirtualMachine, ip: usize, imm: usize, instructions: []Instruction, immediates: []u32) WasmError!void {
        const tableidx = immediates[imm];

        const table = try self.inst.getTable(tableidx);

        const n = self.popOperand(u32);
        const ref = self.popOperand(u64);
        const d = self.popOperand(u32);

        _ = math.add(u32, d, n) catch return error.Trap;

        if (d + n > table.size()) return error.Trap;
        if (n == 0) return;

        var i: u32 = 0;
        while (i < n) : (i += 1) {
            try table.set(d + i, ref);
        }

        return dispatch(self, ip + 1, imm + 1, instructions, immediates);
    }

    // https://webassembly.github.io/spec/core/exec/instructions.html#xref-syntax-instructions-syntax-instr-control-mathsf-br-l
    pub fn branch(self: *VirtualMachine, target: u32) usize {
        const label = self.peekNthLabel(target);
        const n = label.return_arity;

        var i: usize = 0;
        while (i < n) : (i += 1) {
            self.op_stack[label.op_stack_len + i] = self.op_stack[self.op_ptr - n + i];
        }

        self.op_ptr = label.op_stack_len + n;
        _ = self.popLabels(target);

        return label.branch_target;
    }

    pub fn pushOperand(self: *VirtualMachine, comptime T: type, value: T) !void {
        if (self.op_ptr == self.op_stack.len) return error.OperandStackOverflow;

        self.op_ptr += 1;

        self.op_stack[self.op_ptr - 1] = switch (T) {
            i32 => @as(u32, @bitCast(value)),
            i64 => @as(u64, @bitCast(value)),
            f32 => @as(u32, @bitCast(value)),
            f64 => @as(u64, @bitCast(value)),
            u32 => @as(u64, value),
            u64 => value,
            else => |t| @compileError("Unsupported operand type: " ++ @typeName(t)),
        };
    }

    pub fn checkStackSpace(self: *VirtualMachine, n: usize) !void {
        if (self.op_ptr + n > self.op_stack.len) return error.CheckStackSpace;
    }

    pub fn pushOperandNoCheck(self: *VirtualMachine, comptime T: type, value: T) void {
        self.op_ptr += 1;

        self.op_stack[self.op_ptr - 1] = switch (T) {
            i32 => @as(u32, @bitCast(value)),
            i64 => @as(u64, @bitCast(value)),
            f32 => @as(u32, @bitCast(value)),
            f64 => @as(u64, @bitCast(value)),
            u32 => @as(u64, value),
            u64 => value,
            else => |t| @compileError("Unsupported operand type: " ++ @typeName(t)),
        };
    }

    pub fn popOperand(self: *VirtualMachine, comptime T: type) T {
        defer self.op_ptr -= 1;

        const value = self.op_stack[self.op_ptr - 1];
        return switch (T) {
            i32 => @as(i32, @bitCast(@as(u32, @truncate(value)))),
            i64 => @as(i64, @bitCast(value)),
            f32 => @as(f32, @bitCast(@as(u32, @truncate(value)))),
            f64 => @as(f64, @bitCast(value)),
            u32 => @as(u32, @truncate(value)),
            u64 => value,
            else => |t| @compileError("Unsupported operand type: " ++ @typeName(t)),
        };
    }

    pub fn popAnyOperand(self: *VirtualMachine) u64 {
        defer self.op_ptr -= 1;

        return self.op_stack[self.op_ptr - 1];
    }

    pub fn peekOperand(self: *VirtualMachine) u64 {
        return self.op_stack[self.op_ptr - 1];
    }

    pub fn peekNthOperand(self: *VirtualMachine, index: u32) u64 {
        return self.op_stack[self.op_ptr - index - 1];
    }

    // TODO: if the code is validated, do we need to know the params count
    //       i.e. can we get rid of the dependency on params so that we don't
    //       have to lookup a function (necessarily)
    pub fn pushFrame(self: *VirtualMachine, frame: Frame, params_and_locals_count: usize) !void {
        if (self.frame_ptr == self.frame_stack.len) return error.ControlStackOverflow;
        self.frame_ptr += 1;

        const current_frame = &self.frame_stack[self.frame_ptr - 1];
        current_frame.* = frame;

        try self.checkStackSpace(params_and_locals_count);
        current_frame.locals = self.op_stack[frame.op_stack_len .. frame.op_stack_len + params_and_locals_count];
    }

    pub fn popFrame(self: *VirtualMachine) Frame {
        defer self.frame_ptr -= 1;

        return self.frame_stack[self.frame_ptr - 1];
    }

    pub fn peekFrame(self: *VirtualMachine) *Frame {
        return &self.frame_stack[self.frame_ptr - 1];
    }

    pub fn pushLabel(self: *VirtualMachine, label: Label) !void {
        if (self.label_ptr == self.label_stack.len) return error.LabelStackOverflow;

        self.label_ptr += 1;
        const current_label = self.peekNthLabel(0);
        current_label.* = label;
    }

    pub fn popLabel(self: *VirtualMachine) Label {
        defer self.label_ptr -= 1;

        return self.label_stack[self.label_ptr - 1];
    }

    // peekNthLabel
    //
    // Returns nth label on the Label stack relative to the top of the stack
    //
    pub fn peekNthLabel(self: *VirtualMachine, index: u32) *Label {
        return &self.label_stack[self.label_ptr - index - 1];
    }

    // popLabels
    //
    // target: branch target (relative to current scope which is 0)
    //
    // popLabels pops labels up to and including `target`. Returns the
    // the label at `target`.
    pub fn popLabels(self: *VirtualMachine, target: u32) Label {
        defer self.label_ptr = self.label_ptr - target - 1;

        return self.label_stack[self.label_stack.len - target - 1];
    }
};

pub const WasmError = error{
    NotImplemented,
    StackUnderflow,
    StackOverflow,
    TrapUnreachable,
    LabelStackUnderflow,
    LabelStackOverflow,
    OperandStackUnderflow,
    ControlStackUnderflow,
    OperandStackOverflow,
    FunctionIndexOutOfBounds,
    BadFunctionIndex,
    ControlStackOverflow,
    BadInstanceIndex,
    DivisionByZero,
    Overflow,
    InvalidConversion,
    OutOfBoundsMemoryAccess,
    MismatchedSignatures,
    UndefinedElement,
    BadMemoryIndex,
    MemoryIndexOutOfBounds,
    BadTableIndex,
    TableIndexOutOfBounds,
    BadGlobalIndex,
    ElemIndexOutOfBounds,
    BadElemAddr,
    GlobalIndexOutOfBounds,
    NegativeDenominator,
    Trap,
    CheckStackSpace,
    DataIndexOutOfBounds,
    BadDataAddr,
};

const testing = std.testing;

test "operand push / pop test" {
    var op_stack: [6]u64 = [_]u64{0} ** 6;
    var frame_stack_mem: [1024]VirtualMachine.Frame = [_]VirtualMachine.Frame{undefined} ** 1024;
    var label_stack_mem: [1024]VirtualMachine.Label = [_]VirtualMachine.Label{undefined} ** 1024;

    var inst: Instance = undefined;

    var i = VirtualMachine.init(op_stack[0..], frame_stack_mem[0..], label_stack_mem[0..], &inst);

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
//     var frame_stack: [1024]VirtualMachine.Frame = [_]VirtualMachine.Frame{undefined} ** 1024;
//     var label_stack_mem: [1024]VirtualMachine.Label = [_]VirtualMachine.Label{undefined} ** 1024;

//     var inst: Instance = undefined;
//     var i = VirtualMachine.init(op_stack[0..], frame_stack[0..], label_stack_mem[0..], &inst);

//     try i.pushOperand(i32, 22);
//     try i.pushOperand(i32, -23);

//     var code = [_]Instruction{Instruction.@"i32.add"};

//     try i.invoke(0);

//     try testing.expectEqual(@as(i32, -1), i.popOperand(i32));
// }
