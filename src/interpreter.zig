const std = @import("std");
const mem = std.mem;
const math = std.math;
const ArrayList = std.ArrayList;
const ValueType = @import("module.zig").ValueType;
const Instance = @import("instance.zig").Instance;
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
    frame_stack_mem: []Frame = undefined,
    frame_stack: []Frame = undefined,
    label_stack_mem: []Label = undefined,
    label_stack: []Label = undefined,

    continuation: []const u8 = undefined,
    inst: *Instance = undefined,

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

    pub fn invoke(self: *Interpreter, code: []const u8) !void {
        self.continuation = code;
        while (self.continuation.len > 0) {
            const instr = self.continuation[0];
            const instr_code = self.continuation;
            self.continuation = self.continuation[1..];
            try self.interpret(@intToEnum(Instruction, instr), instr_code);
        }
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

    pub fn interpret(self: *Interpreter, opcode: Instruction, code: []const u8) !void {
        // defer self.debug(opcode);
        switch (opcode) {
            .Unreachable => return error.TrapUnreachable,
            .Nop => return,
            .Block => {
                const block_type = try instruction.readILEB128Mem(i32, &self.continuation);

                var block_params: usize = 0;
                var block_returns: usize = if (block_type == -0x40) 0 else 1;
                if (block_type >= 0) {
                    const func_type = self.inst.module.types.list.items[@intCast(usize, block_type)];
                    block_params = func_type.params_count;
                    block_returns = func_type.results_count;
                }

                const end = try instruction.findEnd(false, code);
                const continuation = code[end.offset + 1 ..];

                try self.pushLabel(Label{
                    .return_arity = block_returns,
                    .op_stack_len = self.op_stack.len - block_params, // equivalent to pop and push
                    .continuation = continuation,
                });
            },
            .Loop => {
                const block_type = try instruction.readILEB128Mem(i32, &self.continuation);

                var block_params: usize = 0;
                var block_returns: usize = if (block_type == -0x40) 0 else 1;
                if (block_type >= 0) {
                    const func_type = self.inst.module.types.list.items[@intCast(usize, block_type)];
                    block_params = func_type.params_count;
                    block_returns = func_type.results_count;
                }

                // For loop control flow, the continuation is the loop body (including
                // the initiating loop instruction, as branch consumes the existing label)
                const continuation = code[0..];
                try self.pushLabel(Label{
                    // note that we use block_params rather than block_returns for return arity:
                    .return_arity = block_params,
                    .op_stack_len = self.op_stack.len - block_params,
                    .continuation = continuation,
                });
            },
            .If => {
                // TODO: perform findEnd during parsing
                const block_type = try instruction.readILEB128Mem(i32, &self.continuation);
                const end = try instruction.findEnd(false, code);
                const else_branch = try instruction.findElse(false, code);

                var block_params: usize = 0;
                var block_returns: usize = if (block_type == -0x40) 0 else 1;
                if (block_type >= 0) {
                    const func_type = self.inst.module.types.list.items[@intCast(usize, block_type)];
                    block_params = func_type.params_count;
                    block_returns = func_type.results_count;
                }

                // For if control flow, the continuation for our label
                // is the continuation of code after end
                const continuation = code[end.offset + 1 ..];

                const condition = try self.popOperand(u32);
                if (condition == 0) {
                    // We'll skip to end
                    self.continuation = continuation;
                    // unless we have an else branch
                    if (else_branch) |eb| {
                        self.continuation = code[eb.offset + 1 ..];

                        // We are inside the if branch
                        try self.pushLabel(Label{
                            .return_arity = block_returns,
                            .op_stack_len = self.op_stack.len - block_params,
                            .continuation = continuation,
                        });
                    }
                } else {
                    // We are inside the if branch
                    try self.pushLabel(Label{
                        .return_arity = block_returns,
                        .op_stack_len = self.op_stack.len - block_params,
                        .continuation = continuation,
                    });
                }
            },
            .Else => {
                // If we hit else, it's because we've hit the end of if
                // Therefore we want to skip to end of current label
                const label = try self.popLabel();
                self.continuation = label.continuation;
            },
            .End => {
                // https://webassembly.github.io/spec/core/exec/instructions.html#exiting-xref-syntax-instructions-syntax-instr-mathit-instr-ast-with-label-l
                const label = try self.popLabel();

                // It seems like we need to special case end for a function call. This
                // doesn't seem quite right because the spec doesn't mention it. On
                // call we push a label containing a continuation which is the code to
                // resume after the call has returned. We want to use that if we've run
                // out of code in the current function, i.e. self.continuation is empty
                if (self.continuation.len == 0) {
                    const frame = try self.peekNthFrame(0);
                    const n = label.return_arity;
                    var dst = self.op_stack[label.op_stack_len .. label.op_stack_len + n];
                    const src = self.op_stack[self.op_stack.len - n ..];
                    mem.copy(u64, dst, src);

                    self.op_stack = self.op_stack[0 .. label.op_stack_len + n];
                    self.label_stack = self.label_stack[0..frame.label_stack_len];
                    self.continuation = label.continuation;
                    _ = try self.popFrame();
                }
            },
            .Br => {
                const target = try instruction.readULEB128Mem(u32, &self.continuation);
                try self.branch(target);
            },
            .BrIf => {
                const target = try instruction.readULEB128Mem(u32, &self.continuation);

                const condition = try self.popOperand(u32);
                if (condition == 0) return;

                try self.branch(target);
            },
            .BrTable => {
                const label_count = try instruction.readULEB128Mem(u32, &self.continuation);
                const i = try self.popOperand(u32);

                var label: u32 = 0;
                var j: usize = 0;
                while (j < label_count) : (j += 1) {
                    const tmp_label = try instruction.readULEB128Mem(u32, &self.continuation);
                    if (j == i) label = tmp_label;
                }

                const ln = try instruction.readULEB128Mem(u32, &self.continuation);
                if (i >= j) {
                    label = ln;
                }

                try self.branch(label);
            },
            .Return => {
                const frame = try self.peekNthFrame(0);
                const n = frame.return_arity;

                if (std.builtin.mode == .Debug) {
                    if (self.op_stack.len < n) return error.OperandStackUnderflow;
                }

                const label = self.label_stack[frame.label_stack_len];
                self.continuation = label.continuation;

                // The mem copy is equivalent of popping n operands, doing everything
                // up to and including popFrame and then repushing the n operands
                var dst = self.op_stack[label.op_stack_len .. label.op_stack_len + n];
                const src = self.op_stack[self.op_stack.len - n ..];
                mem.copy(u64, dst, src);

                self.op_stack = self.op_stack[0 .. label.op_stack_len + n];
                self.label_stack = self.label_stack[0..frame.label_stack_len];

                _ = try self.popFrame();
            },
            .Call => {
                // The spec says:
                //      The call instruction invokes another function, consuming the necessary
                //      arguments from the stack and returning the result values of the call.

                // TODO: we need to verify that we're okay to lookup this function.
                //       we can (and probably should) do that at validation time.
                const module = self.inst.module;
                const function_index = try instruction.readULEB128Mem(usize, &self.continuation);
                const function = try self.inst.getFunc(function_index);

                switch (function) {
                    .function => |f| {
                        // Make space for locals (again, params already on stack)
                        var j: usize = 0;
                        while (j < f.locals_count) : (j += 1) {
                            try self.pushOperand(u64, 0);
                        }

                        // Consume parameters from the stack
                        try self.pushFrame(Frame{
                            .op_stack_len = self.op_stack.len - f.params.len - f.locals_count,
                            .label_stack_len = self.label_stack.len,
                            .return_arity = f.results.len,
                        }, f.locals_count + f.params.len);

                        // Our continuation is the code after call
                        try self.pushLabel(Label{
                            .return_arity = f.results.len,
                            .op_stack_len = self.op_stack.len - f.params.len - f.locals_count,
                            .continuation = self.continuation,
                        });

                        self.continuation = f.code;
                        self.inst = f.instance;
                    },
                    .host_function => |hf| {
                        try hf.func(self);
                    },
                }
            },
            .CallIndirect => {
                var module = self.inst.module;

                const op_func_type_index = try instruction.readULEB128Mem(u32, &self.continuation);
                const table_index = try instruction.readULEB128Mem(u32, &self.continuation);

                // Read lookup index from stack
                const lookup_index = try self.popOperand(u32);
                const table = try self.inst.getTable(table_index);
                const function_handle = try table.lookup(lookup_index);
                const function = try self.inst.store.function(function_handle);

                switch (function) {
                    .function => |func| {
                        // Check that signatures match
                        // const func_type = module.types.list.items[function.typeidx];
                        const call_indirect_func_type = module.types.list.items[op_func_type_index];
                        if (!module.signaturesEqual2(func.params, func.results, call_indirect_func_type)) return error.IndirectCallTypeMismatch;

                        // Make space for locals (again, params already on stack)
                        var j: usize = 0;
                        while (j < func.locals_count) : (j += 1) {
                            try self.pushOperand(u64, 0);
                        }

                        // Consume parameters from the stack
                        try self.pushFrame(Frame{
                            .op_stack_len = self.op_stack.len - func.params.len - func.locals_count,
                            .label_stack_len = self.label_stack.len,
                            .return_arity = func.results.len,
                        }, func.locals_count + func.params.len);

                        // Our continuation is the code after call
                        try self.pushLabel(Label{
                            .return_arity = func.results.len,
                            .op_stack_len = self.op_stack.len - func.params.len - func.locals_count,
                            .continuation = self.continuation,
                        });

                        self.continuation = func.code;
                    },
                    .host_function => |host_func| {
                        const call_indirect_func_type = module.types.list.items[op_func_type_index];
                        if (!module.signaturesEqual2(host_func.params, host_func.results, call_indirect_func_type)) return error.IndirectCallTypeMismatch;

                        try host_func.func(self);
                    },
                }
            },
            .Drop => _ = try self.popAnyOperand(),
            .Select => {
                const condition = try self.popOperand(u32);

                const c2 = try self.popAnyOperand();
                const c1 = try self.popAnyOperand();

                if (condition != 0) {
                    try self.pushAnyOperand(c1);
                } else {
                    try self.pushAnyOperand(c2);
                }
            },
            .LocalGet => {
                const frame = try self.peekNthFrame(0);
                const local_index = try instruction.readULEB128Mem(u32, &self.continuation);
                const local_value: u64 = frame.locals[local_index];
                try self.pushOperand(u64, local_value);
            },
            .LocalSet => {
                const value = try self.popAnyOperand();
                const frame = try self.peekNthFrame(0);
                const local_index = try instruction.readULEB128Mem(u32, &self.continuation);
                frame.locals[local_index] = value;
            },
            .LocalTee => {
                const value = try self.popAnyOperand();
                const frame = try self.peekNthFrame(0);
                const local_index = try instruction.readULEB128Mem(u32, &self.continuation);
                // TODO: debug build only for return error:
                if (frame.locals.len < local_index + 1) return error.LocalOutOfBound;
                frame.locals[local_index] = value;
                try self.pushAnyOperand(value);
            },
            .GlobalGet => {
                // 1. Get index of global from immediate
                const global_index = try instruction.readULEB128Mem(u32, &self.continuation);
                // 2. Look up address of global using index. (we don't need to do this because the
                //    the global_index is also the address of the actual global in our store)
                // 3. Get value
                const global = try self.inst.getGlobal(global_index);
                try self.pushAnyOperand(global.value);
            },
            .GlobalSet => {
                // 1. Get index of global from immediate
                const global_index = try instruction.readULEB128Mem(u32, &self.continuation);
                // 2. Look up address of global using index. (we don't need to do this because the
                //    the global_index is also the address of the actual global in our store)
                // 3. Get value
                const value = try self.popAnyOperand();
                const global = try self.inst.getGlobal(global_index);
                global.value = value;
            },
            .I32Load => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const address = try self.popOperand(u32);

                const value = try memory.read(u32, offset, address);
                try self.pushOperand(u32, value);
            },
            .I64Load => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const address = try self.popOperand(u32);

                const value = try memory.read(u64, offset, address);
                try self.pushOperand(u64, value);
            },
            .F32Load => {
                const frame = try self.peekNthFrame(0);
                // TODO: we need to check this / handle multiple memories
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const address = try self.popOperand(u32);

                const value = try memory.read(f32, offset, address);
                try self.pushOperand(f32, value);
            },
            .F64Load => {
                const frame = try self.peekNthFrame(0);
                // TODO: we need to check this / handle multiple memories
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const address = try self.popOperand(u32);

                const value = try memory.read(f64, offset, address);
                try self.pushOperand(f64, value);
            },
            .I32Load8S => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const address = try self.popOperand(u32);

                const value = try memory.read(i8, offset, address);
                try self.pushOperand(i32, value);
            },
            .I32Load8U => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const address = try self.popOperand(u32);

                const value = try memory.read(u8, offset, address);
                try self.pushOperand(u32, value);
            },
            .I32Load16S => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const address = try self.popOperand(u32);

                const value = try memory.read(i16, offset, address);
                try self.pushOperand(i32, value);
            },
            .I32Load16U => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const address = try self.popOperand(u32);

                const value = try memory.read(u16, offset, address);
                try self.pushOperand(u32, value);
            },
            .I64Load8S => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const address = try self.popOperand(u32);

                const value = try memory.read(i8, offset, address);
                try self.pushOperand(i64, value);
            },
            .I64Load8U => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const address = try self.popOperand(u32);

                const value = try memory.read(u8, offset, address);
                try self.pushOperand(u64, value);
            },
            .I64Load16S => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const address = try self.popOperand(u32);

                const value = try memory.read(i16, offset, address);
                try self.pushOperand(i64, value);
            },
            .I64Load16U => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const address = try self.popOperand(u32);

                const value = try memory.read(u16, offset, address);
                try self.pushOperand(u64, value);
            },
            .I64Load32S => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const address = try self.popOperand(u32);

                const value = try memory.read(i32, offset, address);
                try self.pushOperand(i64, value);
            },
            .I64Load32U => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const address = try self.popOperand(u32);

                const value = try memory.read(u32, offset, address);
                try self.pushOperand(u64, value);
            },
            .I32Store => {
                const frame = try self.peekNthFrame(0);
                // TODO: we need to check this / handle multiple memories
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const value = try self.popOperand(u32);
                const address = try self.popOperand(u32);

                try memory.write(u32, offset, address, value);
            },
            .I64Store => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const value = try self.popOperand(u64);
                const address = try self.popOperand(u32);

                try memory.write(u64, offset, address, value);
            },
            .F32Store => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const value = try self.popOperand(f32);
                const address = try self.popOperand(u32);

                try memory.write(f32, offset, address, value);
            },
            .F64Store => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const value = try self.popOperand(f64);
                const address = try self.popOperand(u32);

                try memory.write(f64, offset, address, value);
            },
            .I32Store8 => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const value = @truncate(u8, try self.popOperand(u32));
                const address = try self.popOperand(u32);

                try memory.write(u8, offset, address, value);
            },
            .I32Store16 => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const value = @truncate(u16, try self.popOperand(u32));
                const address = try self.popOperand(u32);

                try memory.write(u16, offset, address, value);
            },
            .I64Store8 => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const value = @truncate(u8, try self.popOperand(u64));
                const address = try self.popOperand(u32);

                try memory.write(u8, offset, address, value);
            },
            .I64Store16 => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const value = @truncate(u16, try self.popOperand(u64));
                const address = try self.popOperand(u32);

                try memory.write(u16, offset, address, value);
            },
            .I64Store32 => {
                const frame = try self.peekNthFrame(0);
                const memory = try self.inst.getMemory(0);

                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);
                const offset = try instruction.readULEB128Mem(u32, &self.continuation);

                const value = @truncate(u32, try self.popOperand(u64));
                const address = try self.popOperand(u32);

                try memory.write(u32, offset, address, value);
            },
            .MemorySize => {
                const frame = try self.peekNthFrame(0);

                const memory_index = try instruction.readULEB128Mem(u32, &self.continuation);
                const memory = try self.inst.getMemory(memory_index);

                try self.pushOperand(u32, @intCast(u32, memory.data.items.len));
            },
            .MemoryGrow => {
                const frame = try self.peekNthFrame(0);

                const memory_index = try instruction.readULEB128Mem(u32, &self.continuation);
                const memory = try self.inst.getMemory(memory_index);

                const num_pages = try self.popOperand(u32);
                if (memory.grow(num_pages)) |old_size| {
                    try self.pushOperand(u32, @intCast(u32, old_size));
                } else |err| {
                    try self.pushOperand(i32, @as(i32, -1));
                }
            },
            .I32Const => {
                const x = try instruction.readILEB128Mem(i32, &self.continuation);
                try self.pushOperand(i32, x);
            },
            .I64Const => {
                const x = try instruction.readILEB128Mem(i64, &self.continuation);
                try self.pushOperand(i64, x);
            },
            .F32Const => {
                const x = try instruction.readU32(&self.continuation);
                try self.pushOperand(f32, @bitCast(f32, x));
            },
            .F64Const => {
                const x = try instruction.readU64(&self.continuation);
                try self.pushOperand(f64, @bitCast(f64, x));
            },
            .I32Eq => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, @as(u32, if (c1 == c2) 1 else 0));
            },
            .I32Ne => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, @as(u32, if (c1 != c2) 1 else 0));
            },
            .I32Eqz => {
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, @as(u32, if (c1 == 0) 1 else 0));
            },
            .I32LtS => {
                const c2 = try self.popOperand(i32);
                const c1 = try self.popOperand(i32);
                try self.pushOperand(u32, @as(u32, if (c1 < c2) 1 else 0));
            },
            .I32LtU => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, @as(u32, if (c1 < c2) 1 else 0));
            },
            .I32GtS => {
                const c2 = try self.popOperand(i32);
                const c1 = try self.popOperand(i32);
                try self.pushOperand(u32, @as(u32, if (c1 > c2) 1 else 0));
            },
            .I32GtU => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, @as(u32, if (c1 > c2) 1 else 0));
            },
            .I32LeS => {
                const c2 = try self.popOperand(i32);
                const c1 = try self.popOperand(i32);
                try self.pushOperand(u32, @as(u32, if (c1 <= c2) 1 else 0));
            },
            .I32LeU => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, @as(u32, if (c1 <= c2) 1 else 0));
            },
            .I32GeS => {
                const c2 = try self.popOperand(i32);
                const c1 = try self.popOperand(i32);
                try self.pushOperand(u32, @as(u32, if (c1 >= c2) 1 else 0));
            },
            .I32GeU => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, @as(u32, if (c1 >= c2) 1 else 0));
            },
            .I64Eq => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, @as(u64, if (c1 == c2) 1 else 0));
            },
            .I64Ne => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, @as(u64, if (c1 != c2) 1 else 0));
            },
            .I64Eqz => {
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, @as(u64, if (c1 == 0) 1 else 0));
            },
            .I64LtS => {
                const c2 = try self.popOperand(i64);
                const c1 = try self.popOperand(i64);
                try self.pushOperand(u64, @as(u64, if (c1 < c2) 1 else 0));
            },
            .I64LtU => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, @as(u64, if (c1 < c2) 1 else 0));
            },
            .I64GtS => {
                const c2 = try self.popOperand(i64);
                const c1 = try self.popOperand(i64);
                try self.pushOperand(u64, @as(u64, if (c1 > c2) 1 else 0));
            },
            .I64GtU => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, @as(u64, if (c1 > c2) 1 else 0));
            },
            .I64LeS => {
                const c2 = try self.popOperand(i64);
                const c1 = try self.popOperand(i64);
                try self.pushOperand(u64, @as(u64, if (c1 <= c2) 1 else 0));
            },
            .I64LeU => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, @as(u64, if (c1 <= c2) 1 else 0));
            },
            .I64GeS => {
                const c2 = try self.popOperand(i64);
                const c1 = try self.popOperand(i64);
                try self.pushOperand(u64, @as(u64, if (c1 >= c2) 1 else 0));
            },
            .I64GeU => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, @as(u64, if (c1 >= c2) 1 else 0));
            },
            .F32Eq => {
                const c2 = try self.popOperand(f32);
                const c1 = try self.popOperand(f32);
                try self.pushOperand(u64, @as(u64, if (c1 == c2) 1 else 0));
            },
            .F32Ne => {
                const c2 = try self.popOperand(f32);
                const c1 = try self.popOperand(f32);
                try self.pushOperand(u64, @as(u64, if (c1 != c2) 1 else 0));
            },
            .F32Lt => {
                const c2 = try self.popOperand(f32);
                const c1 = try self.popOperand(f32);
                try self.pushOperand(u64, @as(u64, if (c1 < c2) 1 else 0));
            },
            .F32Gt => {
                const c2 = try self.popOperand(f32);
                const c1 = try self.popOperand(f32);
                try self.pushOperand(u64, @as(u64, if (c1 > c2) 1 else 0));
            },
            .F32Le => {
                const c2 = try self.popOperand(f32);
                const c1 = try self.popOperand(f32);
                try self.pushOperand(u64, @as(u64, if (c1 <= c2) 1 else 0));
            },
            .F32Ge => {
                const c2 = try self.popOperand(f32);
                const c1 = try self.popOperand(f32);
                try self.pushOperand(u64, @as(u64, if (c1 >= c2) 1 else 0));
            },
            .F64Eq => {
                const c2 = try self.popOperand(f64);
                const c1 = try self.popOperand(f64);
                try self.pushOperand(u64, @as(u64, if (c1 == c2) 1 else 0));
            },
            .F64Ne => {
                const c2 = try self.popOperand(f64);
                const c1 = try self.popOperand(f64);
                try self.pushOperand(u64, @as(u64, if (c1 != c2) 1 else 0));
            },
            .F64Lt => {
                const c2 = try self.popOperand(f64);
                const c1 = try self.popOperand(f64);
                try self.pushOperand(u64, @as(u64, if (c1 < c2) 1 else 0));
            },
            .F64Gt => {
                const c2 = try self.popOperand(f64);
                const c1 = try self.popOperand(f64);
                try self.pushOperand(u64, @as(u64, if (c1 > c2) 1 else 0));
            },
            .F64Le => {
                const c2 = try self.popOperand(f64);
                const c1 = try self.popOperand(f64);
                try self.pushOperand(u64, @as(u64, if (c1 <= c2) 1 else 0));
            },
            .F64Ge => {
                const c2 = try self.popOperand(f64);
                const c1 = try self.popOperand(f64);
                try self.pushOperand(u64, @as(u64, if (c1 >= c2) 1 else 0));
            },
            .I32Clz => {
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, @clz(u32, c1));
            },
            .I32Ctz => {
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, @ctz(u32, c1));
            },
            .I32Popcnt => {
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, @popCount(u32, c1));
            },
            .I32Add => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, c1 +% c2);
            },
            .I32Sub => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, c1 -% c2);
            },
            .I32Mul => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, c1 *% c2);
            },
            .I32DivS => {
                const c2 = try self.popOperand(i32);
                const c1 = try self.popOperand(i32);
                try self.pushOperand(i32, try math.divTrunc(i32, c1, c2));
            },
            .I32DivU => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, try math.divTrunc(u32, c1, c2));
            },
            .I32RemS => {
                const c2 = try self.popOperand(i32);
                const c1 = try self.popOperand(i32);
                try self.pushOperand(i32, try math.rem(i32, c1, try math.absInt(c2)));
            },
            .I32RemU => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, try math.rem(u32, c1, c2));
            },
            .I32And => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, c1 & c2);
            },
            .I32Or => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, c1 | c2);
            },
            .I32Xor => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, c1 ^ c2);
            },
            .I32Shl => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, math.shl(u32, c1, c2 % 32));
            },
            .I32ShrS => {
                const c2 = try self.popOperand(i32);
                const c1 = try self.popOperand(i32);
                try self.pushOperand(i32, math.shr(i32, c1, try math.mod(i32, c2, 32)));
            },
            .I32ShrU => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, math.shr(u32, c1, c2 % 32));
            },
            .I32Rotl => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, math.rotl(u32, c1, c2 % 32));
            },
            .I32Rotr => {
                const c2 = try self.popOperand(u32);
                const c1 = try self.popOperand(u32);
                try self.pushOperand(u32, math.rotr(u32, c1, c2 % 32));
            },
            .I64Clz => {
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, @clz(u64, c1));
            },
            .I64Ctz => {
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, @ctz(u64, c1));
            },
            .I64Popcnt => {
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, @popCount(u64, c1));
            },
            .I64Add => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, c1 +% c2);
            },
            .I64Sub => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, c1 -% c2);
            },
            .I64Mul => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, c1 *% c2);
            },
            .I64DivS => {
                const c2 = try self.popOperand(i64);
                const c1 = try self.popOperand(i64);
                try self.pushOperand(i64, try math.divTrunc(i64, c1, c2));
            },
            .I64DivU => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, try math.divTrunc(u64, c1, c2));
            },
            .I64RemS => {
                const c2 = try self.popOperand(i64);
                const c1 = try self.popOperand(i64);
                try self.pushOperand(i64, try math.rem(i64, c1, try math.absInt(c2)));
            },
            .I64RemU => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, try math.rem(u64, c1, c2));
            },
            .I64And => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, c1 & c2);
            },
            .I64Or => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, c1 | c2);
            },
            .I64Xor => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, c1 ^ c2);
            },
            .I64Shl => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, math.shl(u64, c1, c2 % 64));
            },
            .I64ShrS => {
                const c2 = try self.popOperand(i64);
                const c1 = try self.popOperand(i64);
                try self.pushOperand(i64, math.shr(i64, c1, try math.mod(i64, c2, 64)));
            },
            .I64ShrU => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, math.shr(u64, c1, c2 % 64));
            },
            .I64Rotl => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, math.rotl(u64, c1, c2 % 64));
            },
            .I64Rotr => {
                const c2 = try self.popOperand(u64);
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, math.rotr(u64, c1, c2 % 64));
            },
            .F32Abs => {
                const c1 = try self.popOperand(f32);
                try self.pushOperand(f32, math.fabs(c1));
            },
            .F32Neg => {
                const c1 = try self.popOperand(f32);
                try self.pushOperand(f32, -c1);
            },
            .F32Ceil => {
                const c1 = try self.popOperand(f32);
                try self.pushOperand(f32, @ceil(c1));
            },
            .F32Floor => {
                const c1 = try self.popOperand(f32);
                try self.pushOperand(f32, @floor(c1));
            },
            .F32Trunc => {
                const c1 = try self.popOperand(f32);
                try self.pushOperand(f32, @trunc(c1));
            },
            .F32Nearest => {
                const c1 = try self.popOperand(f32);
                const floor = @floor(c1);
                const ceil = @ceil(c1);

                if (ceil - c1 == c1 - floor) {
                    if (@mod(ceil, 2) == 0) {
                        try self.pushOperand(f32, ceil);
                    } else {
                        try self.pushOperand(f32, floor);
                    }
                } else {
                    try self.pushOperand(f32, @round(c1));
                }
            },
            .F32Sqrt => {
                const c1 = try self.popOperand(f32);
                try self.pushOperand(f32, math.sqrt(c1));
            },
            .F32Add => {
                const c2 = try self.popOperand(f32);
                const c1 = try self.popOperand(f32);
                try self.pushOperand(f32, c1 + c2);
            },
            .F32Sub => {
                const c2 = try self.popOperand(f32);
                const c1 = try self.popOperand(f32);
                try self.pushOperand(f32, c1 - c2);
            },
            .F32Mul => {
                const c2 = try self.popOperand(f32);
                const c1 = try self.popOperand(f32);
                try self.pushOperand(f32, c1 * c2);
            },
            .F32Div => {
                const c2 = try self.popOperand(f32);
                const c1 = try self.popOperand(f32);
                try self.pushOperand(f32, c1 / c2);
            },
            .F32Min => {
                const c2 = try self.popOperand(f32);
                const c1 = try self.popOperand(f32);

                if (math.isNan(c1)) {
                    try self.pushOperand(f32, math.nan_f32);
                    return;
                }
                if (math.isNan(c2)) {
                    try self.pushOperand(f32, math.nan_f32);
                    return;
                }

                // There's surely a better way of doing this
                if (c1 == 0.0 and c2 == 0.0) {
                    if (@bitCast(u32, c1) == 0x80000000) {
                        try self.pushOperand(f32, c1);
                    } else {
                        try self.pushOperand(f32, c2);
                    }
                } else {
                    try self.pushOperand(f32, math.min(c1, c2));
                }
            },
            .F32Max => {
                const c2 = try self.popOperand(f32);
                const c1 = try self.popOperand(f32);

                if (math.isNan(c1)) {
                    try self.pushOperand(f32, math.nan_f32);
                    return;
                }
                if (math.isNan(c2)) {
                    try self.pushOperand(f32, math.nan_f32);
                    return;
                }

                // There's surely a better way of doing this
                if (c1 == 0.0 and c2 == 0.0) {
                    if (@bitCast(u32, c1) == 0x80000000) {
                        try self.pushOperand(f32, c2);
                    } else {
                        try self.pushOperand(f32, c1);
                    }
                } else {
                    try self.pushOperand(f32, math.max(c1, c2));
                }
            },
            .F32CopySign => {
                const c2 = try self.popOperand(f32);
                const c1 = try self.popOperand(f32);

                if (math.signbit(c2)) {
                    try self.pushOperand(f32, -math.fabs(c1));
                } else {
                    try self.pushOperand(f32, math.fabs(c1));
                }
            },
            .F64Abs => {
                const c1 = try self.popOperand(f64);
                try self.pushOperand(f64, math.fabs(c1));
            },
            .F64Neg => {
                const c1 = try self.popOperand(f64);
                try self.pushOperand(f64, -c1);
            },
            .F64Ceil => {
                const c1 = try self.popOperand(f64);
                try self.pushOperand(f64, @ceil(c1));
            },
            .F64Floor => {
                const c1 = try self.popOperand(f64);
                try self.pushOperand(f64, @floor(c1));
            },
            .F64Trunc => {
                const c1 = try self.popOperand(f64);
                try self.pushOperand(f64, @trunc(c1));
            },
            .F64Nearest => {
                const c1 = try self.popOperand(f64);
                const floor = @floor(c1);
                const ceil = @ceil(c1);

                if (ceil - c1 == c1 - floor) {
                    if (@mod(ceil, 2) == 0) {
                        try self.pushOperand(f64, ceil);
                    } else {
                        try self.pushOperand(f64, floor);
                    }
                } else {
                    try self.pushOperand(f64, @round(c1));
                }
            },
            .F64Sqrt => {
                const c1 = try self.popOperand(f64);
                try self.pushOperand(f64, math.sqrt(c1));
            },
            .F64Add => {
                const c2 = try self.popOperand(f64);
                const c1 = try self.popOperand(f64);
                try self.pushOperand(f64, c1 + c2);
            },
            .F64Sub => {
                const c2 = try self.popOperand(f64);
                const c1 = try self.popOperand(f64);
                try self.pushOperand(f64, c1 - c2);
            },
            .F64Mul => {
                const c2 = try self.popOperand(f64);
                const c1 = try self.popOperand(f64);
                try self.pushOperand(f64, c1 * c2);
            },
            .F64Div => {
                const c2 = try self.popOperand(f64);
                const c1 = try self.popOperand(f64);
                try self.pushOperand(f64, c1 / c2);
            },
            .F64Min => {
                const c2 = try self.popOperand(f64);
                const c1 = try self.popOperand(f64);

                if (math.isNan(c1)) {
                    try self.pushOperand(f64, math.nan_f64);
                    return;
                }
                if (math.isNan(c2)) {
                    try self.pushOperand(f64, math.nan_f64);
                    return;
                }

                // There's surely a better way of doing this
                if (c1 == 0.0 and c2 == 0.0) {
                    if (@bitCast(u64, c1) == 0x8000000000000000) {
                        try self.pushOperand(f64, c1);
                    } else {
                        try self.pushOperand(f64, c2);
                    }
                } else {
                    try self.pushOperand(f64, math.min(c1, c2));
                }
            },
            .F64Max => {
                const c2 = try self.popOperand(f64);
                const c1 = try self.popOperand(f64);

                if (math.isNan(c1)) {
                    try self.pushOperand(f64, math.nan_f64);
                    return;
                }
                if (math.isNan(c2)) {
                    try self.pushOperand(f64, math.nan_f64);
                    return;
                }

                // There's surely a better way of doing this
                if (c1 == 0.0 and c2 == 0.0) {
                    if (@bitCast(u64, c1) == 0x8000000000000000) {
                        try self.pushOperand(f64, c2);
                    } else {
                        try self.pushOperand(f64, c1);
                    }
                } else {
                    try self.pushOperand(f64, math.max(c1, c2));
                }
            },
            .F64CopySign => {
                const c2 = try self.popOperand(f64);
                const c1 = try self.popOperand(f64);

                if (math.signbit(c2)) {
                    try self.pushOperand(f64, -math.fabs(c1));
                } else {
                    try self.pushOperand(f64, math.fabs(c1));
                }
            },
            .I32WrapI64 => {
                const c1 = try self.popOperand(i64);
                try self.pushOperand(i32, @truncate(i32, c1));
            },
            .I32TruncF32S => {
                const c1 = try self.popOperand(f32);
                if (math.isNan(c1)) return error.InvalidConversion;
                const trunc = @trunc(c1);

                if (trunc >= @intToFloat(f32, std.math.maxInt(i32))) return error.Overflow;
                if (trunc < @intToFloat(f32, std.math.minInt(i32))) return error.Overflow;

                try self.pushOperand(i32, @floatToInt(i32, trunc));
            },
            .I32TruncF32U => {
                const c1 = try self.popOperand(f32);
                if (math.isNan(c1)) return error.InvalidConversion;
                const trunc = @trunc(c1);

                if (trunc >= @intToFloat(f32, std.math.maxInt(u32))) return error.Overflow;
                if (trunc < @intToFloat(f32, std.math.minInt(u32))) return error.Overflow;

                try self.pushOperand(u32, @floatToInt(u32, trunc));
            },
            .I32TruncF64S => {
                const c1 = try self.popOperand(f64);
                if (math.isNan(c1)) return error.InvalidConversion;
                const trunc = @trunc(c1);

                if (trunc > @intToFloat(f64, std.math.maxInt(i32))) return error.Overflow;
                if (trunc < @intToFloat(f64, std.math.minInt(i32))) return error.Overflow;

                try self.pushOperand(i32, @floatToInt(i32, trunc));
            },
            .I32TruncF64U => {
                const c1 = try self.popOperand(f64);
                if (math.isNan(c1)) return error.InvalidConversion;
                const trunc = @trunc(c1);

                if (trunc > @intToFloat(f64, std.math.maxInt(u32))) return error.Overflow;
                if (trunc < @intToFloat(f64, std.math.minInt(u32))) return error.Overflow;

                try self.pushOperand(u32, @floatToInt(u32, trunc));
            },
            .I64ExtendI32S => {
                const c1 = try self.popOperand(i64);
                try self.pushOperand(i64, @truncate(i32, c1));
            },
            .I64ExtendI32U => {
                const c1 = try self.popOperand(u64);
                try self.pushOperand(u64, @truncate(u32, c1));
            },
            .I64TruncF32S => {
                const c1 = try self.popOperand(f32);
                if (math.isNan(c1)) return error.InvalidConversion;
                const trunc = @trunc(c1);

                if (trunc >= @intToFloat(f32, std.math.maxInt(i64))) return error.Overflow;
                if (trunc < @intToFloat(f32, std.math.minInt(i64))) return error.Overflow;

                try self.pushOperand(i64, @floatToInt(i64, trunc));
            },
            .I64TruncF32U => {
                const c1 = try self.popOperand(f32);
                if (math.isNan(c1)) return error.InvalidConversion;
                const trunc = @trunc(c1);

                if (trunc >= @intToFloat(f32, std.math.maxInt(u64))) return error.Overflow;
                if (trunc < @intToFloat(f32, std.math.minInt(u64))) return error.Overflow;

                try self.pushOperand(u64, @floatToInt(u64, trunc));
            },
            .I64TruncF64S => {
                const c1 = try self.popOperand(f64);
                if (math.isNan(c1)) return error.InvalidConversion;
                const trunc = @trunc(c1);

                if (trunc >= @intToFloat(f64, std.math.maxInt(i64))) return error.Overflow;
                if (trunc < @intToFloat(f64, std.math.minInt(i64))) return error.Overflow;

                try self.pushOperand(i64, @floatToInt(i64, trunc));
            },
            .I64TruncF64U => {
                const c1 = try self.popOperand(f64);
                if (math.isNan(c1)) return error.InvalidConversion;
                const trunc = @trunc(c1);

                if (trunc >= @intToFloat(f64, std.math.maxInt(u64))) return error.Overflow;
                if (trunc < @intToFloat(f64, std.math.minInt(u64))) return error.Overflow;

                try self.pushOperand(u64, @floatToInt(u64, trunc));
            },
            .F32ConvertI32S => {
                const c1 = try self.popOperand(i32);
                try self.pushOperand(f32, @intToFloat(f32, c1));
            },
            .F32ConvertI32U => {
                const c1 = try self.popOperand(u32);
                try self.pushOperand(f32, @intToFloat(f32, c1));
            },
            .F32ConvertI64S => {
                const c1 = try self.popOperand(i64);
                try self.pushOperand(f32, @intToFloat(f32, c1));
            },
            .F32ConvertI64U => {
                const c1 = try self.popOperand(u64);
                try self.pushOperand(f32, @intToFloat(f32, c1));
            },
            .F32DemoteF64 => {
                const c1 = try self.popOperand(f64);
                try self.pushOperand(f32, @floatCast(f32, c1));
            },
            .F64ConvertI32S => {
                const c1 = try self.popOperand(i32);
                try self.pushOperand(f64, @intToFloat(f64, c1));
            },
            .F64ConvertI32U => {
                const c1 = try self.popOperand(u32);
                try self.pushOperand(f64, @intToFloat(f64, c1));
            },
            .F64ConvertI64S => {
                const c1 = try self.popOperand(i64);
                try self.pushOperand(f64, @intToFloat(f64, c1));
            },
            .F64ConvertI64U => {
                const c1 = try self.popOperand(u64);
                try self.pushOperand(f64, @intToFloat(f64, c1));
            },
            .F64PromoteF32 => {
                const c1 = try self.popOperand(f32);
                try self.pushOperand(f64, @floatCast(f64, c1));
            },
            .I32ReinterpretF32 => {
                const c1 = try self.popOperand(f32);
                try self.pushOperand(i32, @bitCast(i32, c1));
            },
            .I64ReinterpretF64 => {
                const c1 = try self.popOperand(f64);
                try self.pushOperand(i64, @bitCast(i64, c1));
            },
            .F32ReinterpretI32 => {
                const c1 = try self.popOperand(i32);
                try self.pushOperand(f32, @bitCast(f32, c1));
            },
            .F64ReinterpretI64 => {
                const c1 = try self.popOperand(f64);
                try self.pushOperand(i64, @bitCast(i64, c1));
            },
            .I32Extend8S => {
                const c1 = try self.popOperand(i32);
                try self.pushOperand(i32, @truncate(i8, c1));
            },
            .I32Extend16S => {
                const c1 = try self.popOperand(i32);
                try self.pushOperand(i32, @truncate(i16, c1));
            },
            .I64Extend8S => {
                const c1 = try self.popOperand(i64);
                try self.pushOperand(i64, @truncate(i8, c1));
            },
            .I64Extend16S => {
                const c1 = try self.popOperand(i64);
                try self.pushOperand(i64, @truncate(i16, c1));
            },
            .I64Extend32S => {
                const c1 = try self.popOperand(i64);
                try self.pushOperand(i64, @truncate(i32, c1));
            },
            .TruncSat => {
                const trunc_type = try instruction.readULEB128Mem(u32, &self.continuation);
                switch (trunc_type) {
                    0 => {
                        const c1 = try self.popOperand(f32);
                        const trunc = @trunc(c1);

                        if (math.isNan(c1)) {
                            try self.pushOperand(i32, 0);
                            return;
                        }

                        if (trunc >= @intToFloat(f32, std.math.maxInt(i32))) {
                            try self.pushOperand(i32, @bitCast(i32, @as(u32, 0x7fffffff)));
                            return;
                        }
                        if (trunc < @intToFloat(f32, std.math.minInt(i32))) {
                            try self.pushOperand(i32, @bitCast(i32, @as(u32, 0x80000000)));
                            return;
                        }

                        try self.pushOperand(i32, @floatToInt(i32, trunc));
                    },
                    1 => {
                        const c1 = try self.popOperand(f32);
                        const trunc = @trunc(c1);

                        if (math.isNan(c1)) {
                            try self.pushOperand(u32, 0);
                            return;
                        }

                        if (trunc >= @intToFloat(f32, std.math.maxInt(u32))) {
                            try self.pushOperand(u32, @bitCast(u32, @as(u32, 0xffffffff)));
                            return;
                        }
                        if (trunc < @intToFloat(f32, std.math.minInt(u32))) {
                            try self.pushOperand(u32, @bitCast(u32, @as(u32, 0x00000000)));
                            return;
                        }

                        try self.pushOperand(u32, @floatToInt(u32, trunc));
                    },
                    2 => {
                        const c1 = try self.popOperand(f64);
                        const trunc = @trunc(c1);

                        if (math.isNan(c1)) {
                            try self.pushOperand(i32, 0);
                            return;
                        }

                        if (trunc >= @intToFloat(f64, std.math.maxInt(i32))) {
                            try self.pushOperand(i32, @bitCast(i32, @as(u32, 0x7fffffff)));
                            return;
                        }
                        if (trunc < @intToFloat(f64, std.math.minInt(i32))) {
                            try self.pushOperand(i32, @bitCast(i32, @as(u32, 0x80000000)));
                            return;
                        }

                        try self.pushOperand(i32, @floatToInt(i32, trunc));
                    },
                    3 => {
                        const c1 = try self.popOperand(f64);
                        const trunc = @trunc(c1);

                        if (math.isNan(c1)) {
                            try self.pushOperand(u32, 0);
                            return;
                        }

                        if (trunc >= @intToFloat(f64, std.math.maxInt(u32))) {
                            try self.pushOperand(u32, @bitCast(u32, @as(u32, 0xffffffff)));
                            return;
                        }
                        if (trunc < @intToFloat(f64, std.math.minInt(u32))) {
                            try self.pushOperand(u32, @bitCast(u32, @as(u32, 0x00000000)));
                            return;
                        }

                        try self.pushOperand(u32, @floatToInt(u32, trunc));
                    },
                    4 => {
                        const c1 = try self.popOperand(f32);
                        const trunc = @trunc(c1);

                        if (math.isNan(c1)) {
                            try self.pushOperand(i64, 0);
                            return;
                        }

                        if (trunc >= @intToFloat(f32, std.math.maxInt(i64))) {
                            try self.pushOperand(i64, @bitCast(i64, @as(u64, 0x7fffffffffffffff)));
                            return;
                        }
                        if (trunc < @intToFloat(f32, std.math.minInt(i64))) {
                            try self.pushOperand(i64, @bitCast(i64, @as(u64, 0x8000000000000000)));
                            return;
                        }

                        try self.pushOperand(i64, @floatToInt(i64, trunc));
                    },
                    5 => {
                        const c1 = try self.popOperand(f32);
                        const trunc = @trunc(c1);

                        if (math.isNan(c1)) {
                            try self.pushOperand(u64, 0);
                            return;
                        }

                        if (trunc >= @intToFloat(f32, std.math.maxInt(u64))) {
                            try self.pushOperand(u64, @bitCast(u64, @as(u64, 0xffffffffffffffff)));
                            return;
                        }
                        if (trunc < @intToFloat(f32, std.math.minInt(u64))) {
                            try self.pushOperand(u64, @bitCast(u64, @as(u64, 0x0000000000000000)));
                            return;
                        }

                        try self.pushOperand(u64, @floatToInt(u64, trunc));
                    },
                    6 => {
                        const c1 = try self.popOperand(f64);
                        const trunc = @trunc(c1);

                        if (math.isNan(c1)) {
                            try self.pushOperand(i64, 0);
                            return;
                        }

                        if (trunc >= @intToFloat(f64, std.math.maxInt(i64))) {
                            try self.pushOperand(i64, @bitCast(i64, @as(u64, 0x7fffffffffffffff)));
                            return;
                        }
                        if (trunc < @intToFloat(f64, std.math.minInt(i64))) {
                            try self.pushOperand(i64, @bitCast(i64, @as(u64, 0x8000000000000000)));
                            return;
                        }

                        try self.pushOperand(i64, @floatToInt(i64, trunc));
                    },
                    7 => {
                        const c1 = try self.popOperand(f64);
                        const trunc = @trunc(c1);

                        if (math.isNan(c1)) {
                            try self.pushOperand(u64, 0);
                            return;
                        }

                        if (trunc >= @intToFloat(f64, std.math.maxInt(u64))) {
                            try self.pushOperand(u64, @bitCast(u64, @as(u64, 0xffffffffffffffff)));
                            return;
                        }
                        if (trunc < @intToFloat(f64, std.math.minInt(u64))) {
                            try self.pushOperand(u64, @bitCast(u64, @as(u64, 0x0000000000000000)));
                            return;
                        }

                        try self.pushOperand(u64, @floatToInt(u64, trunc));
                    },
                    else => return error.Trap,
                }
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
            i32 => @as(u64, @bitCast(u32, value)),
            i64 => @bitCast(u64, value),
            f32 => @as(u64, @bitCast(u32, value)),
            f64 => @bitCast(u64, value),
            u32 => @as(u64, value), // TODO: figure out types
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
    pub fn pushFrame(self: *Interpreter, frame: Frame, params_and_locals_count: usize) !void {
        if (self.frame_stack.len == self.frame_stack_mem.len) return error.ControlStackOverflow;
        self.frame_stack = self.frame_stack_mem[0 .. self.frame_stack.len + 1];

        const current_frame = &self.frame_stack[self.frame_stack.len - 1];
        current_frame.* = frame;
        // TODO: index out of bounds (error if we've run out of operand stack space):
        current_frame.locals = self.op_stack[frame.op_stack_len .. frame.op_stack_len + params_and_locals_count];
    }

    pub fn popFrame(self: *Interpreter) !Frame {
        if (self.frame_stack.len == 0) return error.ControlStackUnderflow;
        defer self.frame_stack = self.frame_stack[0 .. self.frame_stack.len - 1];

        return self.frame_stack[self.frame_stack.len - 1];
    }

    // peekNthFrame
    //
    // Returns nth label on the Label stack relative to the top of the stack
    //
    fn peekNthFrame(self: *Interpreter, index: u32) !*Frame {
        if (index + 1 > self.frame_stack.len) return error.ControlStackUnderflow;
        return &self.frame_stack[self.frame_stack.len - index - 1];
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

    pub const Frame = struct {
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
    var frame_stack: [1024]Interpreter.Frame = [_]Interpreter.Frame{undefined} ** 1024;
    var label_stack_mem: [1024]Interpreter.Label = [_]Interpreter.Label{undefined} ** 1024;

    var inst: Instance = undefined;
    var i = Interpreter.init(op_stack[0..], frame_stack[0..], label_stack_mem[0..], &inst);

    try i.pushOperand(i32, 22);
    try i.pushOperand(i32, -23);

    const code: [0]u8 = [_]u8{0} ** 0;

    try i.interpret(.I32Add, code[0..]);

    testing.expectEqual(@as(i32, -1), try i.popOperand(i32));
}
