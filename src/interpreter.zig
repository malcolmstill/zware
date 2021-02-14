const std = @import("std");
const mem = std.mem;
const math = std.math;
const ArrayList = std.ArrayList;
const ValueType = @import("module.zig").ValueType;
const ModuleInstance = @import("module.zig").ModuleInstance;
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
    mod_inst: *ModuleInstance = undefined,

    pub fn init(op_stack_mem: []u64, frame_stack_mem: []Frame, label_stack_mem: []Label, mod_inst: *ModuleInstance) Interpreter {
        return Interpreter{
            .op_stack_mem = op_stack_mem,
            .op_stack = op_stack_mem[0..0],
            .frame_stack_mem = frame_stack_mem,
            .frame_stack = frame_stack_mem[0..0],
            .label_stack_mem = label_stack_mem,
            .label_stack = label_stack_mem[0..0],
            .mod_inst = mod_inst,
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
        if (std.builtin.mode == .Debug) {
            errdefer {
                std.debug.warn("stack after: {}\n", .{opcode});
                var i: usize = 0;
                while (i < self.op_stack.len) : (i += 1) {
                    std.debug.warn("stack[{}] = {}\n", .{ i, self.op_stack[i] });
                }
                std.debug.warn("\n", .{});
            }
        }

        switch (opcode) {
            .Unreachable => return error.TrapUnreachable,
            .Nop => return,
            .Block => {
                const block_type = try instruction.readILEB128Mem(i32, &self.continuation);

                var block_params: usize = 0;
                var block_returns: usize = if (block_type == -0x40) 0 else 1;
                if (block_type >= 0) {
                    const func_type = self.mod_inst.module.types.items[@intCast(usize, block_type)];
                    block_params = func_type.params_count;
                    block_returns = func_type.results_count;
                }

                const end = try instruction.findEnd(code);
                const continuation = code[end.offset..];

                try self.pushLabel(Label{
                    .return_arity = block_returns,
                    .op_stack_len = self.op_stack.len - block_params, // equivalent to pop and push
                    .continuation = continuation,
                });
            },
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

                const condition = try self.popOperand(u32);
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
                if (self.continuation.len == 0) self.continuation = label.continuation;
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
                while (j < label_count + 1) : (j += 1) {
                    const tmp_label = try instruction.readULEB128Mem(u32, &self.continuation);
                    if (i == j) label = tmp_label;
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
                const module = self.mod_inst.module;
                const function_index = try instruction.readULEB128Mem(usize, &self.continuation);
                const func_type_index = module.functions.items[function_index];
                const func_type = module.types.items[func_type_index];
                const func = module.codes.items[function_index];
                const params = module.value_types.items[func_type.params_offset .. func_type.params_offset + func_type.params_count];
                const results = module.value_types.items[func_type.results_offset .. func_type.results_offset + func_type.results_count];

                // Consume parameters from the stack
                try self.pushFrame(Frame{
                    .op_stack_len = self.op_stack.len - params.len,
                    .label_stack_len = self.label_stack.len,
                    .return_arity = results.len,
                }, func.locals_count + params.len);

                // Our continuation is the code after call
                try self.pushLabel(Label{
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
                frame.locals[local_index] = value;
                try self.pushAnyOperand(value);
            },
            .GlobalGet => {
                // 1. Get index of global from immediate
                const global_index = try instruction.readULEB128Mem(u32, &self.continuation);
                // 2. Look up address of global using index. (we don't need to do this because the
                //    the global_index is also the address of the actual global in our store)
                // 3. Get value
                const value = self.mod_inst.store.globals.items[global_index];
                try self.pushAnyOperand(value);
            },
            .GlobalSet => {
                // 1. Get index of global from immediate
                const global_index = try instruction.readULEB128Mem(u32, &self.continuation);
                // 2. Look up address of global using index. (we don't need to do this because the
                //    the global_index is also the address of the actual global in our store)
                // 3. Get value
                const value = try self.popAnyOperand();
                self.mod_inst.store.globals.items[global_index] = value;
            },
            .I32Load => {
                const frame = try self.peekNthFrame(0);
                // TODO: we need to check this / handle multiple memories
                var memory = self.mod_inst.store.memories.items[0];

                const offset = try instruction.readULEB128Mem(u32, &self.continuation);
                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);

                const address = try self.popOperand(u32);

                const value = try memory.read(u32, offset + address);
                try self.pushOperand(u32, value);
            },
            .I32Store => {
                const frame = try self.peekNthFrame(0);
                // TODO: we need to check this / handle multiple memories
                var memory = self.mod_inst.store.memories.items[0];

                const offset = try instruction.readULEB128Mem(u32, &self.continuation);
                const alignment = try instruction.readULEB128Mem(u32, &self.continuation);

                const value = try self.popOperand(u32);
                const address = try self.popOperand(u32);

                try memory.write(u32, offset + address, value);
            },
            .MemorySize => {
                const frame = try self.peekNthFrame(0);

                const memory_index = try instruction.readULEB128Mem(u32, &self.continuation);
                var memory = self.mod_inst.store.memories.items[memory_index];

                try self.pushOperand(u32, @intCast(u32, memory.data.items.len));
            },
            .MemoryGrow => {
                const frame = try self.peekNthFrame(0);
                // TODO: we need to check this / handle multiple memories

                const memory_index = try instruction.readULEB128Mem(u32, &self.continuation);
                var memory = self.mod_inst.store.memories.items[memory_index];

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
                const x = try instruction.readULEB128Mem(u64, &self.continuation);
                try self.pushOperand(u64, x);
            },
            .F32Const => {
                const x = try instruction.readU32(&self.continuation);
                try self.pushOperand(f32, @intToFloat(f32, x));
            },
            .F64Const => {
                const x = try instruction.readU64(&self.continuation);
                try self.pushOperand(f64, @intToFloat(f64, x));
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
            .F32Gt => {
                const c2 = try self.popOperand(f32);
                const c1 = try self.popOperand(f32);
                try self.pushOperand(u32, @as(u32, if (c1 > c2) 1 else 0));
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
            else => {
                std.debug.warn("unimplemented instruction: {}\n", .{opcode});
                return error.UnimplementedOpcode;
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
            f32 => @bitCast(u64, @as(f64, value)),
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
            f32 => @floatCast(f32, @bitCast(f64, value)),
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

    var mod_inst: ModuleInstance = undefined;

    var i = Interpreter.init(op_stack[0..], frame_stack_mem[0..], label_stack_mem[0..], &mod_inst);

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

    var mod_inst: ModuleInstance = undefined;
    var i = Interpreter.init(op_stack[0..], frame_stack[0..], label_stack_mem[0..], &mod_inst);

    try i.pushOperand(i32, 22);
    try i.pushOperand(i32, -23);

    const code: [0]u8 = [_]u8{0} ** 0;

    try i.interpret(.I32Add, code[0..]);

    testing.expectEqual(@as(i32, -1), try i.popOperand(i32));
}
