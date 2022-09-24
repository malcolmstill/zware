const std = @import("std");
const math = std.math;
const Module = @import("module.zig").Module;
const Range = @import("common.zig").Range;
const Validator = @import("validator.zig").Validator;
const LocalType = @import("common.zig").LocalType;
const ArrayList = std.ArrayList;
const opcode = @import("opcode.zig");
const Opcode = @import("opcode.zig").Opcode;
const MiscOpcode = @import("opcode.zig").MiscOpcode;
const RefType = @import("value_type.zig").RefType;
const ValueTypeUnknown = @import("validator.zig").ValueTypeUnknown;
const valueTypeFromBlockType = @import("value_type.zig").valueTypeFromBlockType;
const ValueType = @import("value_type.zig").ValueType;
const Instruction = @import("instruction.zig").Instruction;
const MiscInstruction = @import("instruction.zig").MiscInstruction;

const EMPTY = [0]ValueType{} ** 0;
const I32_OUT = [1]ValueType{.I32} ** 1;
const I64_OUT = [1]ValueType{.I64} ** 1;
const F32_OUT = [1]ValueType{.F32} ** 1;
const F64_OUT = [1]ValueType{.F64} ** 1;
const V128_OUT = [1]ValueType{.V128} ** 1;
const FUNCREF_OUT = [1]ValueType{.FuncRef} ** 1;
const EXTERNREF_OUT = [1]ValueType{.ExternRef} ** 1;

pub const Parser = struct {
    function: []const u8,
    code: []const u8,
    code_ptr: usize,
    parsed: *ArrayList(Instruction),
    module: *Module,
    validator: Validator,
    params: ?[]const ValueType,
    locals: ?[]LocalType,
    continuation_stack: []usize,
    continuation_stack_ptr: usize,
    is_constant: bool,

    pub fn init(module: *Module, function: []const u8, parsed_code: *ArrayList(Instruction), continuation_stack: []usize, is_constant: bool) Parser {
        return Parser{
            .code = function,
            .code_ptr = parsed_code.items.len,
            .function = function,
            .parsed = parsed_code,
            .module = module,
            .params = null,
            .locals = null,
            // TODO: what type of allocator is this?
            // we want to free this every function parse, so we
            // want a general purpose allocator, not an arena allocator
            .validator = Validator.init(module.alloc, module.dataCount != null, is_constant),
            .continuation_stack = continuation_stack,
            .continuation_stack_ptr = 0,
            .is_constant = is_constant,
        };
    }

    // pushFunction initiliase the validator for the current function
    pub fn pushFunction(self: *Parser, locals: []LocalType, func_index: usize) !void {
        const function = self.module.functions.list.items[@intCast(usize, func_index)];
        const function_type = self.module.types.list.items[@intCast(usize, function.typeidx)];

        self.params = function_type.params;
        self.locals = locals;

        try self.validator.pushControlFrame(
            .nop, // block?
            function_type.params[0..0],
            function_type.results,
        );
    }

    fn pushContinuationStack(self: *Parser, offset: usize) !void {
        defer self.continuation_stack_ptr += 1;
        if (self.continuation_stack_ptr >= self.continuation_stack.len) return error.ContinuationStackOverflow;

        self.continuation_stack[self.continuation_stack_ptr] = offset;
    }

    fn peekContinuationStack(self: *Parser) usize {
        return self.continuation_stack[self.continuation_stack_ptr - 1];
    }

    fn popContinuationStack(self: *Parser) !usize {
        if (self.continuation_stack_ptr <= 0) return error.ContinuationStackUnderflow;
        self.continuation_stack_ptr -= 1;

        return self.continuation_stack[self.continuation_stack_ptr];
    }

    pub fn next(self: *Parser) !?Instruction {
        defer self.code_ptr += 1;
        if (self.code.len == 0) return null;

        // 1. Get the instruction we're going to return and increment code
        const instr = @intToEnum(Opcode, self.code[0]);
        self.code = self.code[1..];

        var rt_instr: Instruction = undefined;

        // 2. Find the start of the next instruction
        switch (instr) {
            .@"unreachable" => rt_instr = Instruction.@"unreachable",
            .nop => rt_instr = Instruction.nop,
            .block => {
                const block_type = try opcode.readILEB128Mem(i32, &self.code);

                var block_params: u16 = 0;
                var block_returns: u16 = if (block_type == -0x40) 0 else 1;

                if (block_type >= 0) {
                    const func_type = self.module.types.list.items[@intCast(usize, block_type)];
                    block_params = math.cast(u16, func_type.params.len) orelse return error.FailedCast;
                    block_returns = math.cast(u16, func_type.results.len) orelse return error.FailedCast;
                    try self.validator.validateBlock(func_type.params, func_type.results);
                } else {
                    if (block_type == -0x40) {
                        try self.validator.validateBlock(EMPTY[0..], EMPTY[0..]);
                    } else {
                        switch (try valueTypeFromBlockType(block_type)) {
                            .I32 => try self.validator.validateBlock(EMPTY[0..], I32_OUT[0..]),
                            .I64 => try self.validator.validateBlock(EMPTY[0..], I64_OUT[0..]),
                            .F32 => try self.validator.validateBlock(EMPTY[0..], F32_OUT[0..]),
                            .F64 => try self.validator.validateBlock(EMPTY[0..], F64_OUT[0..]),
                            .V128 => try self.validator.validateBlock(EMPTY[0..], V128_OUT[0..]),
                            .FuncRef => try self.validator.validateBlock(EMPTY[0..], FUNCREF_OUT[0..]),
                            .ExternRef => try self.validator.validateBlock(EMPTY[0..], EXTERNREF_OUT[0..]),
                        }
                    }
                }

                try self.pushContinuationStack(self.code_ptr);

                rt_instr = Instruction{
                    .block = .{
                        .param_arity = block_params,
                        .return_arity = block_returns,
                        .branch_target = 0,
                    },
                };
            },
            .loop => {
                const block_type = try opcode.readILEB128Mem(i32, &self.code);

                var block_params: u16 = 0;
                var block_returns: u16 = if (block_type == -0x40) 0 else 1;
                if (block_type >= 0) {
                    const func_type = self.module.types.list.items[@intCast(usize, block_type)];
                    block_params = math.cast(u16, func_type.params.len) orelse return error.FailedCast;
                    block_returns = math.cast(u16, func_type.results.len) orelse return error.FailedCast;
                    try self.validator.validateLoop(func_type.params, func_type.results);
                } else {
                    if (block_type == -0x40) {
                        try self.validator.validateLoop(EMPTY[0..], EMPTY[0..]);
                    } else {
                        switch (try valueTypeFromBlockType(block_type)) {
                            .I32 => try self.validator.validateLoop(EMPTY[0..], I32_OUT[0..]),
                            .I64 => try self.validator.validateLoop(EMPTY[0..], I64_OUT[0..]),
                            .F32 => try self.validator.validateLoop(EMPTY[0..], F32_OUT[0..]),
                            .F64 => try self.validator.validateLoop(EMPTY[0..], F64_OUT[0..]),
                            .V128 => try self.validator.validateBlock(EMPTY[0..], V128_OUT[0..]),
                            .FuncRef => try self.validator.validateBlock(EMPTY[0..], FUNCREF_OUT[0..]),
                            .ExternRef => try self.validator.validateBlock(EMPTY[0..], EXTERNREF_OUT[0..]),
                        }
                    }
                }

                try self.pushContinuationStack(self.code_ptr);

                rt_instr = Instruction{
                    .loop = .{
                        .param_arity = block_params,
                        .return_arity = block_params,
                        .branch_target = math.cast(u32, self.code_ptr) orelse return error.FailedCast,
                    },
                };
            },
            .@"if" => {
                const block_type = try opcode.readILEB128Mem(i32, &self.code);

                // 1. First assume the number of block params is 0
                // 2. The number of return values is 0 if block_type == -0x40
                //    otherwise assume temporarily that 1 value is returned
                // 3. If block_type >= 0 then we reference a function type,
                //    so look it up and update the params / returns count to match
                var block_params: u16 = 0;
                var block_returns: u16 = if (block_type == -0x40) 0 else 1;
                if (block_type >= 0) {
                    const func_type = self.module.types.list.items[@intCast(usize, block_type)];
                    block_params = math.cast(u16, func_type.params.len) orelse return error.FailedCast;
                    block_returns = math.cast(u16, func_type.results.len) orelse return error.FailedCast;
                    try self.validator.validateIf(func_type.params, func_type.results);
                } else {
                    if (block_type == -0x40) {
                        try self.validator.validateIf(EMPTY[0..], EMPTY[0..]);
                    } else {
                        switch (try valueTypeFromBlockType(block_type)) {
                            .I32 => try self.validator.validateIf(EMPTY[0..], I32_OUT[0..]),
                            .I64 => try self.validator.validateIf(EMPTY[0..], I64_OUT[0..]),
                            .F32 => try self.validator.validateIf(EMPTY[0..], F32_OUT[0..]),
                            .F64 => try self.validator.validateIf(EMPTY[0..], F64_OUT[0..]),
                            .V128 => try self.validator.validateBlock(EMPTY[0..], V128_OUT[0..]),
                            .FuncRef => try self.validator.validateBlock(EMPTY[0..], FUNCREF_OUT[0..]),
                            .ExternRef => try self.validator.validateBlock(EMPTY[0..], EXTERNREF_OUT[0..]),
                        }
                    }
                }

                try self.pushContinuationStack(self.code_ptr);

                rt_instr = Instruction{
                    .if_no_else = .{
                        .param_arity = block_params,
                        .return_arity = block_returns,
                        .branch_target = 0,
                    },
                };
            },
            .@"else" => {
                const parsed_code_offset = self.peekContinuationStack();

                switch (self.parsed.items[parsed_code_offset]) {
                    .if_no_else => |*b| {
                        self.parsed.items[parsed_code_offset] = Instruction{
                            .@"if" = .{
                                .param_arity = b.param_arity,
                                .return_arity = b.return_arity,
                                .branch_target = 0,
                                .else_ip = math.cast(u32, self.code_ptr + 1) orelse return error.FailedCast,
                            },
                        };
                    },
                    else => return error.UnexpectedInstruction,
                }

                rt_instr = Instruction.@"else";
            },
            .end => {
                // If we're not looking at the `end` of a function
                if (self.code.len != 0) {
                    const parsed_code_offset = try self.popContinuationStack();

                    switch (self.parsed.items[parsed_code_offset]) {
                        .block => |*b| b.branch_target = math.cast(u32, self.code_ptr + 1) orelse return error.FailedCast,
                        .loop => {},
                        .@"if" => |*b| {
                            b.branch_target = math.cast(u32, self.code_ptr + 1) orelse return error.FailedCast;
                        },
                        .if_no_else => |*b| {
                            // We have an if with no else, check that this works arity-wise and replace with fast if
                            if (b.param_arity -% b.return_arity != 0) return error.ValidatorElseBranchExpected;

                            b.branch_target = math.cast(u32, self.code_ptr + 1) orelse return error.FailedCast;
                        },
                        else => return error.UnexpectedInstruction,
                    }
                }

                rt_instr = Instruction.end;
            },
            .br => {
                const label = try opcode.readULEB128Mem(u32, &self.code);
                try self.validator.validateBr(label);
                rt_instr = Instruction{ .br = label };
            },
            .br_if => {
                const label = try opcode.readULEB128Mem(u32, &self.code);
                try self.validator.validateBrIf(label);
                rt_instr = Instruction{ .br_if = label };
            },
            .br_table => {
                const label_start = self.module.br_table_indices.items.len;
                const label_count = try opcode.readULEB128Mem(u32, &self.code);

                var j: usize = 0;
                while (j < label_count) : (j += 1) {
                    const tmp_label = try opcode.readULEB128Mem(u32, &self.code);
                    try self.module.br_table_indices.append(tmp_label);
                }
                const ln = try opcode.readULEB128Mem(u32, &self.code);
                const l_star = self.module.br_table_indices.items[label_start .. label_start + j];

                try self.validator.validateBrTable(l_star, ln);

                rt_instr = Instruction{
                    .br_table = .{
                        .ls = Range{ .offset = label_start, .count = label_count },
                        .ln = ln,
                    },
                };
            },
            .@"return" => rt_instr = Instruction.@"return",
            .call => {
                const function_index = try opcode.readULEB128Mem(u32, &self.code);
                if (function_index >= self.module.functions.list.items.len) return error.ValidatorCallInvalidFunctionIndex;
                const function = self.module.functions.list.items[@intCast(usize, function_index)];
                const function_type = self.module.types.list.items[@intCast(usize, function.typeidx)];

                try self.validator.validateCall(function_type);

                rt_instr = Instruction{ .call = function_index };
                // TODO: do the replacement at instantiate-time for a fastcall if in same module?
                // rt_instr = Instruction{ .fast_call = .{ .ip_start = 0, .params = 1, .locals = 0, .results = 1 } };
            },
            .call_indirect => {
                const type_index = try opcode.readULEB128Mem(u32, &self.code);
                if (type_index >= self.module.types.list.items.len) return error.ValidatorCallIndirectInvalidTypeIndex;

                const tableidx = try opcode.readByte(&self.code);
                if (tableidx >= self.module.tables.list.items.len) return error.ValidatorCallIndirectNoTable;

                const function_type = self.module.types.list.items[@intCast(usize, type_index)];
                try self.validator.validateCallIndirect(function_type);

                rt_instr = Instruction{
                    .call_indirect = .{
                        .@"type" = type_index,
                        .table = tableidx,
                    },
                };
            },
            .drop => rt_instr = Instruction.drop,
            .select => rt_instr = Instruction.select,
            .select_t => {
                const type_count = try opcode.readULEB128Mem(u32, &self.code);
                if (type_count != 1) return error.OnlyOneSelectTTypeSupported; // Future versions may support more than one
                const valuetype_raw = try opcode.readULEB128Mem(i32, &self.code);
                const valuetype = try std.meta.intToEnum(ValueType, valuetype_raw);

                try self.validator.validateSelectT(valuetype);
                rt_instr = Instruction.select;
            },
            .@"global.get" => {
                const index = try opcode.readULEB128Mem(u32, &self.code);

                // TODO: add a getGlobal to module?
                if (index >= self.module.globals.list.items.len) return error.ValidatorUnknownGlobal;
                const global = self.module.globals.list.items[@intCast(usize, index)];
                try self.validator.validateGlobalGet(global);

                rt_instr = Instruction{ .@"global.get" = index };
            },
            .@"global.set" => {
                const index = try opcode.readULEB128Mem(u32, &self.code);

                if (index >= self.module.globals.list.items.len) return error.ValidatorUnknownGlobal;

                const global = self.module.globals.list.items[@intCast(usize, index)];
                try self.validator.validateGlobalSet(global);

                rt_instr = Instruction{ .@"global.set" = index };
            },
            .@"table.get" => {
                const tableidx = try opcode.readULEB128Mem(u32, &self.code);

                if (tableidx >= self.module.tables.list.items.len) return error.ValidatorUnknownTable;

                const table = self.module.tables.list.items[@intCast(usize, tableidx)];
                const reftype: ValueType = switch (table.reftype) {
                    .FuncRef => .FuncRef,
                    .ExternRef => .ExternRef,
                };

                _ = try self.validator.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                _ = try self.validator.pushOperand(ValueTypeUnknown{ .Known = reftype });

                rt_instr = Instruction{ .@"table.get" = tableidx };
            },
            .@"table.set" => {
                const tableidx = try opcode.readULEB128Mem(u32, &self.code);

                if (tableidx >= self.module.tables.list.items.len) return error.ValidatorUnknownTable;

                const table = self.module.tables.list.items[@intCast(usize, tableidx)];
                const reftype: ValueType = switch (table.reftype) {
                    .FuncRef => .FuncRef,
                    .ExternRef => .ExternRef,
                };

                _ = try self.validator.popOperandExpecting(ValueTypeUnknown{ .Known = reftype });
                _ = try self.validator.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });

                rt_instr = Instruction{ .@"table.set" = tableidx };
            },
            .@"local.get" => {
                const index = try opcode.readULEB128Mem(u32, &self.code);
                const params = self.params orelse return error.ValidatorConstantExpressionRequired;
                const locals = self.locals orelse return error.ValidatorConstantExpressionRequired;

                if (index < params.len) {
                    try self.validator.validateLocalGet(params[index]);
                } else {
                    const local_index = index - params.len;
                    var local_type: ?ValueType = null;
                    var count: usize = 0;
                    for (locals) |l| {
                        if (local_index < count + l.count) {
                            local_type = l.value_type;
                            break;
                        }
                        count += l.count;
                    }

                    if (local_type) |ltype| {
                        try self.validator.validateLocalGet(ltype);
                    } else {
                        return error.LocalGetIndexOutOfBounds;
                    }
                }

                rt_instr = Instruction{ .@"local.get" = index };
            },
            .@"local.set" => {
                const index = try opcode.readULEB128Mem(u32, &self.code);

                const params = self.params orelse return error.ValidatorConstantExpressionRequired;
                const locals = self.locals orelse return error.ValidatorConstantExpressionRequired;

                if (index < params.len) {
                    try self.validator.validateLocalSet(params[index]);
                } else {
                    const local_index = index - params.len;
                    var local_type: ?ValueType = null;
                    var count: usize = 0;
                    for (locals) |l| {
                        if (local_index < count + l.count) {
                            local_type = l.value_type;
                            break;
                        }
                        count += l.count;
                    }

                    if (local_type) |ltype| {
                        try self.validator.validateLocalSet(ltype);
                    } else {
                        return error.LocalSetIndexOutOfBounds;
                    }
                }

                rt_instr = Instruction{ .@"local.set" = index };
            },
            .@"local.tee" => {
                const index = try opcode.readULEB128Mem(u32, &self.code);

                const params = self.params orelse return error.ValidatorConstantExpressionRequired;
                const locals = self.locals orelse return error.ValidatorConstantExpressionRequired;

                if (index < params.len) {
                    try self.validator.validateLocalTee(params[index]);
                } else {
                    const local_index = index - params.len;
                    var local_type: ?ValueType = null;
                    var count: usize = 0;
                    for (locals) |l| {
                        if (local_index < count + l.count) {
                            local_type = l.value_type;
                            break;
                        }
                        count += l.count;
                    }

                    if (local_type) |ltype| {
                        try self.validator.validateLocalTee(ltype);
                    } else {
                        return error.LocalTeeIndexOutOfBounds;
                    }
                }

                rt_instr = Instruction{ .@"local.tee" = index };
            },
            .@"memory.size" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const memory_index = try opcode.readByte(&self.code);
                if (memory_index != 0) return error.MalformedMemoryReserved;

                rt_instr = Instruction{ .@"memory.size" = memory_index };
            },
            .@"memory.grow" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const memory_index = try opcode.readByte(&self.code);
                if (memory_index != 0) return error.MalformedMemoryReserved;

                rt_instr = Instruction{ .@"memory.grow" = memory_index };
            },
            .@"i32.const" => {
                const i32_const = try opcode.readILEB128Mem(i32, &self.code);
                rt_instr = Instruction{ .@"i32.const" = i32_const };
            },
            .@"i64.const" => {
                const i64_const = try opcode.readILEB128Mem(i64, &self.code);
                rt_instr = Instruction{ .@"i64.const" = i64_const };
            },
            .@"f32.const" => {
                const float_const = @bitCast(f32, try opcode.readU32(&self.code));
                rt_instr = Instruction{ .@"f32.const" = float_const };
            },
            .@"f64.const" => {
                const float_const = @bitCast(f64, try opcode.readU64(&self.code));
                rt_instr = Instruction{ .@"f64.const" = float_const };
            },
            .@"i32.load" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;

                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 32) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i32.load" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 64) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i64.load" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"f32.load" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 32) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"f32.load" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"f64.load" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 64) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"f64.load" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.load8_s" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 8) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i32.load8_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.load8_u" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 8) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i32.load8_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.load16_s" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 16) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i32.load16_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.load16_u" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 16) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i32.load16_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load8_s" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 8) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i64.load8_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load8_u" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 8) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i64.load8_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load16_s" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 16) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i64.load16_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load16_u" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 16) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i64.load16_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load32_s" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 32) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i64.load32_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load32_u" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 32) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i64.load32_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.store" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 32) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i32.store" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.store" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 64) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i64.store" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"f32.store" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 32) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"f32.store" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"f64.store" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 64) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"f64.store" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.store8" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 8) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i32.store8" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.store16" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 16) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i32.store16" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.store8" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 8) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i64.store8" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.store16" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 16) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i64.store16" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.store32" => {
                const alignment = try opcode.readULEB128Mem(u32, &self.code);
                const offset = try opcode.readULEB128Mem(u32, &self.code);

                if (8 * try std.math.powi(u32, 2, alignment) > 32) return error.InvalidAlignment;

                rt_instr = Instruction{
                    .@"i64.store32" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.eqz" => rt_instr = Instruction.@"i32.eqz",
            .@"i32.eq" => rt_instr = Instruction.@"i32.eq",
            .@"i32.ne" => rt_instr = Instruction.@"i32.ne",
            .@"i32.lt_s" => rt_instr = Instruction.@"i32.lt_s",
            .@"i32.lt_u" => rt_instr = Instruction.@"i32.lt_u",
            .@"i32.gt_s" => rt_instr = Instruction.@"i32.gt_s",
            .@"i32.gt_u" => rt_instr = Instruction.@"i32.gt_u",
            .@"i32.le_s" => rt_instr = Instruction.@"i32.le_s",
            .@"i32.le_u" => rt_instr = Instruction.@"i32.le_u",
            .@"i32.ge_s" => rt_instr = Instruction.@"i32.ge_s",
            .@"i32.ge_u" => rt_instr = Instruction.@"i32.ge_u",
            .@"i64.eqz" => rt_instr = Instruction.@"i64.eqz",
            .@"i64.eq" => rt_instr = Instruction.@"i64.eq",
            .@"i64.ne" => rt_instr = Instruction.@"i64.ne",
            .@"i64.lt_s" => rt_instr = Instruction.@"i64.lt_s",
            .@"i64.lt_u" => rt_instr = Instruction.@"i64.lt_u",
            .@"i64.gt_s" => rt_instr = Instruction.@"i64.gt_s",
            .@"i64.gt_u" => rt_instr = Instruction.@"i64.gt_u",
            .@"i64.le_s" => rt_instr = Instruction.@"i64.le_s",
            .@"i64.le_u" => rt_instr = Instruction.@"i64.le_u",
            .@"i64.ge_s" => rt_instr = Instruction.@"i64.ge_s",
            .@"i64.ge_u" => rt_instr = Instruction.@"i64.ge_u",
            .@"f32.eq" => rt_instr = Instruction.@"f32.eq",
            .@"f32.ne" => rt_instr = Instruction.@"f32.ne",
            .@"f32.lt" => rt_instr = Instruction.@"f32.lt",
            .@"f32.gt" => rt_instr = Instruction.@"f32.gt",
            .@"f32.le" => rt_instr = Instruction.@"f32.le",
            .@"f32.ge" => rt_instr = Instruction.@"f32.ge",
            .@"f64.eq" => rt_instr = Instruction.@"f64.eq",
            .@"f64.ne" => rt_instr = Instruction.@"f64.ne",
            .@"f64.lt" => rt_instr = Instruction.@"f64.lt",
            .@"f64.gt" => rt_instr = Instruction.@"f64.gt",
            .@"f64.le" => rt_instr = Instruction.@"f64.le",
            .@"f64.ge" => rt_instr = Instruction.@"f64.ge",
            .@"i32.clz" => rt_instr = Instruction.@"i32.clz",
            .@"i32.ctz" => rt_instr = Instruction.@"i32.ctz",
            .@"i32.popcnt" => rt_instr = Instruction.@"i32.popcnt",
            .@"i32.add" => rt_instr = Instruction.@"i32.add",
            .@"i32.sub" => rt_instr = Instruction.@"i32.sub",
            .@"i32.mul" => rt_instr = Instruction.@"i32.mul",
            .@"i32.div_s" => rt_instr = Instruction.@"i32.div_s",
            .@"i32.div_u" => rt_instr = Instruction.@"i32.div_u",
            .@"i32.rem_s" => rt_instr = Instruction.@"i32.rem_s",
            .@"i32.rem_u" => rt_instr = Instruction.@"i32.rem_u",
            .@"i32.and" => rt_instr = Instruction.@"i32.and",
            .@"i32.or" => rt_instr = Instruction.@"i32.or",
            .@"i32.xor" => rt_instr = Instruction.@"i32.xor",
            .@"i32.shl" => rt_instr = Instruction.@"i32.shl",
            .@"i32.shr_s" => rt_instr = Instruction.@"i32.shr_s",
            .@"i32.shr_u" => rt_instr = Instruction.@"i32.shr_u",
            .@"i32.rotl" => rt_instr = Instruction.@"i32.rotl",
            .@"i32.rotr" => rt_instr = Instruction.@"i32.rotr",
            .@"i64.clz" => rt_instr = Instruction.@"i64.clz",
            .@"i64.ctz" => rt_instr = Instruction.@"i64.ctz",
            .@"i64.popcnt" => rt_instr = Instruction.@"i64.popcnt",
            .@"i64.add" => rt_instr = Instruction.@"i64.add",
            .@"i64.sub" => rt_instr = Instruction.@"i64.sub",
            .@"i64.mul" => rt_instr = Instruction.@"i64.mul",
            .@"i64.div_s" => rt_instr = Instruction.@"i64.div_s",
            .@"i64.div_u" => rt_instr = Instruction.@"i64.div_u",
            .@"i64.rem_s" => rt_instr = Instruction.@"i64.rem_s",
            .@"i64.rem_u" => rt_instr = Instruction.@"i64.rem_u",
            .@"i64.and" => rt_instr = Instruction.@"i64.and",
            .@"i64.or" => rt_instr = Instruction.@"i64.or",
            .@"i64.xor" => rt_instr = Instruction.@"i64.xor",
            .@"i64.shl" => rt_instr = Instruction.@"i64.shl",
            .@"i64.shr_s" => rt_instr = Instruction.@"i64.shr_s",
            .@"i64.shr_u" => rt_instr = Instruction.@"i64.shr_u",
            .@"i64.rotl" => rt_instr = Instruction.@"i64.rotl",
            .@"i64.rotr" => rt_instr = Instruction.@"i64.rotr",
            .@"f32.abs" => rt_instr = Instruction.@"f32.abs",
            .@"f32.neg" => rt_instr = Instruction.@"f32.neg",
            .@"f32.ceil" => rt_instr = Instruction.@"f32.ceil",
            .@"f32.floor" => rt_instr = Instruction.@"f32.floor",
            .@"f32.trunc" => rt_instr = Instruction.@"f32.trunc",
            .@"f32.nearest" => rt_instr = Instruction.@"f32.nearest",
            .@"f32.sqrt" => rt_instr = Instruction.@"f32.sqrt",
            .@"f32.add" => rt_instr = Instruction.@"f32.add",
            .@"f32.sub" => rt_instr = Instruction.@"f32.sub",
            .@"f32.mul" => rt_instr = Instruction.@"f32.mul",
            .@"f32.div" => rt_instr = Instruction.@"f32.div",
            .@"f32.min" => rt_instr = Instruction.@"f32.min",
            .@"f32.max" => rt_instr = Instruction.@"f32.max",
            .@"f32.copysign" => rt_instr = Instruction.@"f32.copysign",
            .@"f64.abs" => rt_instr = Instruction.@"f64.abs",
            .@"f64.neg" => rt_instr = Instruction.@"f64.neg",
            .@"f64.ceil" => rt_instr = Instruction.@"f64.ceil",
            .@"f64.floor" => rt_instr = Instruction.@"f64.floor",
            .@"f64.trunc" => rt_instr = Instruction.@"f64.trunc",
            .@"f64.nearest" => rt_instr = Instruction.@"f64.nearest",
            .@"f64.sqrt" => rt_instr = Instruction.@"f64.sqrt",
            .@"f64.add" => rt_instr = Instruction.@"f64.add",
            .@"f64.sub" => rt_instr = Instruction.@"f64.sub",
            .@"f64.mul" => rt_instr = Instruction.@"f64.mul",
            .@"f64.div" => rt_instr = Instruction.@"f64.div",
            .@"f64.min" => rt_instr = Instruction.@"f64.min",
            .@"f64.max" => rt_instr = Instruction.@"f64.max",
            .@"f64.copysign" => rt_instr = Instruction.@"f64.copysign",
            .@"i32.wrap_i64" => rt_instr = Instruction.@"i32.wrap_i64",
            .@"i32.trunc_f32_s" => rt_instr = Instruction.@"i32.trunc_f32_s",
            .@"i32.trunc_f32_u" => rt_instr = Instruction.@"i32.trunc_f32_u",
            .@"i32.trunc_f64_s" => rt_instr = Instruction.@"i32.trunc_f64_s",
            .@"i32.trunc_f64_u" => rt_instr = Instruction.@"i32.trunc_f64_u",
            .@"i64.extend_i32_s" => rt_instr = Instruction.@"i64.extend_i32_s",
            .@"i64.extend_i32_u" => rt_instr = Instruction.@"i64.extend_i32_u",
            .@"i64.trunc_f32_s" => rt_instr = Instruction.@"i64.trunc_f32_s",
            .@"i64.trunc_f32_u" => rt_instr = Instruction.@"i64.trunc_f32_u",
            .@"i64.trunc_f64_s" => rt_instr = Instruction.@"i64.trunc_f64_s",
            .@"i64.trunc_f64_u" => rt_instr = Instruction.@"i64.trunc_f64_u",
            .@"f32.convert_i32_s" => rt_instr = Instruction.@"f32.convert_i32_s",
            .@"f32.convert_i32_u" => rt_instr = Instruction.@"f32.convert_i32_u",
            .@"f32.convert_i64_s" => rt_instr = Instruction.@"f32.convert_i64_s",
            .@"f32.convert_i64_u" => rt_instr = Instruction.@"f32.convert_i64_u",
            .@"f32.demote_f64" => rt_instr = Instruction.@"f32.demote_f64",
            .@"f64.convert_i32_s" => rt_instr = Instruction.@"f64.convert_i32_s",
            .@"f64.convert_i32_u" => rt_instr = Instruction.@"f64.convert_i32_u",
            .@"f64.convert_i64_s" => rt_instr = Instruction.@"f64.convert_i64_s",
            .@"f64.convert_i64_u" => rt_instr = Instruction.@"f64.convert_i64_u",
            .@"f64.promote_f32" => rt_instr = Instruction.@"f64.promote_f32",
            .@"i32.reinterpret_f32" => rt_instr = Instruction.@"i32.reinterpret_f32",
            .@"i64.reinterpret_f64" => rt_instr = Instruction.@"i64.reinterpret_f64",
            .@"f32.reinterpret_i32" => rt_instr = Instruction.@"f32.reinterpret_i32",
            .@"f64.reinterpret_i64" => rt_instr = Instruction.@"f64.reinterpret_i64",
            .@"i32.extend8_s" => rt_instr = Instruction.@"i32.extend8_s",
            .@"i32.extend16_s" => rt_instr = Instruction.@"i32.extend16_s",
            .@"i64.extend8_s" => rt_instr = Instruction.@"i64.extend8_s",
            .@"i64.extend16_s" => rt_instr = Instruction.@"i64.extend16_s",
            .@"i64.extend32_s" => rt_instr = Instruction.@"i64.extend32_s",
            .@"ref.null" => {
                const rtype = try opcode.readULEB128Mem(i32, &self.code);
                const reftype = std.meta.intToEnum(RefType, rtype) catch return error.MalformedRefType;

                try self.validator.validateRefNull(reftype);
                rt_instr = Instruction{ .@"ref.null" = reftype };
            },
            .@"ref.is_null" => rt_instr = Instruction.@"ref.is_null",
            .@"ref.func" => {
                const funcidx = try opcode.readULEB128Mem(u32, &self.code);
                if (funcidx >= self.module.functions.list.items.len) return error.ValidatorInvalidFunction;

                // Gather funcidx* from constant expressions
                if (self.is_constant) {
                    try self.module.references.append(funcidx);
                } else {
                    // For functions, check that the funcidx has already been referenced
                    var in_references = false;
                    for (self.module.references.items) |ref_funcidx| {
                        if (funcidx == ref_funcidx) {
                            in_references = true;
                        }
                    }

                    if (!in_references) return error.ValidatorUnreferencedFunction;
                }

                rt_instr = Instruction{ .@"ref.func" = funcidx };
            },
            .misc => {
                const version = try opcode.readULEB128Mem(u32, &self.code);
                const misc_opcode = try std.meta.intToEnum(MiscOpcode, version);
                try self.validator.validateMisc(misc_opcode);

                switch (misc_opcode) {
                    .@"i32.trunc_sat_f32_s" => rt_instr = Instruction{ .misc = MiscInstruction.@"i32.trunc_sat_f32_s" },
                    .@"i32.trunc_sat_f32_u" => rt_instr = Instruction{ .misc = MiscInstruction.@"i32.trunc_sat_f32_u" },
                    .@"i32.trunc_sat_f64_s" => rt_instr = Instruction{ .misc = MiscInstruction.@"i32.trunc_sat_f64_s" },
                    .@"i32.trunc_sat_f64_u" => rt_instr = Instruction{ .misc = MiscInstruction.@"i32.trunc_sat_f64_u" },
                    .@"i64.trunc_sat_f32_s" => rt_instr = Instruction{ .misc = MiscInstruction.@"i64.trunc_sat_f32_s" },
                    .@"i64.trunc_sat_f32_u" => rt_instr = Instruction{ .misc = MiscInstruction.@"i64.trunc_sat_f32_u" },
                    .@"i64.trunc_sat_f64_s" => rt_instr = Instruction{ .misc = MiscInstruction.@"i64.trunc_sat_f64_s" },
                    .@"i64.trunc_sat_f64_u" => rt_instr = Instruction{ .misc = MiscInstruction.@"i64.trunc_sat_f64_u" },
                    .@"memory.init" => {
                        const dataidx = try opcode.readULEB128Mem(u32, &self.code);
                        const memidx = try opcode.readByte(&self.code);

                        const data_count = self.module.dataCount orelse return error.InstructionRequiresDataCountSection;
                        if (!(dataidx < data_count)) return error.InvalidDataIndex;

                        if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                        rt_instr = Instruction{
                            .misc = MiscInstruction{
                                .@"memory.init" = .{
                                    .dataidx = dataidx,
                                    .memidx = memidx,
                                },
                            },
                        };
                    },
                    .@"data.drop" => {
                        const dataidx = try opcode.readULEB128Mem(u32, &self.code);

                        const data_count = self.module.dataCount orelse return error.InstructionRequiresDataCountSection;
                        if (!(dataidx < data_count)) return error.InvalidDataIndex;

                        rt_instr = Instruction{ .misc = MiscInstruction{ .@"data.drop" = dataidx } };
                    },
                    .@"memory.copy" => {
                        const src_memidx = try opcode.readByte(&self.code);
                        if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                        const dest_memidx = try opcode.readByte(&self.code);
                        if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;

                        rt_instr = Instruction{ .misc = MiscInstruction{ .@"memory.copy" = .{
                            .src_memidx = src_memidx,
                            .dest_memidx = dest_memidx,
                        } } };
                    },
                    .@"memory.fill" => {
                        const memidx = try opcode.readByte(&self.code);
                        if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;

                        rt_instr = Instruction{ .misc = MiscInstruction{ .@"memory.fill" = memidx } };
                    },
                    .@"table.init" => {
                        const elemidx = try opcode.readULEB128Mem(u32, &self.code);
                        if (elemidx >= self.module.elements.list.items.len) return error.ValidatorInvalidElementIndex;
                        const tableidx = try opcode.readULEB128Mem(u32, &self.code);
                        if (tableidx >= self.module.tables.list.items.len) return error.ValidatorInvalidTableIndex;

                        const elemtype = self.module.elements.list.items[elemidx];
                        const tabletype = self.module.tables.list.items[tableidx];
                        if (elemtype.reftype != tabletype.reftype) return error.MismatchedTypes;

                        rt_instr = Instruction{ .misc = MiscInstruction{ .@"table.init" = .{
                            .elemidx = elemidx,
                            .tableidx = tableidx,
                        } } };
                    },
                    .@"elem.drop" => {
                        const elemidx = try opcode.readULEB128Mem(u32, &self.code);

                        if (elemidx >= self.module.elements.list.items.len) return error.ValidatorInvalidElementIndex;

                        rt_instr = Instruction{ .misc = MiscInstruction{ .@"elem.drop" = .{ .elemidx = elemidx } } };
                    },
                    .@"table.copy" => {
                        const dest_tableidx = try opcode.readULEB128Mem(u32, &self.code);
                        if (dest_tableidx >= self.module.tables.list.items.len) return error.ValidatorInvalidTableIndex;
                        const src_tableidx = try opcode.readULEB128Mem(u32, &self.code);
                        if (src_tableidx >= self.module.tables.list.items.len) return error.ValidatorInvalidTableIndex;

                        const dest_tabletype = self.module.tables.list.items[dest_tableidx];
                        const src_tabletype = self.module.tables.list.items[src_tableidx];
                        if (dest_tabletype.reftype != src_tabletype.reftype) return error.MismatchedTypes;

                        rt_instr = Instruction{ .misc = MiscInstruction{ .@"table.copy" = .{
                            .dest_tableidx = dest_tableidx,
                            .src_tableidx = src_tableidx,
                        } } };
                    },
                    .@"table.grow" => {
                        const tableidx = try opcode.readULEB128Mem(u32, &self.code);
                        if (tableidx >= self.module.tables.list.items.len) return error.ValidatorInvalidTableIndex;

                        const table = self.module.tables.list.items[tableidx];
                        const reftype: ValueType = switch (table.reftype) {
                            .FuncRef => .FuncRef,
                            .ExternRef => .ExternRef,
                        };

                        _ = try self.validator.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                        _ = try self.validator.popOperandExpecting(ValueTypeUnknown{ .Known = reftype });

                        try self.validator.pushOperand(ValueTypeUnknown{ .Known = .I32 });

                        rt_instr = Instruction{ .misc = MiscInstruction{ .@"table.grow" = .{
                            .tableidx = tableidx,
                        } } };
                    },
                    .@"table.size" => {
                        const tableidx = try opcode.readULEB128Mem(u32, &self.code);
                        if (tableidx >= self.module.tables.list.items.len) return error.ValidatorInvalidTableIndex;

                        rt_instr = Instruction{ .misc = MiscInstruction{ .@"table.size" = .{
                            .tableidx = tableidx,
                        } } };
                    },
                    .@"table.fill" => {
                        const tableidx = try opcode.readULEB128Mem(u32, &self.code);
                        if (tableidx >= self.module.tables.list.items.len) return error.ValidatorInvalidTableIndex;

                        const table = self.module.tables.list.items[tableidx];
                        const reftype: ValueType = switch (table.reftype) {
                            .FuncRef => .FuncRef,
                            .ExternRef => .ExternRef,
                        };

                        _ = try self.validator.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });
                        _ = try self.validator.popOperandExpecting(ValueTypeUnknown{ .Known = reftype });
                        _ = try self.validator.popOperandExpecting(ValueTypeUnknown{ .Known = .I32 });

                        rt_instr = Instruction{ .misc = MiscInstruction{ .@"table.fill" = .{
                            .tableidx = tableidx,
                        } } };
                    },
                }
            },
        }

        // Validate the instruction. Some instructions, e.g. block, loop
        // are validate separately above.
        switch (instr) {
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
            .misc,
            .@"ref.null",
            .select_t,
            .@"table.get",
            .@"table.set",
            => {},
            else => try self.validator.validate(instr),
        }

        return rt_instr;
    }
};
