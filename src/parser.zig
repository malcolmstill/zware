const std = @import("std");
const math = std.math;
const leb = std.leb;
const ArrayList = std.ArrayList;
const Module = @import("module.zig").Module;
const LocalType = @import("module.zig").LocalType;
const Opcode = @import("opcode.zig").Opcode;
const MiscOpcode = @import("opcode.zig").MiscOpcode;
const Validator = @import("validator.zig").Validator;
const ValTypeUnknown = @import("validator.zig").ValTypeUnknown;
const ValType = @import("valtype.zig").ValType;
const RefType = @import("valtype.zig").RefType;
const Range = @import("rr.zig").Range;
const Rr = @import("rr.zig").Rr;
const MiscRr = @import("rr.zig").MiscRr;

pub const Parser = struct {
    function: []const u8,
    code: []const u8,
    code_ptr: usize,
    parsed: *ArrayList(Rr),
    module: *Module,
    validator: Validator,
    params: ?[]const ValType,
    locals: ?[]LocalType,
    continuation_stack: []usize,
    continuation_stack_ptr: usize,
    is_constant: bool,
    scope: usize,

    pub fn init(module: *Module, function: []const u8, parsed_code: *ArrayList(Rr), continuation_stack: []usize, is_constant: bool) Parser {
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
            .scope = 1,
        };
    }

    // pushFunction initiliase the validator for the current function
    pub fn pushFunction(self: *Parser, locals: []LocalType, funcidx: usize) !void {
        const func = self.module.functions.list.items[@intCast(usize, funcidx)];
        const functype = self.module.types.list.items[@intCast(usize, func.typeidx)];

        self.params = functype.params;
        self.locals = locals;

        try self.validator.pushControlFrame(
            .nop, // block?
            functype.params[0..0],
            functype.results,
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

    pub fn bytesRead(self: *Parser) usize {
        return self.function.len - self.code.len;
    }

    pub fn next(self: *Parser) !?Rr {
        defer self.code_ptr += 1;
        if (self.scope > 0 and self.code.len == 0) return error.CouldntFindEnd;

        if (self.code.len == 0) return null;
        if (self.scope == 0) return null;

        // 1. Get the instruction we're going to return and increment code
        const instr = std.meta.intToEnum(Opcode, self.code[0]) catch return error.IllegalOpcode;
        self.code = self.code[1..];

        // 2. Find the start of the next instruction
        const rr = switch (instr) {
            .@"unreachable" => Rr.@"unreachable",
            .nop => Rr.nop,
            .block => rr: {
                const block_type = try self.readILEB128Mem(i32);

                var block_params: u16 = 0;
                var block_returns: u16 = if (block_type == -0x40) 0 else 1;

                if (block_type >= 0) {
                    const funcidx = @intCast(u32, block_type);
                    const functype = try self.module.types.lookup(funcidx);
                    block_params = math.cast(u16, functype.params.len) orelse return error.FailedCast;
                    block_returns = math.cast(u16, functype.results.len) orelse return error.FailedCast;
                    try self.validator.validateBlock(functype.params, functype.results);
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
                self.scope += 1;

                break :rr Rr{
                    .block = .{
                        .param_arity = block_params,
                        .return_arity = block_returns,
                        .branch_target = 0,
                    },
                };
            },
            .loop => rr: {
                const block_type = try self.readILEB128Mem(i32);

                var block_params: u16 = 0;
                var block_returns: u16 = if (block_type == -0x40) 0 else 1;
                if (block_type >= 0) {
                    const funcidx = @intCast(u32, block_type);
                    const functype = try self.module.types.lookup(funcidx);
                    block_params = math.cast(u16, functype.params.len) orelse return error.FailedCast;
                    block_returns = math.cast(u16, functype.results.len) orelse return error.FailedCast;
                    try self.validator.validateLoop(functype.params, functype.results);
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
                self.scope += 1;

                break :rr Rr{
                    .loop = .{
                        .param_arity = block_params,
                        .return_arity = block_params,
                        .branch_target = math.cast(u32, self.code_ptr) orelse return error.FailedCast,
                    },
                };
            },
            .@"if" => rr: {
                const block_type = try self.readILEB128Mem(i32);

                // 1. First assume the number of block params is 0
                // 2. The number of return values is 0 if block_type == -0x40
                //    otherwise assume temporarily that 1 value is returned
                // 3. If block_type >= 0 then we reference a function type,
                //    so look it up and update the params / returns count to match
                var block_params: u16 = 0;
                var block_returns: u16 = if (block_type == -0x40) 0 else 1;
                if (block_type >= 0) {
                    const funcidx = @intCast(u32, block_type);
                    const functype = try self.module.types.lookup(funcidx);
                    block_params = math.cast(u16, functype.params.len) orelse return error.FailedCast;
                    block_returns = math.cast(u16, functype.results.len) orelse return error.FailedCast;
                    try self.validator.validateIf(functype.params, functype.results);
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
                self.scope += 1;

                break :rr Rr{
                    .if_no_else = .{
                        .param_arity = block_params,
                        .return_arity = block_returns,
                        .branch_target = 0,
                    },
                };
            },
            .@"else" => rr: {
                const parsed_code_offset = self.peekContinuationStack();

                switch (self.parsed.items[parsed_code_offset]) {
                    .if_no_else => |*b| {
                        self.parsed.items[parsed_code_offset] = Rr{
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

                break :rr Rr.@"else";
            },
            .end => rr: {
                self.scope -= 1;
                // If we're not looking at the `end` of a function
                if (self.scope != 0) {
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

                break :rr Rr.end;
            },
            .br => rr: {
                const label = try self.readULEB128Mem(u32);
                try self.validator.validateBr(label);
                break :rr Rr{ .br = label };
            },
            .br_if => rr: {
                const label = try self.readULEB128Mem(u32);
                try self.validator.validateBrIf(label);
                break :rr Rr{ .br_if = label };
            },
            .br_table => rr: {
                const label_start = self.module.br_table_indices.items.len;
                const label_count = try self.readULEB128Mem(u32);

                var j: usize = 0;
                while (j < label_count) : (j += 1) {
                    const tmp_label = try self.readULEB128Mem(u32);
                    try self.module.br_table_indices.append(tmp_label);
                }
                const ln = try self.readULEB128Mem(u32);
                const l_star = self.module.br_table_indices.items[label_start .. label_start + j];

                try self.validator.validateBrTable(l_star, ln);

                break :rr Rr{
                    .br_table = .{
                        .ls = Range{ .offset = label_start, .count = label_count },
                        .ln = ln,
                    },
                };
            },
            .@"return" => Rr.@"return",
            .call => rr: {
                const funcidx = try self.readULEB128Mem(u32);
                const func = try self.module.functions.lookup(funcidx);
                const functype = try self.module.types.lookup(func.typeidx);

                try self.validator.validateCall(functype);

                break :rr Rr{ .call = funcidx };
                // TODO: do the replacement at instantiate-time for a fastcall if in same module?
                // break :rr Rr{ .fast_call = .{ .ip_start = 0, .params = 1, .locals = 0, .results = 1 } };
            },
            .call_indirect => rr: {
                const typeidx = try self.readULEB128Mem(u32);
                const functype = try self.module.types.lookup(typeidx);

                const tableidx = try self.readByte();
                if (tableidx >= self.module.tables.list.items.len) return error.ValidatorCallIndirectNoTable;

                try self.validator.validateCallIndirect(functype);

                break :rr Rr{
                    .call_indirect = .{
                        .typeidx = typeidx,
                        .tableidx = tableidx,
                    },
                };
            },
            .drop => Rr.drop,
            .select => Rr.select,
            .select_t => rr: {
                const type_count = try self.readULEB128Mem(u32);
                if (type_count != 1) return error.OnlyOneSelectTTypeSupported; // Future versions may support more than one
                const valuetype_raw = try self.readULEB128Mem(i32);
                const valuetype = try std.meta.intToEnum(ValType, valuetype_raw);

                try self.validator.validateSelectT(valuetype);

                break :rr Rr.select;
            },
            .@"global.get" => rr: {
                const globalidx = try self.readULEB128Mem(u32);
                const global = try self.module.globals.lookup(globalidx);

                try self.validator.validateGlobalGet(global);

                break :rr Rr{ .@"global.get" = globalidx };
            },
            .@"global.set" => rr: {
                const globalidx = try self.readULEB128Mem(u32);
                const global = try self.module.globals.lookup(globalidx);

                try self.validator.validateGlobalSet(global);

                break :rr Rr{ .@"global.set" = globalidx };
            },
            .@"table.get" => rr: {
                const tableidx = try self.readULEB128Mem(u32);
                const table = try self.module.tables.lookup(tableidx);

                const reftype: ValType = switch (table.reftype) {
                    .FuncRef => .FuncRef,
                    .ExternRef => .ExternRef,
                };

                _ = try self.validator.popOperandExpecting(ValTypeUnknown{ .Known = .I32 });
                _ = try self.validator.pushOperand(ValTypeUnknown{ .Known = reftype });

                break :rr Rr{ .@"table.get" = tableidx };
            },
            .@"table.set" => rr: {
                const tableidx = try self.readULEB128Mem(u32);
                const table = try self.module.tables.lookup(tableidx);

                const reftype: ValType = switch (table.reftype) {
                    .FuncRef => .FuncRef,
                    .ExternRef => .ExternRef,
                };

                _ = try self.validator.popOperandExpecting(ValTypeUnknown{ .Known = reftype });
                _ = try self.validator.popOperandExpecting(ValTypeUnknown{ .Known = .I32 });

                break :rr Rr{ .@"table.set" = tableidx };
            },
            .@"local.get" => rr: {
                const localidx = try self.readULEB128Mem(u32);
                const params = self.params orelse return error.ValidatorConstantExpressionRequired;
                const locals = self.locals orelse return error.ValidatorConstantExpressionRequired;

                if (localidx < params.len) {
                    try self.validator.validateLocalGet(params[localidx]);
                } else {
                    const local_index = localidx - params.len;
                    var local_type: ?ValType = null;
                    var count: usize = 0;
                    for (locals) |l| {
                        if (local_index < count + l.count) {
                            local_type = l.valtype;
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

                break :rr Rr{ .@"local.get" = localidx };
            },
            .@"local.set" => rr: {
                const localidx = try self.readULEB128Mem(u32);

                const params = self.params orelse return error.ValidatorConstantExpressionRequired;
                const locals = self.locals orelse return error.ValidatorConstantExpressionRequired;

                if (localidx < params.len) {
                    try self.validator.validateLocalSet(params[localidx]);
                } else {
                    const local_index = localidx - params.len;
                    var local_type: ?ValType = null;
                    var count: usize = 0;
                    for (locals) |l| {
                        if (local_index < count + l.count) {
                            local_type = l.valtype;
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

                break :rr Rr{ .@"local.set" = localidx };
            },
            .@"local.tee" => rr: {
                const localidx = try self.readULEB128Mem(u32);

                const params = self.params orelse return error.ValidatorConstantExpressionRequired;
                const locals = self.locals orelse return error.ValidatorConstantExpressionRequired;

                if (localidx < params.len) {
                    try self.validator.validateLocalTee(params[localidx]);
                } else {
                    const local_index = localidx - params.len;
                    var local_type: ?ValType = null;
                    var count: usize = 0;
                    for (locals) |l| {
                        if (local_index < count + l.count) {
                            local_type = l.valtype;
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

                break :rr Rr{ .@"local.tee" = localidx };
            },
            .@"memory.size" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const memidx = try self.readByte();
                if (memidx != 0) return error.MalformedMemoryReserved;

                break :rr Rr{ .@"memory.size" = memidx };
            },
            .@"memory.grow" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const memidx = try self.readByte();
                if (memidx != 0) return error.MalformedMemoryReserved;

                break :rr Rr{ .@"memory.grow" = memidx };
            },
            .@"i32.const" => rr: {
                const i32_const = try self.readILEB128Mem(i32);
                break :rr Rr{ .@"i32.const" = i32_const };
            },
            .@"i64.const" => rr: {
                const i64_const = try self.readILEB128Mem(i64);
                break :rr Rr{ .@"i64.const" = i64_const };
            },
            .@"f32.const" => rr: {
                const float_const = @bitCast(f32, try self.readU32());
                break :rr Rr{ .@"f32.const" = float_const };
            },
            .@"f64.const" => rr: {
                const float_const = @bitCast(f64, try self.readU64());
                break :rr Rr{ .@"f64.const" = float_const };
            },
            .@"i32.load" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;

                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 32) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i32.load" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 64) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i64.load" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"f32.load" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 32) return error.InvalidAlignment;

                break :rr Rr{
                    .@"f32.load" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"f64.load" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 64) return error.InvalidAlignment;

                break :rr Rr{
                    .@"f64.load" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.load8_s" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 8) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i32.load8_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.load8_u" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 8) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i32.load8_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.load16_s" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 16) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i32.load16_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.load16_u" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 16) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i32.load16_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load8_s" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 8) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i64.load8_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load8_u" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 8) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i64.load8_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load16_s" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 16) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i64.load16_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load16_u" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 16) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i64.load16_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load32_s" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 32) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i64.load32_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load32_u" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 32) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i64.load32_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.store" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 32) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i32.store" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.store" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 64) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i64.store" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"f32.store" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 32) return error.InvalidAlignment;

                break :rr Rr{
                    .@"f32.store" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"f64.store" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 64) return error.InvalidAlignment;

                break :rr Rr{
                    .@"f64.store" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.store8" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 8) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i32.store8" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.store16" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 16) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i32.store16" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.store8" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 8) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i64.store8" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.store16" => rr: {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 16) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i64.store16" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.store32" => rr: {
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (8 * try std.math.powi(u32, 2, alignment) > 32) return error.InvalidAlignment;

                break :rr Rr{
                    .@"i64.store32" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.eqz" => Rr.@"i32.eqz",
            .@"i32.eq" => Rr.@"i32.eq",
            .@"i32.ne" => Rr.@"i32.ne",
            .@"i32.lt_s" => Rr.@"i32.lt_s",
            .@"i32.lt_u" => Rr.@"i32.lt_u",
            .@"i32.gt_s" => Rr.@"i32.gt_s",
            .@"i32.gt_u" => Rr.@"i32.gt_u",
            .@"i32.le_s" => Rr.@"i32.le_s",
            .@"i32.le_u" => Rr.@"i32.le_u",
            .@"i32.ge_s" => Rr.@"i32.ge_s",
            .@"i32.ge_u" => Rr.@"i32.ge_u",
            .@"i64.eqz" => Rr.@"i64.eqz",
            .@"i64.eq" => Rr.@"i64.eq",
            .@"i64.ne" => Rr.@"i64.ne",
            .@"i64.lt_s" => Rr.@"i64.lt_s",
            .@"i64.lt_u" => Rr.@"i64.lt_u",
            .@"i64.gt_s" => Rr.@"i64.gt_s",
            .@"i64.gt_u" => Rr.@"i64.gt_u",
            .@"i64.le_s" => Rr.@"i64.le_s",
            .@"i64.le_u" => Rr.@"i64.le_u",
            .@"i64.ge_s" => Rr.@"i64.ge_s",
            .@"i64.ge_u" => Rr.@"i64.ge_u",
            .@"f32.eq" => Rr.@"f32.eq",
            .@"f32.ne" => Rr.@"f32.ne",
            .@"f32.lt" => Rr.@"f32.lt",
            .@"f32.gt" => Rr.@"f32.gt",
            .@"f32.le" => Rr.@"f32.le",
            .@"f32.ge" => Rr.@"f32.ge",
            .@"f64.eq" => Rr.@"f64.eq",
            .@"f64.ne" => Rr.@"f64.ne",
            .@"f64.lt" => Rr.@"f64.lt",
            .@"f64.gt" => Rr.@"f64.gt",
            .@"f64.le" => Rr.@"f64.le",
            .@"f64.ge" => Rr.@"f64.ge",
            .@"i32.clz" => Rr.@"i32.clz",
            .@"i32.ctz" => Rr.@"i32.ctz",
            .@"i32.popcnt" => Rr.@"i32.popcnt",
            .@"i32.add" => Rr.@"i32.add",
            .@"i32.sub" => Rr.@"i32.sub",
            .@"i32.mul" => Rr.@"i32.mul",
            .@"i32.div_s" => Rr.@"i32.div_s",
            .@"i32.div_u" => Rr.@"i32.div_u",
            .@"i32.rem_s" => Rr.@"i32.rem_s",
            .@"i32.rem_u" => Rr.@"i32.rem_u",
            .@"i32.and" => Rr.@"i32.and",
            .@"i32.or" => Rr.@"i32.or",
            .@"i32.xor" => Rr.@"i32.xor",
            .@"i32.shl" => Rr.@"i32.shl",
            .@"i32.shr_s" => Rr.@"i32.shr_s",
            .@"i32.shr_u" => Rr.@"i32.shr_u",
            .@"i32.rotl" => Rr.@"i32.rotl",
            .@"i32.rotr" => Rr.@"i32.rotr",
            .@"i64.clz" => Rr.@"i64.clz",
            .@"i64.ctz" => Rr.@"i64.ctz",
            .@"i64.popcnt" => Rr.@"i64.popcnt",
            .@"i64.add" => Rr.@"i64.add",
            .@"i64.sub" => Rr.@"i64.sub",
            .@"i64.mul" => Rr.@"i64.mul",
            .@"i64.div_s" => Rr.@"i64.div_s",
            .@"i64.div_u" => Rr.@"i64.div_u",
            .@"i64.rem_s" => Rr.@"i64.rem_s",
            .@"i64.rem_u" => Rr.@"i64.rem_u",
            .@"i64.and" => Rr.@"i64.and",
            .@"i64.or" => Rr.@"i64.or",
            .@"i64.xor" => Rr.@"i64.xor",
            .@"i64.shl" => Rr.@"i64.shl",
            .@"i64.shr_s" => Rr.@"i64.shr_s",
            .@"i64.shr_u" => Rr.@"i64.shr_u",
            .@"i64.rotl" => Rr.@"i64.rotl",
            .@"i64.rotr" => Rr.@"i64.rotr",
            .@"f32.abs" => Rr.@"f32.abs",
            .@"f32.neg" => Rr.@"f32.neg",
            .@"f32.ceil" => Rr.@"f32.ceil",
            .@"f32.floor" => Rr.@"f32.floor",
            .@"f32.trunc" => Rr.@"f32.trunc",
            .@"f32.nearest" => Rr.@"f32.nearest",
            .@"f32.sqrt" => Rr.@"f32.sqrt",
            .@"f32.add" => Rr.@"f32.add",
            .@"f32.sub" => Rr.@"f32.sub",
            .@"f32.mul" => Rr.@"f32.mul",
            .@"f32.div" => Rr.@"f32.div",
            .@"f32.min" => Rr.@"f32.min",
            .@"f32.max" => Rr.@"f32.max",
            .@"f32.copysign" => Rr.@"f32.copysign",
            .@"f64.abs" => Rr.@"f64.abs",
            .@"f64.neg" => Rr.@"f64.neg",
            .@"f64.ceil" => Rr.@"f64.ceil",
            .@"f64.floor" => Rr.@"f64.floor",
            .@"f64.trunc" => Rr.@"f64.trunc",
            .@"f64.nearest" => Rr.@"f64.nearest",
            .@"f64.sqrt" => Rr.@"f64.sqrt",
            .@"f64.add" => Rr.@"f64.add",
            .@"f64.sub" => Rr.@"f64.sub",
            .@"f64.mul" => Rr.@"f64.mul",
            .@"f64.div" => Rr.@"f64.div",
            .@"f64.min" => Rr.@"f64.min",
            .@"f64.max" => Rr.@"f64.max",
            .@"f64.copysign" => Rr.@"f64.copysign",
            .@"i32.wrap_i64" => Rr.@"i32.wrap_i64",
            .@"i32.trunc_f32_s" => Rr.@"i32.trunc_f32_s",
            .@"i32.trunc_f32_u" => Rr.@"i32.trunc_f32_u",
            .@"i32.trunc_f64_s" => Rr.@"i32.trunc_f64_s",
            .@"i32.trunc_f64_u" => Rr.@"i32.trunc_f64_u",
            .@"i64.extend_i32_s" => Rr.@"i64.extend_i32_s",
            .@"i64.extend_i32_u" => Rr.@"i64.extend_i32_u",
            .@"i64.trunc_f32_s" => Rr.@"i64.trunc_f32_s",
            .@"i64.trunc_f32_u" => Rr.@"i64.trunc_f32_u",
            .@"i64.trunc_f64_s" => Rr.@"i64.trunc_f64_s",
            .@"i64.trunc_f64_u" => Rr.@"i64.trunc_f64_u",
            .@"f32.convert_i32_s" => Rr.@"f32.convert_i32_s",
            .@"f32.convert_i32_u" => Rr.@"f32.convert_i32_u",
            .@"f32.convert_i64_s" => Rr.@"f32.convert_i64_s",
            .@"f32.convert_i64_u" => Rr.@"f32.convert_i64_u",
            .@"f32.demote_f64" => Rr.@"f32.demote_f64",
            .@"f64.convert_i32_s" => Rr.@"f64.convert_i32_s",
            .@"f64.convert_i32_u" => Rr.@"f64.convert_i32_u",
            .@"f64.convert_i64_s" => Rr.@"f64.convert_i64_s",
            .@"f64.convert_i64_u" => Rr.@"f64.convert_i64_u",
            .@"f64.promote_f32" => Rr.@"f64.promote_f32",
            .@"i32.reinterpret_f32" => Rr.@"i32.reinterpret_f32",
            .@"i64.reinterpret_f64" => Rr.@"i64.reinterpret_f64",
            .@"f32.reinterpret_i32" => Rr.@"f32.reinterpret_i32",
            .@"f64.reinterpret_i64" => Rr.@"f64.reinterpret_i64",
            .@"i32.extend8_s" => Rr.@"i32.extend8_s",
            .@"i32.extend16_s" => Rr.@"i32.extend16_s",
            .@"i64.extend8_s" => Rr.@"i64.extend8_s",
            .@"i64.extend16_s" => Rr.@"i64.extend16_s",
            .@"i64.extend32_s" => Rr.@"i64.extend32_s",
            .@"ref.null" => rr: {
                const rtype = try self.readULEB128Mem(i32);
                const reftype = std.meta.intToEnum(RefType, rtype) catch return error.MalformedRefType;

                try self.validator.validateRefNull(reftype);
                break :rr Rr{ .@"ref.null" = reftype };
            },
            .@"ref.is_null" => Rr.@"ref.is_null",
            .@"ref.func" => rr: {
                const funcidx = try self.readULEB128Mem(u32);
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

                break :rr Rr{ .@"ref.func" = funcidx };
            },
            .misc => rr: {
                const version = try self.readULEB128Mem(u32);
                const misc_opcode = try std.meta.intToEnum(MiscOpcode, version);
                try self.validator.validateMisc(misc_opcode);

                switch (misc_opcode) {
                    .@"i32.trunc_sat_f32_s" => break :rr Rr{ .misc = MiscRr.@"i32.trunc_sat_f32_s" },
                    .@"i32.trunc_sat_f32_u" => break :rr Rr{ .misc = MiscRr.@"i32.trunc_sat_f32_u" },
                    .@"i32.trunc_sat_f64_s" => break :rr Rr{ .misc = MiscRr.@"i32.trunc_sat_f64_s" },
                    .@"i32.trunc_sat_f64_u" => break :rr Rr{ .misc = MiscRr.@"i32.trunc_sat_f64_u" },
                    .@"i64.trunc_sat_f32_s" => break :rr Rr{ .misc = MiscRr.@"i64.trunc_sat_f32_s" },
                    .@"i64.trunc_sat_f32_u" => break :rr Rr{ .misc = MiscRr.@"i64.trunc_sat_f32_u" },
                    .@"i64.trunc_sat_f64_s" => break :rr Rr{ .misc = MiscRr.@"i64.trunc_sat_f64_s" },
                    .@"i64.trunc_sat_f64_u" => break :rr Rr{ .misc = MiscRr.@"i64.trunc_sat_f64_u" },
                    .@"memory.init" => {
                        const dataidx = try self.readULEB128Mem(u32);
                        const memidx = try self.readByte();

                        const data_count = self.module.dataCount orelse return error.InstructionRequiresDataCountSection;
                        if (!(dataidx < data_count)) return error.InvalidDataIndex;

                        if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                        break :rr Rr{
                            .misc = MiscRr{
                                .@"memory.init" = .{
                                    .dataidx = dataidx,
                                    .memidx = memidx,
                                },
                            },
                        };
                    },
                    .@"data.drop" => {
                        const dataidx = try self.readULEB128Mem(u32);

                        const data_count = self.module.dataCount orelse return error.InstructionRequiresDataCountSection;
                        if (!(dataidx < data_count)) return error.InvalidDataIndex;

                        break :rr Rr{ .misc = MiscRr{ .@"data.drop" = dataidx } };
                    },
                    .@"memory.copy" => {
                        const src_memidx = try self.readByte();
                        if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                        const dest_memidx = try self.readByte();
                        if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;

                        break :rr Rr{ .misc = MiscRr{ .@"memory.copy" = .{
                            .src_memidx = src_memidx,
                            .dest_memidx = dest_memidx,
                        } } };
                    },
                    .@"memory.fill" => {
                        const memidx = try self.readByte();
                        if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;

                        break :rr Rr{ .misc = MiscRr{ .@"memory.fill" = memidx } };
                    },
                    .@"table.init" => {
                        const elemidx = try self.readULEB128Mem(u32);
                        const elemtype = try self.module.elements.lookup(elemidx);

                        const tableidx = try self.readULEB128Mem(u32);
                        const tabletype = try self.module.tables.lookup(tableidx);

                        if (elemtype.reftype != tabletype.reftype) return error.MismatchedTypes;

                        break :rr Rr{ .misc = MiscRr{ .@"table.init" = .{
                            .elemidx = elemidx,
                            .tableidx = tableidx,
                        } } };
                    },
                    .@"elem.drop" => {
                        const elemidx = try self.readULEB128Mem(u32);

                        if (elemidx >= self.module.elements.list.items.len) return error.ValidatorInvalidElementIndex;

                        break :rr Rr{ .misc = MiscRr{ .@"elem.drop" = .{ .elemidx = elemidx } } };
                    },
                    .@"table.copy" => {
                        const dest_tableidx = try self.readULEB128Mem(u32);
                        const dest_tabletype = try self.module.tables.lookup(dest_tableidx);

                        const src_tableidx = try self.readULEB128Mem(u32);
                        const src_tabletype = try self.module.tables.lookup(src_tableidx);

                        if (dest_tabletype.reftype != src_tabletype.reftype) return error.MismatchedTypes;

                        break :rr Rr{ .misc = MiscRr{ .@"table.copy" = .{
                            .dest_tableidx = dest_tableidx,
                            .src_tableidx = src_tableidx,
                        } } };
                    },
                    .@"table.grow" => {
                        const tableidx = try self.readULEB128Mem(u32);
                        const table = try self.module.tables.lookup(tableidx);

                        const reftype: ValType = switch (table.reftype) {
                            .FuncRef => .FuncRef,
                            .ExternRef => .ExternRef,
                        };

                        _ = try self.validator.popOperandExpecting(ValTypeUnknown{ .Known = .I32 });
                        _ = try self.validator.popOperandExpecting(ValTypeUnknown{ .Known = reftype });

                        try self.validator.pushOperand(ValTypeUnknown{ .Known = .I32 });

                        break :rr Rr{ .misc = MiscRr{ .@"table.grow" = .{
                            .tableidx = tableidx,
                        } } };
                    },
                    .@"table.size" => {
                        const tableidx = try self.readULEB128Mem(u32);
                        if (tableidx >= self.module.tables.list.items.len) return error.ValidatorInvalidTableIndex;

                        break :rr Rr{ .misc = MiscRr{ .@"table.size" = .{
                            .tableidx = tableidx,
                        } } };
                    },
                    .@"table.fill" => {
                        const tableidx = try self.readULEB128Mem(u32);
                        const table = try self.module.tables.lookup(tableidx);

                        const reftype: ValType = switch (table.reftype) {
                            .FuncRef => .FuncRef,
                            .ExternRef => .ExternRef,
                        };

                        _ = try self.validator.popOperandExpecting(ValTypeUnknown{ .Known = .I32 });
                        _ = try self.validator.popOperandExpecting(ValTypeUnknown{ .Known = reftype });
                        _ = try self.validator.popOperandExpecting(ValTypeUnknown{ .Known = .I32 });

                        break :rr Rr{ .misc = MiscRr{ .@"table.fill" = .{
                            .tableidx = tableidx,
                        } } };
                    },
                }
            },
        };

        try self.validator.validate(instr);
        return rr;
    }

    pub fn readULEB128Mem(self: *Parser, comptime T: type) !T {
        var buf = std.io.fixedBufferStream(self.code);
        const value = try leb.readULEB128(T, buf.reader());
        self.code.ptr += buf.pos;
        self.code.len -= buf.pos;
        return value;
    }

    pub fn readILEB128Mem(self: *Parser, comptime T: type) !T {
        var buf = std.io.fixedBufferStream(self.code);
        const value = try leb.readILEB128(T, buf.reader());

        // The following is a bit of a kludge that should really
        // be fixed in either the std lib self.readILEB128 or using a
        // a fresh implementation. The issue is that the wasm spec
        // expects the "unused" bits in a negative ILEB128 to all be
        // one and the same bits in a positive ILEB128 to be zero.
        switch (T) {
            i32 => if (buf.pos == 5 and value < 0 and buf.buffer[4] & 0x70 != 0x70) return error.Overflow,
            i64 => if (buf.pos == 10 and value < 0 and buf.buffer[9] & 0x7e != 0x7e) return error.Overflow,
            else => @compileError("self.readILEB128Mem expects i32 or i64"),
        }

        self.code.ptr += buf.pos;
        self.code.len -= buf.pos;
        return value;
    }

    pub fn readU32(self: *Parser) !u32 {
        var buf = std.io.fixedBufferStream(self.code);
        const rd = buf.reader();
        const value = try rd.readIntLittle(u32);

        self.code.ptr += buf.pos;
        self.code.len -= buf.pos;
        return value;
    }

    pub fn readU64(self: *Parser) !u64 {
        var buf = std.io.fixedBufferStream(self.code);
        const rd = buf.reader();
        const value = try rd.readIntLittle(u64);

        self.code.ptr += buf.pos;
        self.code.len -= buf.pos;
        return value;
    }

    pub fn readByte(self: *Parser) !u8 {
        var buf = std.io.fixedBufferStream(self.code);
        const rd = buf.reader();
        const value = try rd.readByte();

        self.code.ptr += buf.pos;
        self.code.len -= buf.pos;
        return value;
    }
};

const EMPTY = [0]ValType{} ** 0;
const I32_OUT = [1]ValType{.I32} ** 1;
const I64_OUT = [1]ValType{.I64} ** 1;
const F32_OUT = [1]ValType{.F32} ** 1;
const F64_OUT = [1]ValType{.F64} ** 1;
const V128_OUT = [1]ValType{.V128} ** 1;
const FUNCREF_OUT = [1]ValType{.FuncRef} ** 1;
const EXTERNREF_OUT = [1]ValType{.ExternRef} ** 1;

pub fn valueTypeFromBlockType(block_type: i32) !ValType {
    return switch (block_type) {
        -0x01 => .I32,
        -0x02 => .I64,
        -0x03 => .F32,
        -0x04 => .F64,
        -0x10 => .FuncRef,
        -0x11 => .ExternRef,
        else => error.UnexpectedBlockType,
    };
}
