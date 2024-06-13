const std = @import("std");
const math = std.math;
const leb = std.leb;
const ArrayList = std.ArrayList;
const Module = @import("../module.zig").Module;
const Decoder = @import("../module.zig").Decoder;
const LocalType = @import("../module.zig").LocalType;
const Opcode = @import("../opcode.zig").Opcode;
const MiscOpcode = @import("../opcode.zig").MiscOpcode;
const Validator = @import("validator.zig").Validator;
const Type = @import("validator.zig").Type;
const ValType = @import("../valtype.zig").ValType;
const RefType = @import("../valtype.zig").RefType;
const Range = @import("../rr.zig").Range;
const Rr = @import("../rr.zig").Rr;
const MiscRr = @import("../rr.zig").MiscRr;

pub const Parsed = struct {
    start: usize,
    max_depth: usize,
};

pub const Parser = struct {
    function: []const u8 = undefined,
    code: []const u8 = undefined,
    code_ptr: usize,
    module: *Module,
    decoder: *Decoder,
    validator: Validator = undefined,
    params: ?[]const ValType,
    locals: ?[]LocalType,
    continuation_stack: [1024]usize = [_]usize{0} ** 1024,
    continuation_stack_ptr: usize,
    is_constant: bool = false,
    scope: usize,

    pub fn init(module: *Module, decoder: *Decoder) Parser {
        return Parser{
            .code_ptr = module.parsed_code.items.len,
            .module = module,
            .decoder = decoder,
            .params = null,
            .locals = null,
            .continuation_stack_ptr = 0,
            .scope = 1,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.validator.deinit();
    }

    pub fn parseFunction(self: *Parser, funcidx: usize, locals: []LocalType, code: []const u8) !Parsed {
        self.validator = Validator.init(self.module.alloc, false);
        self.is_constant = false;

        self.function = code;
        self.code = code;
        const code_start = self.module.parsed_code.items.len;

        try self.pushFunction(locals, funcidx);

        while (try self.next()) |instr| {
            try self.module.parsed_code.append(instr);
        }

        const bytes_read = self.bytesRead();
        _ = try self.decoder.readSlice(bytes_read);

        // Patch last end so that it is return
        self.module.parsed_code.items[self.module.parsed_code.items.len - 1] = .@"return";

        return Parsed{ .start = code_start, .max_depth = self.validator.max_depth };
    }

    pub fn parseConstantExpression(self: *Parser, valtype: ValType, code: []const u8) !Parsed {
        self.validator = Validator.init(self.module.alloc, true);
        self.is_constant = true;

        self.function = code;
        self.code = code;
        const code_start = self.module.parsed_code.items.len;

        const in: [0]ValType = [_]ValType{} ** 0;
        const out: [1]ValType = [_]ValType{valtype} ** 1;

        try self.validator.pushControlFrame(
            .block,
            in[0..0],
            out[0..1],
        );

        while (try self.next()) |instr| {
            switch (instr) {
                .@"i32.const",
                .@"i64.const",
                .@"f32.const",
                .@"f64.const",
                .@"global.get",
                .@"ref.null",
                .@"ref.func",
                .end,
                => {},
                else => return error.ValidatorConstantExpressionRequired,
            }
            try self.module.parsed_code.append(instr);
        }

        const bytes_read = self.bytesRead();
        _ = try self.decoder.readSlice(bytes_read);

        // Patch last end so that it is return
        self.module.parsed_code.items[self.module.parsed_code.items.len - 1] = .@"return";

        return Parsed{ .start = code_start, .max_depth = self.validator.max_depth };
    }

    // pushFunction initiliase the validator for the current function
    fn pushFunction(self: *Parser, locals: []LocalType, funcidx: usize) !void {
        const func = try self.module.functions.lookup(funcidx);
        const functype = try self.module.types.lookup(func.typeidx);

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

    fn peekContinuationStack(self: *Parser) !usize {
        if (self.continuation_stack_ptr <= 0) return error.ContinuationStackUnderflow; // No test covering this
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

        var rr: Rr = undefined;

        // 2. Find the start of the next instruction
        switch (instr) {
            .@"unreachable" => rr = Rr.@"unreachable",
            .nop => rr = Rr.nop,
            .block => {
                const block_type = try self.readILEB128Mem(i32);

                var block_params: u16 = 0;
                var block_returns: u16 = if (block_type == -0x40) 0 else 1;

                if (block_type >= 0) {
                    const funcidx: u32 = @intCast(block_type);
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

                rr = Rr{
                    .block = .{
                        .param_arity = block_params,
                        .return_arity = block_returns,
                        .branch_target = 0,
                    },
                };
            },
            .loop => {
                const block_type = try self.readILEB128Mem(i32);

                var block_params: u16 = 0;
                var block_returns: u16 = if (block_type == -0x40) 0 else 1;
                if (block_type >= 0) {
                    const funcidx: u32 = @intCast(block_type);
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

                rr = Rr{
                    .loop = .{
                        .param_arity = block_params,
                        .return_arity = block_params,
                        .branch_target = math.cast(u32, self.code_ptr) orelse return error.FailedCast,
                    },
                };
            },
            .@"if" => {
                const block_type = try self.readILEB128Mem(i32);

                // 1. First assume the number of block params is 0
                // 2. The number of return values is 0 if block_type == -0x40
                //    otherwise assume temporarily that 1 value is returned
                // 3. If block_type >= 0 then we reference a function type,
                //    so look it up and update the params / returns count to match
                var block_params: u16 = 0;
                var block_returns: u16 = if (block_type == -0x40) 0 else 1;
                if (block_type >= 0) {
                    const funcidx: u32 = @intCast(block_type);
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

                rr = Rr{
                    .if_no_else = .{
                        .param_arity = block_params,
                        .return_arity = block_returns,
                        .branch_target = 0,
                    },
                };
            },
            .@"else" => {
                const parsed_code_offset = try self.peekContinuationStack();

                switch (self.module.parsed_code.items[parsed_code_offset]) {
                    .if_no_else => |*b| {
                        self.module.parsed_code.items[parsed_code_offset] = Rr{
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

                rr = Rr.@"else";
            },
            .end => {
                self.scope -= 1;
                // If we're not looking at the `end` of a function
                if (self.scope != 0) {
                    const parsed_code_offset = try self.popContinuationStack();

                    switch (self.module.parsed_code.items[parsed_code_offset]) {
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

                rr = Rr.end;
            },
            .br => {
                const label = try self.readULEB128Mem(u32);
                try self.validator.validateBr(label);
                rr = Rr{ .br = label };
            },
            .br_if => {
                const label = try self.readULEB128Mem(u32);
                try self.validator.validateBrIf(label);
                rr = Rr{ .br_if = label };
            },
            .br_table => {
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

                rr = Rr{
                    .br_table = .{
                        .ls = Range{ .offset = label_start, .count = label_count },
                        .ln = ln,
                    },
                };
            },
            .@"return" => rr = Rr.@"return",
            .call => {
                const funcidx = try self.readULEB128Mem(u32);
                const func = try self.module.functions.lookup(funcidx);
                const functype = try self.module.types.lookup(func.typeidx);

                try self.validator.validateCall(functype);

                rr = Rr{ .call = funcidx };
                // TODO: do the replacement at instantiate-time for a fastcall if in same module?
                // rr =  Rr{ .fast_call = .{ .ip_start = 0, .params = 1, .locals = 0, .results = 1 } };
            },
            .call_indirect => {
                const typeidx = try self.readULEB128Mem(u32);
                const functype = try self.module.types.lookup(typeidx);

                const tableidx = try self.readByte();
                if (tableidx >= self.module.tables.list.items.len) return error.ValidatorCallIndirectNoTable;

                try self.validator.validateCallIndirect(functype);

                rr = Rr{
                    .call_indirect = .{
                        .typeidx = typeidx,
                        .tableidx = tableidx,
                    },
                };
            },
            .drop => rr = Rr.drop,
            .select => rr = Rr.select,
            .select_t => {
                const type_count = try self.readULEB128Mem(u32);
                if (type_count != 1) return error.OnlyOneSelectTTypeSupported; // Future versions may support more than one
                const valuetype_raw = try self.readULEB128Mem(i32);
                const valuetype = try std.meta.intToEnum(ValType, valuetype_raw);

                try self.validator.validateSelectT(valuetype);

                rr = Rr.select;
            },
            .@"global.get" => {
                const globalidx = try self.readULEB128Mem(u32);
                const global = try self.module.globals.lookup(globalidx);

                try self.validator.validateGlobalGet(global);

                rr = Rr{ .@"global.get" = globalidx };
            },
            .@"global.set" => {
                const globalidx = try self.readULEB128Mem(u32);
                const global = try self.module.globals.lookup(globalidx);

                try self.validator.validateGlobalSet(global);

                rr = Rr{ .@"global.set" = globalidx };
            },
            .@"table.get" => {
                const tableidx = try self.readULEB128Mem(u32);
                const table = try self.module.tables.lookup(tableidx);

                const reftype: ValType = switch (table.reftype) {
                    .FuncRef => .FuncRef,
                    .ExternRef => .ExternRef,
                };

                _ = try self.validator.popOperandExpecting(Type{ .Known = .I32 });
                _ = try self.validator.pushOperand(Type{ .Known = reftype });

                rr = Rr{ .@"table.get" = tableidx };
            },
            .@"table.set" => {
                const tableidx = try self.readULEB128Mem(u32);
                const table = try self.module.tables.lookup(tableidx);

                const reftype: ValType = switch (table.reftype) {
                    .FuncRef => .FuncRef,
                    .ExternRef => .ExternRef,
                };

                _ = try self.validator.popOperandExpecting(Type{ .Known = reftype });
                _ = try self.validator.popOperandExpecting(Type{ .Known = .I32 });

                rr = Rr{ .@"table.set" = tableidx };
            },
            .@"local.get" => {
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

                rr = Rr{ .@"local.get" = localidx };
            },
            .@"local.set" => {
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

                rr = Rr{ .@"local.set" = localidx };
            },
            .@"local.tee" => {
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

                rr = Rr{ .@"local.tee" = localidx };
            },
            .@"memory.size" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const memidx = try self.readByte();
                if (memidx != 0) return error.MalformedMemoryReserved;

                rr = Rr{ .@"memory.size" = memidx };
            },
            .@"memory.grow" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const memidx = try self.readByte();
                if (memidx != 0) return error.MalformedMemoryReserved;

                rr = Rr{ .@"memory.grow" = memidx };
            },
            .@"i32.const" => {
                const i32_const = try self.readILEB128Mem(i32);
                rr = Rr{ .@"i32.const" = i32_const };
            },
            .@"i64.const" => {
                const i64_const = try self.readILEB128Mem(i64);
                rr = Rr{ .@"i64.const" = i64_const };
            },
            .@"f32.const" => {
                const float_const: f32 = @bitCast(try self.readU32());
                rr = Rr{ .@"f32.const" = float_const };
            },
            .@"f64.const" => {
                const float_const: f64 = @bitCast(try self.readU64());
                rr = Rr{ .@"f64.const" = float_const };
            },
            .@"i32.load" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;

                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 32) return error.InvalidAlignment;

                rr = Rr{
                    .@"i32.load" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 64) return error.InvalidAlignment;

                rr = Rr{
                    .@"i64.load" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"f32.load" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 32) return error.InvalidAlignment;

                rr = Rr{
                    .@"f32.load" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"f64.load" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 64) return error.InvalidAlignment;

                rr = Rr{
                    .@"f64.load" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.load8_s" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 8) return error.InvalidAlignment;

                rr = Rr{
                    .@"i32.load8_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.load8_u" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 8) return error.InvalidAlignment;

                rr = Rr{
                    .@"i32.load8_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.load16_s" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 16) return error.InvalidAlignment;

                rr = Rr{
                    .@"i32.load16_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.load16_u" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 16) return error.InvalidAlignment;

                rr = Rr{
                    .@"i32.load16_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load8_s" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 8) return error.InvalidAlignment;

                rr = Rr{
                    .@"i64.load8_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load8_u" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 8) return error.InvalidAlignment;

                rr = Rr{
                    .@"i64.load8_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load16_s" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 16) return error.InvalidAlignment;

                rr = Rr{
                    .@"i64.load16_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load16_u" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 16) return error.InvalidAlignment;

                rr = Rr{
                    .@"i64.load16_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load32_s" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 32) return error.InvalidAlignment;

                rr = Rr{
                    .@"i64.load32_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load32_u" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 32) return error.InvalidAlignment;

                rr = Rr{
                    .@"i64.load32_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.store" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 32) return error.InvalidAlignment;

                rr = Rr{
                    .@"i32.store" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.store" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 64) return error.InvalidAlignment;

                rr = Rr{
                    .@"i64.store" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"f32.store" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 32) return error.InvalidAlignment;

                rr = Rr{
                    .@"f32.store" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"f64.store" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 64) return error.InvalidAlignment;

                rr = Rr{
                    .@"f64.store" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.store8" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 8) return error.InvalidAlignment;

                rr = Rr{
                    .@"i32.store8" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.store16" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 16) return error.InvalidAlignment;

                rr = Rr{
                    .@"i32.store16" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.store8" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 8) return error.InvalidAlignment;

                rr = Rr{
                    .@"i64.store8" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.store16" => {
                if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 16) return error.InvalidAlignment;

                rr = Rr{
                    .@"i64.store16" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.store32" => {
                const alignment = try self.readULEB128Mem(u32);
                const offset = try self.readULEB128Mem(u32);

                if (try math.mul(u32, 8, try math.powi(u32, 2, alignment)) > 32) return error.InvalidAlignment;

                rr = Rr{
                    .@"i64.store32" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.eqz" => rr = Rr.@"i32.eqz",
            .@"i32.eq" => rr = Rr.@"i32.eq",
            .@"i32.ne" => rr = Rr.@"i32.ne",
            .@"i32.lt_s" => rr = Rr.@"i32.lt_s",
            .@"i32.lt_u" => rr = Rr.@"i32.lt_u",
            .@"i32.gt_s" => rr = Rr.@"i32.gt_s",
            .@"i32.gt_u" => rr = Rr.@"i32.gt_u",
            .@"i32.le_s" => rr = Rr.@"i32.le_s",
            .@"i32.le_u" => rr = Rr.@"i32.le_u",
            .@"i32.ge_s" => rr = Rr.@"i32.ge_s",
            .@"i32.ge_u" => rr = Rr.@"i32.ge_u",
            .@"i64.eqz" => rr = Rr.@"i64.eqz",
            .@"i64.eq" => rr = Rr.@"i64.eq",
            .@"i64.ne" => rr = Rr.@"i64.ne",
            .@"i64.lt_s" => rr = Rr.@"i64.lt_s",
            .@"i64.lt_u" => rr = Rr.@"i64.lt_u",
            .@"i64.gt_s" => rr = Rr.@"i64.gt_s",
            .@"i64.gt_u" => rr = Rr.@"i64.gt_u",
            .@"i64.le_s" => rr = Rr.@"i64.le_s",
            .@"i64.le_u" => rr = Rr.@"i64.le_u",
            .@"i64.ge_s" => rr = Rr.@"i64.ge_s",
            .@"i64.ge_u" => rr = Rr.@"i64.ge_u",
            .@"f32.eq" => rr = Rr.@"f32.eq",
            .@"f32.ne" => rr = Rr.@"f32.ne",
            .@"f32.lt" => rr = Rr.@"f32.lt",
            .@"f32.gt" => rr = Rr.@"f32.gt",
            .@"f32.le" => rr = Rr.@"f32.le",
            .@"f32.ge" => rr = Rr.@"f32.ge",
            .@"f64.eq" => rr = Rr.@"f64.eq",
            .@"f64.ne" => rr = Rr.@"f64.ne",
            .@"f64.lt" => rr = Rr.@"f64.lt",
            .@"f64.gt" => rr = Rr.@"f64.gt",
            .@"f64.le" => rr = Rr.@"f64.le",
            .@"f64.ge" => rr = Rr.@"f64.ge",
            .@"i32.clz" => rr = Rr.@"i32.clz",
            .@"i32.ctz" => rr = Rr.@"i32.ctz",
            .@"i32.popcnt" => rr = Rr.@"i32.popcnt",
            .@"i32.add" => rr = Rr.@"i32.add",
            .@"i32.sub" => rr = Rr.@"i32.sub",
            .@"i32.mul" => rr = Rr.@"i32.mul",
            .@"i32.div_s" => rr = Rr.@"i32.div_s",
            .@"i32.div_u" => rr = Rr.@"i32.div_u",
            .@"i32.rem_s" => rr = Rr.@"i32.rem_s",
            .@"i32.rem_u" => rr = Rr.@"i32.rem_u",
            .@"i32.and" => rr = Rr.@"i32.and",
            .@"i32.or" => rr = Rr.@"i32.or",
            .@"i32.xor" => rr = Rr.@"i32.xor",
            .@"i32.shl" => rr = Rr.@"i32.shl",
            .@"i32.shr_s" => rr = Rr.@"i32.shr_s",
            .@"i32.shr_u" => rr = Rr.@"i32.shr_u",
            .@"i32.rotl" => rr = Rr.@"i32.rotl",
            .@"i32.rotr" => rr = Rr.@"i32.rotr",
            .@"i64.clz" => rr = Rr.@"i64.clz",
            .@"i64.ctz" => rr = Rr.@"i64.ctz",
            .@"i64.popcnt" => rr = Rr.@"i64.popcnt",
            .@"i64.add" => rr = Rr.@"i64.add",
            .@"i64.sub" => rr = Rr.@"i64.sub",
            .@"i64.mul" => rr = Rr.@"i64.mul",
            .@"i64.div_s" => rr = Rr.@"i64.div_s",
            .@"i64.div_u" => rr = Rr.@"i64.div_u",
            .@"i64.rem_s" => rr = Rr.@"i64.rem_s",
            .@"i64.rem_u" => rr = Rr.@"i64.rem_u",
            .@"i64.and" => rr = Rr.@"i64.and",
            .@"i64.or" => rr = Rr.@"i64.or",
            .@"i64.xor" => rr = Rr.@"i64.xor",
            .@"i64.shl" => rr = Rr.@"i64.shl",
            .@"i64.shr_s" => rr = Rr.@"i64.shr_s",
            .@"i64.shr_u" => rr = Rr.@"i64.shr_u",
            .@"i64.rotl" => rr = Rr.@"i64.rotl",
            .@"i64.rotr" => rr = Rr.@"i64.rotr",
            .@"f32.abs" => rr = Rr.@"f32.abs",
            .@"f32.neg" => rr = Rr.@"f32.neg",
            .@"f32.ceil" => rr = Rr.@"f32.ceil",
            .@"f32.floor" => rr = Rr.@"f32.floor",
            .@"f32.trunc" => rr = Rr.@"f32.trunc",
            .@"f32.nearest" => rr = Rr.@"f32.nearest",
            .@"f32.sqrt" => rr = Rr.@"f32.sqrt",
            .@"f32.add" => rr = Rr.@"f32.add",
            .@"f32.sub" => rr = Rr.@"f32.sub",
            .@"f32.mul" => rr = Rr.@"f32.mul",
            .@"f32.div" => rr = Rr.@"f32.div",
            .@"f32.min" => rr = Rr.@"f32.min",
            .@"f32.max" => rr = Rr.@"f32.max",
            .@"f32.copysign" => rr = Rr.@"f32.copysign",
            .@"f64.abs" => rr = Rr.@"f64.abs",
            .@"f64.neg" => rr = Rr.@"f64.neg",
            .@"f64.ceil" => rr = Rr.@"f64.ceil",
            .@"f64.floor" => rr = Rr.@"f64.floor",
            .@"f64.trunc" => rr = Rr.@"f64.trunc",
            .@"f64.nearest" => rr = Rr.@"f64.nearest",
            .@"f64.sqrt" => rr = Rr.@"f64.sqrt",
            .@"f64.add" => rr = Rr.@"f64.add",
            .@"f64.sub" => rr = Rr.@"f64.sub",
            .@"f64.mul" => rr = Rr.@"f64.mul",
            .@"f64.div" => rr = Rr.@"f64.div",
            .@"f64.min" => rr = Rr.@"f64.min",
            .@"f64.max" => rr = Rr.@"f64.max",
            .@"f64.copysign" => rr = Rr.@"f64.copysign",
            .@"i32.wrap_i64" => rr = Rr.@"i32.wrap_i64",
            .@"i32.trunc_f32_s" => rr = Rr.@"i32.trunc_f32_s",
            .@"i32.trunc_f32_u" => rr = Rr.@"i32.trunc_f32_u",
            .@"i32.trunc_f64_s" => rr = Rr.@"i32.trunc_f64_s",
            .@"i32.trunc_f64_u" => rr = Rr.@"i32.trunc_f64_u",
            .@"i64.extend_i32_s" => rr = Rr.@"i64.extend_i32_s",
            .@"i64.extend_i32_u" => rr = Rr.@"i64.extend_i32_u",
            .@"i64.trunc_f32_s" => rr = Rr.@"i64.trunc_f32_s",
            .@"i64.trunc_f32_u" => rr = Rr.@"i64.trunc_f32_u",
            .@"i64.trunc_f64_s" => rr = Rr.@"i64.trunc_f64_s",
            .@"i64.trunc_f64_u" => rr = Rr.@"i64.trunc_f64_u",
            .@"f32.convert_i32_s" => rr = Rr.@"f32.convert_i32_s",
            .@"f32.convert_i32_u" => rr = Rr.@"f32.convert_i32_u",
            .@"f32.convert_i64_s" => rr = Rr.@"f32.convert_i64_s",
            .@"f32.convert_i64_u" => rr = Rr.@"f32.convert_i64_u",
            .@"f32.demote_f64" => rr = Rr.@"f32.demote_f64",
            .@"f64.convert_i32_s" => rr = Rr.@"f64.convert_i32_s",
            .@"f64.convert_i32_u" => rr = Rr.@"f64.convert_i32_u",
            .@"f64.convert_i64_s" => rr = Rr.@"f64.convert_i64_s",
            .@"f64.convert_i64_u" => rr = Rr.@"f64.convert_i64_u",
            .@"f64.promote_f32" => rr = Rr.@"f64.promote_f32",
            .@"i32.reinterpret_f32" => rr = Rr.@"i32.reinterpret_f32",
            .@"i64.reinterpret_f64" => rr = Rr.@"i64.reinterpret_f64",
            .@"f32.reinterpret_i32" => rr = Rr.@"f32.reinterpret_i32",
            .@"f64.reinterpret_i64" => rr = Rr.@"f64.reinterpret_i64",
            .@"i32.extend8_s" => rr = Rr.@"i32.extend8_s",
            .@"i32.extend16_s" => rr = Rr.@"i32.extend16_s",
            .@"i64.extend8_s" => rr = Rr.@"i64.extend8_s",
            .@"i64.extend16_s" => rr = Rr.@"i64.extend16_s",
            .@"i64.extend32_s" => rr = Rr.@"i64.extend32_s",
            .@"ref.null" => {
                const rtype = try self.readULEB128Mem(i32);
                const reftype = std.meta.intToEnum(RefType, rtype) catch return error.MalformedRefType;

                try self.validator.validateRefNull(reftype);
                rr = Rr{ .@"ref.null" = reftype };
            },
            .@"ref.is_null" => rr = Rr.@"ref.is_null",
            .@"ref.func" => {
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

                rr = Rr{ .@"ref.func" = funcidx };
            },
            .misc => {
                const version = try self.readULEB128Mem(u32);
                const misc_opcode = try std.meta.intToEnum(MiscOpcode, version);
                try self.validator.validateMisc(misc_opcode);

                switch (misc_opcode) {
                    .@"i32.trunc_sat_f32_s" => rr = Rr{ .misc = MiscRr.@"i32.trunc_sat_f32_s" },
                    .@"i32.trunc_sat_f32_u" => rr = Rr{ .misc = MiscRr.@"i32.trunc_sat_f32_u" },
                    .@"i32.trunc_sat_f64_s" => rr = Rr{ .misc = MiscRr.@"i32.trunc_sat_f64_s" },
                    .@"i32.trunc_sat_f64_u" => rr = Rr{ .misc = MiscRr.@"i32.trunc_sat_f64_u" },
                    .@"i64.trunc_sat_f32_s" => rr = Rr{ .misc = MiscRr.@"i64.trunc_sat_f32_s" },
                    .@"i64.trunc_sat_f32_u" => rr = Rr{ .misc = MiscRr.@"i64.trunc_sat_f32_u" },
                    .@"i64.trunc_sat_f64_s" => rr = Rr{ .misc = MiscRr.@"i64.trunc_sat_f64_s" },
                    .@"i64.trunc_sat_f64_u" => rr = Rr{ .misc = MiscRr.@"i64.trunc_sat_f64_u" },
                    .@"memory.init" => {
                        const dataidx = try self.readULEB128Mem(u32);
                        const memidx = try self.readByte();

                        const data_count = self.module.data_count orelse return error.InstructionRequiresDataCountSection;
                        if (!(dataidx < data_count)) return error.InvalidDataIndex;

                        if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                        rr = Rr{
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

                        const data_count = self.module.data_count orelse return error.InstructionRequiresDataCountSection;
                        if (!(dataidx < data_count)) return error.InvalidDataIndex;

                        rr = Rr{ .misc = MiscRr{ .@"data.drop" = dataidx } };
                    },
                    .@"memory.copy" => {
                        const src_memidx = try self.readByte();
                        if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;
                        const dest_memidx = try self.readByte();
                        if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;

                        rr = Rr{ .misc = MiscRr{ .@"memory.copy" = .{
                            .src_memidx = src_memidx,
                            .dest_memidx = dest_memidx,
                        } } };
                    },
                    .@"memory.fill" => {
                        const memidx = try self.readByte();
                        if (self.module.memories.list.items.len != 1) return error.ValidatorUnknownMemory;

                        rr = Rr{ .misc = MiscRr{ .@"memory.fill" = memidx } };
                    },
                    .@"table.init" => {
                        const elemidx = try self.readULEB128Mem(u32);
                        const elemtype = try self.module.elements.lookup(elemidx);

                        const tableidx = try self.readULEB128Mem(u32);
                        const tabletype = try self.module.tables.lookup(tableidx);

                        if (elemtype.reftype != tabletype.reftype) return error.MismatchedTypes;

                        rr = Rr{ .misc = MiscRr{ .@"table.init" = .{
                            .elemidx = elemidx,
                            .tableidx = tableidx,
                        } } };
                    },
                    .@"elem.drop" => {
                        const elemidx = try self.readULEB128Mem(u32);

                        if (elemidx >= self.module.elements.list.items.len) return error.ValidatorInvalidElementIndex;

                        rr = Rr{ .misc = MiscRr{ .@"elem.drop" = .{ .elemidx = elemidx } } };
                    },
                    .@"table.copy" => {
                        const dest_tableidx = try self.readULEB128Mem(u32);
                        const dest_tabletype = try self.module.tables.lookup(dest_tableidx);

                        const src_tableidx = try self.readULEB128Mem(u32);
                        const src_tabletype = try self.module.tables.lookup(src_tableidx);

                        if (dest_tabletype.reftype != src_tabletype.reftype) return error.MismatchedTypes;

                        rr = Rr{ .misc = MiscRr{ .@"table.copy" = .{
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

                        _ = try self.validator.popOperandExpecting(Type{ .Known = .I32 });
                        _ = try self.validator.popOperandExpecting(Type{ .Known = reftype });

                        try self.validator.pushOperand(Type{ .Known = .I32 });

                        rr = Rr{ .misc = MiscRr{ .@"table.grow" = .{
                            .tableidx = tableidx,
                        } } };
                    },
                    .@"table.size" => {
                        const tableidx = try self.readULEB128Mem(u32);
                        if (tableidx >= self.module.tables.list.items.len) return error.ValidatorInvalidTableIndex;

                        rr = Rr{ .misc = MiscRr{ .@"table.size" = .{
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

                        _ = try self.validator.popOperandExpecting(Type{ .Known = .I32 });
                        _ = try self.validator.popOperandExpecting(Type{ .Known = reftype });
                        _ = try self.validator.popOperandExpecting(Type{ .Known = .I32 });

                        rr = Rr{ .misc = MiscRr{ .@"table.fill" = .{
                            .tableidx = tableidx,
                        } } };
                    },
                }
            },
        }

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
        const value = try rd.readInt(u32, .little);

        self.code.ptr += buf.pos;
        self.code.len -= buf.pos;
        return value;
    }

    pub fn readU64(self: *Parser) !u64 {
        var buf = std.io.fixedBufferStream(self.code);
        const rd = buf.reader();
        const value = try rd.readInt(u64, .little);

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
