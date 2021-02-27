const std = @import("std");
const leb = std.leb;
const Module = @import("module.zig").Module;
const RuntimeInstruction = @import("function.zig").RuntimeInstruction;

pub const OpcodeIterator = struct {
    function: []const u8,
    code: []const u8,

    pub fn init(function: []const u8) OpcodeIterator {
        return OpcodeIterator{
            .code = function,
            .function = function,
        };
    }

    pub fn next(self: *OpcodeIterator) !?InstructionMeta {
        if (self.code.len == 0) return null;

        // 1. Get the instruction we're going to return and increment code
        const instr = @intToEnum(Opcode, self.code[0]);
        const offset = @ptrToInt(self.code.ptr) - @ptrToInt(self.function.ptr);
        self.code = self.code[1..];

        // 2. Find the start of the next instruction
        switch (instr) {
            .@"i32.const" => _ = try readILEB128Mem(i32, &self.code),
            .@"i64.const" => _ = try readILEB128Mem(i64, &self.code),
            .@"global.get",
            .@"global.set",
            .@"local.get",
            .@"local.set",
            .@"local.tee",
            .@"if",
            .call,
            .br,
            .br_if,
            .block,
            .loop,
            => _ = try readULEB128Mem(u32, &self.code),
            .@"memory.size", .@"memory.grow" => {
                const reserved = try readByte(&self.code);
                if (reserved != 0) return error.MalformedMemoryReserved;
            },
            .br_table => {
                const label_count = try readULEB128Mem(u32, &self.code);
                var j: usize = 0;
                while (j < label_count + 1) : (j += 1) {
                    const tmp_label = try readULEB128Mem(u32, &self.code);
                }
            },
            .call_indirect => {
                _ = try readULEB128Mem(u32, &self.code);
                const reserved = try readByte(&self.code);
                if (reserved != 0) return error.MalformedCallIndirectReserved;
            },
            .@"i32.load",
            .@"i64.load",
            .@"f32.load",
            .@"f64.load",
            .@"i32.load8_s",
            .@"i32.load8_u",
            .@"i32.load16_s",
            .@"i32.load16_u",
            .@"i64.load8_s",
            .@"i64.load8_u",
            .@"i64.load16_s",
            .@"i64.load16_u",
            .@"i64.load32_s",
            .@"i64.load32_u",
            .@"i32.store",
            .@"i64.store",
            .@"f32.store",
            .@"f64.store",
            .@"i32.store8",
            .@"i32.store16",
            .@"i64.store8",
            .@"i64.store16",
            .@"i64.store32",
            => {
                _ = try readULEB128Mem(u32, &self.code);
                _ = try readULEB128Mem(u32, &self.code);
            },
            .@"f32.const" => self.code = self.code[4..],
            .@"f64.const" => self.code = self.code[8..],
            .trunc_sat => self.code = self.code[1..],
            else => {},
        }

        return InstructionMeta{
            .instruction = instr,
            .offset = offset,
        };
    }
};

pub const ParseIterator = struct {
    function: []const u8,
    code: []const u8,
    parsed: []RuntimeInstruction,
    module: *Module,

    pub fn init(module: *Module, function: []const u8) ParseIterator {
        return ParseIterator{
            .code = function,
            .function = function,
            .parsed = module.parsed_code.items[module.parsed_code.items.len..module.parsed_code.items.len],
            .module = module,
        };
    }

    pub fn next(self: *ParseIterator) !?RuntimeInstruction {
        if (self.code.len == 0) return null;

        // 1. Get the instruction we're going to return and increment code
        const instr = @intToEnum(Opcode, self.code[0]);
        const instr_offset = @ptrToInt(self.code.ptr) - @ptrToInt(self.function.ptr);
        self.code = self.code[1..];

        var rt_instr: RuntimeInstruction = undefined;

        // 2. Find the start of the next instruction
        switch (instr) {
            .@"unreachable" => rt_instr = RuntimeInstruction.@"unreachable",
            .nop => rt_instr = RuntimeInstruction.nop,
            .block => {
                const block_type = try readILEB128Mem(i32, &self.code);

                var block_params: usize = 0;
                var block_returns: usize = if (block_type == -0x40) 0 else 1;
                if (block_type >= 0) {
                    const func_type = self.module.types.list.items[@intCast(usize, block_type)];
                    block_params = func_type.params.len;
                    block_returns = func_type.results.len;
                }

                rt_instr = RuntimeInstruction{
                    .block = .{
                        .param_arity = block_params,
                        .return_arity = block_returns,
                        .continuation = self.parsed, // TODO: fix this
                    },
                };
            },
            .loop => {
                const block_type = try readILEB128Mem(i32, &self.code);

                var block_params: usize = 0;
                var block_returns: usize = if (block_type == -0x40) 0 else 1;
                if (block_type >= 0) {
                    const func_type = self.module.types.list.items[@intCast(usize, block_type)];
                    block_params = func_type.params.len;
                    block_returns = func_type.results.len;
                }

                rt_instr = RuntimeInstruction{
                    .loop = .{
                        .param_arity = block_params,
                        .return_arity = block_params,
                        .continuation = self.parsed, // TODO: fix this
                    },
                };
            },
            .@"if" => {
                const block_type = try readILEB128Mem(i32, &self.code);

                var block_params: usize = 0;
                var block_returns: usize = if (block_type == -0x40) 0 else 1;
                if (block_type >= 0) {
                    const func_type = self.module.types.list.items[@intCast(usize, block_type)];
                    block_params = func_type.params.len;
                    block_returns = func_type.results.len;
                }

                rt_instr = RuntimeInstruction{
                    .@"if" = .{
                        .param_arity = block_params,
                        .return_arity = block_returns,
                        .continuation = self.parsed, // TODO: fix this
                        .else_continuation = null,
                    },
                };
            },
            .@"else" => rt_instr = RuntimeInstruction.@"else",
            .end => rt_instr = RuntimeInstruction.end,
            .br => {
                const immediate_u32 = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{ .br = immediate_u32 };
            },
            .br_if => {
                const immediate_u32 = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{ .br_if = immediate_u32 };
            },
            .br_table => {
                const label_start = self.module.br_table_indices.items.len;
                const label_count = try readULEB128Mem(u32, &self.code);

                var j: usize = 0;
                while (j < label_count) : (j += 1) {
                    const tmp_label = try readULEB128Mem(u32, &self.code);
                    try self.module.br_table_indices.append(tmp_label);
                }
                const ln = try readULEB128Mem(u32, &self.code);

                rt_instr = RuntimeInstruction{
                    .br_table = .{
                        .ls = self.module.br_table_indices.items[label_start .. label_start + label_count],
                        .ln = ln,
                    },
                };
            },
            .@"return" => rt_instr = RuntimeInstruction.@"return",
            .call => {
                const function_index = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{ .call = function_index };
            },
            .call_indirect => {
                const type_index = try readULEB128Mem(u32, &self.code);
                const table_reserved = try readByte(&self.code);

                if (table_reserved != 0) return error.MalformedCallIndirectReserved;

                rt_instr = RuntimeInstruction{
                    .call_indirect = .{
                        .@"type" = type_index,
                        .table = table_reserved,
                    },
                };
            },
            .drop => rt_instr = RuntimeInstruction.drop,
            .select => rt_instr = RuntimeInstruction.select,
            .@"global.get" => {
                const immediate_u32 = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{ .@"global.get" = immediate_u32 };
            },
            .@"global.set" => {
                const immediate_u32 = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{ .@"global.set" = immediate_u32 };
            },
            .@"local.get" => {
                const immediate_u32 = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{ .@"local.get" = immediate_u32 };
            },
            .@"local.set" => {
                const immediate_u32 = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{ .@"local.set" = immediate_u32 };
            },
            .@"local.tee" => {
                const immediate_u32 = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{ .@"local.tee" = immediate_u32 };
            },
            .@"memory.size" => {
                const memory_index = try readByte(&self.code);
                if (memory_index != 0) return error.MalformedMemoryReserved;

                rt_instr = RuntimeInstruction{ .@"memory.size" = memory_index };
            },
            .@"memory.grow" => {
                const memory_index = try readByte(&self.code);
                if (memory_index != 0) return error.MalformedMemoryReserved;

                rt_instr = RuntimeInstruction{ .@"memory.grow" = memory_index };
            },
            .@"i32.const" => {
                const i32_const = try readILEB128Mem(i32, &self.code);
                rt_instr = RuntimeInstruction{ .@"i32.const" = i32_const };
            },
            .@"i64.const" => {
                const i64_const = try readILEB128Mem(i64, &self.code);
                rt_instr = RuntimeInstruction{ .@"i64.const" = i64_const };
            },
            .@"f32.const" => {
                const float_const = @bitCast(f32, try readU32(&self.code));
                rt_instr = RuntimeInstruction{ .@"f32.const" = float_const };
            },
            .@"f64.const" => {
                const float_const = @bitCast(f64, try readU64(&self.code));
                rt_instr = RuntimeInstruction{ .@"f64.const" = float_const };
            },
            .@"i32.load" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i32.load" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i64.load" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"f32.load" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"f32.load" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"f64.load" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"f64.load" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.load8_s" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i32.load8_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.load8_u" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i32.load8_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.load16_s" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i32.load16_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.load16_u" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i32.load16_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load8_s" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i64.load8_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load8_u" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i64.load8_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load16_s" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i64.load16_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load16_u" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i64.load16_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load32_s" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i64.load32_s" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.load32_u" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i64.load32_u" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.store" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i32.store" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.store" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i64.store" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"f32.store" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"f32.store" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"f64.store" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"f64.store" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.store8" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i32.store8" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.store16" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i32.store16" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.store8" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i64.store8" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.store16" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i64.store16" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i64.store32" => {
                const alignment = try readULEB128Mem(u32, &self.code);
                const offset = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{
                    .@"i64.store32" = .{
                        .alignment = alignment,
                        .offset = offset,
                    },
                };
            },
            .@"i32.eqz" => rt_instr = RuntimeInstruction.@"i32.eqz",
            .@"i32.eq" => rt_instr = RuntimeInstruction.@"i32.eq",
            .@"i32.ne" => rt_instr = RuntimeInstruction.@"i32.ne",
            .@"i32.lt_s" => rt_instr = RuntimeInstruction.@"i32.lt_s",
            .@"i32.lt_u" => rt_instr = RuntimeInstruction.@"i32.lt_u",
            .@"i32.gt_s" => rt_instr = RuntimeInstruction.@"i32.gt_s",
            .@"i32.gt_u" => rt_instr = RuntimeInstruction.@"i32.gt_u",
            .@"i32.le_s" => rt_instr = RuntimeInstruction.@"i32.le_s",
            .@"i32.le_u" => rt_instr = RuntimeInstruction.@"i32.le_u",
            .@"i32.ge_s" => rt_instr = RuntimeInstruction.@"i32.ge_s",
            .@"i32.ge_u" => rt_instr = RuntimeInstruction.@"i32.ge_u",
            .@"i64.eqz" => rt_instr = RuntimeInstruction.@"i64.eqz",
            .@"i64.eq" => rt_instr = RuntimeInstruction.@"i64.eq",
            .@"i64.ne" => rt_instr = RuntimeInstruction.@"i64.ne",
            .@"i64.lt_s" => rt_instr = RuntimeInstruction.@"i64.lt_s",
            .@"i64.lt_u" => rt_instr = RuntimeInstruction.@"i64.lt_u",
            .@"i64.gt_s" => rt_instr = RuntimeInstruction.@"i64.gt_s",
            .@"i64.gt_u" => rt_instr = RuntimeInstruction.@"i64.gt_u",
            .@"i64.le_s" => rt_instr = RuntimeInstruction.@"i64.le_s",
            .@"i64.le_u" => rt_instr = RuntimeInstruction.@"i64.le_u",
            .@"i64.ge_s" => rt_instr = RuntimeInstruction.@"i64.ge_s",
            .@"i64.ge_u" => rt_instr = RuntimeInstruction.@"i64.ge_u",
            .@"f32.eq" => rt_instr = RuntimeInstruction.@"f32.eq",
            .@"f32.ne" => rt_instr = RuntimeInstruction.@"f32.ne",
            .@"f32.lt" => rt_instr = RuntimeInstruction.@"f32.lt",
            .@"f32.gt" => rt_instr = RuntimeInstruction.@"f32.gt",
            .@"f32.le" => rt_instr = RuntimeInstruction.@"f32.le",
            .@"f32.ge" => rt_instr = RuntimeInstruction.@"f32.ge",
            .@"f64.eq" => rt_instr = RuntimeInstruction.@"f64.eq",
            .@"f64.ne" => rt_instr = RuntimeInstruction.@"f64.ne",
            .@"f64.lt" => rt_instr = RuntimeInstruction.@"f64.lt",
            .@"f64.gt" => rt_instr = RuntimeInstruction.@"f64.gt",
            .@"f64.le" => rt_instr = RuntimeInstruction.@"f64.le",
            .@"f64.ge" => rt_instr = RuntimeInstruction.@"f64.ge",
            .@"i32.clz" => rt_instr = RuntimeInstruction.@"i32.clz",
            .@"i32.ctz" => rt_instr = RuntimeInstruction.@"i32.ctz",
            .@"i32.popcnt" => rt_instr = RuntimeInstruction.@"i32.popcnt",
            .@"i32.add" => rt_instr = RuntimeInstruction.@"i32.add",
            .@"i32.sub" => rt_instr = RuntimeInstruction.@"i32.sub",
            .@"i32.mul" => rt_instr = RuntimeInstruction.@"i32.mul",
            .@"i32.div_s" => rt_instr = RuntimeInstruction.@"i32.div_s",
            .@"i32.div_u" => rt_instr = RuntimeInstruction.@"i32.div_u",
            .@"i32.rem_s" => rt_instr = RuntimeInstruction.@"i32.rem_s",
            .@"i32.rem_u" => rt_instr = RuntimeInstruction.@"i32.rem_u",
            .@"i32.and" => rt_instr = RuntimeInstruction.@"i32.and",
            .@"i32.or" => rt_instr = RuntimeInstruction.@"i32.or",
            .@"i32.xor" => rt_instr = RuntimeInstruction.@"i32.xor",
            .@"i32.shl" => rt_instr = RuntimeInstruction.@"i32.shl",
            .@"i32.shr_s" => rt_instr = RuntimeInstruction.@"i32.shr_s",
            .@"i32.shr_u" => rt_instr = RuntimeInstruction.@"i32.shr_u",
            .@"i32.rotl" => rt_instr = RuntimeInstruction.@"i32.rotl",
            .@"i32.rotr" => rt_instr = RuntimeInstruction.@"i32.rotr",
            .@"i64.clz" => rt_instr = RuntimeInstruction.@"i64.clz",
            .@"i64.ctz" => rt_instr = RuntimeInstruction.@"i64.ctz",
            .@"i64.popcnt" => rt_instr = RuntimeInstruction.@"i64.popcnt",
            .@"i64.add" => rt_instr = RuntimeInstruction.@"i64.add",
            .@"i64.sub" => rt_instr = RuntimeInstruction.@"i64.sub",
            .@"i64.mul" => rt_instr = RuntimeInstruction.@"i64.mul",
            .@"i64.div_s" => rt_instr = RuntimeInstruction.@"i64.div_s",
            .@"i64.div_u" => rt_instr = RuntimeInstruction.@"i64.div_u",
            .@"i64.rem_s" => rt_instr = RuntimeInstruction.@"i64.rem_s",
            .@"i64.rem_u" => rt_instr = RuntimeInstruction.@"i64.rem_u",
            .@"i64.and" => rt_instr = RuntimeInstruction.@"i64.and",
            .@"i64.or" => rt_instr = RuntimeInstruction.@"i64.or",
            .@"i64.xor" => rt_instr = RuntimeInstruction.@"i64.xor",
            .@"i64.shl" => rt_instr = RuntimeInstruction.@"i64.shl",
            .@"i64.shr_s" => rt_instr = RuntimeInstruction.@"i64.shr_s",
            .@"i64.shr_u" => rt_instr = RuntimeInstruction.@"i64.shr_u",
            .@"i64.rotl" => rt_instr = RuntimeInstruction.@"i64.rotl",
            .@"i64.rotr" => rt_instr = RuntimeInstruction.@"i64.rotr",
            .@"f32.abs" => rt_instr = RuntimeInstruction.@"f32.abs",
            .@"f32.neg" => rt_instr = RuntimeInstruction.@"f32.neg",
            .@"f32.ceil" => rt_instr = RuntimeInstruction.@"f32.ceil",
            .@"f32.floor" => rt_instr = RuntimeInstruction.@"f32.floor",
            .@"f32.trunc" => rt_instr = RuntimeInstruction.@"f32.trunc",
            .@"f32.nearest" => rt_instr = RuntimeInstruction.@"f32.nearest",
            .@"f32.sqrt" => rt_instr = RuntimeInstruction.@"f32.sqrt",
            .@"f32.add" => rt_instr = RuntimeInstruction.@"f32.add",
            .@"f32.sub" => rt_instr = RuntimeInstruction.@"f32.sub",
            .@"f32.mul" => rt_instr = RuntimeInstruction.@"f32.mul",
            .@"f32.div" => rt_instr = RuntimeInstruction.@"f32.div",
            .@"f32.min" => rt_instr = RuntimeInstruction.@"f32.min",
            .@"f32.max" => rt_instr = RuntimeInstruction.@"f32.max",
            .@"f32.copysign" => rt_instr = RuntimeInstruction.@"f32.copysign",
            .@"f64.abs" => rt_instr = RuntimeInstruction.@"f64.abs",
            .@"f64.neg" => rt_instr = RuntimeInstruction.@"f64.neg",
            .@"f64.ceil" => rt_instr = RuntimeInstruction.@"f64.ceil",
            .@"f64.floor" => rt_instr = RuntimeInstruction.@"f64.floor",
            .@"f64.trunc" => rt_instr = RuntimeInstruction.@"f64.trunc",
            .@"f64.nearest" => rt_instr = RuntimeInstruction.@"f64.nearest",
            .@"f64.sqrt" => rt_instr = RuntimeInstruction.@"f64.sqrt",
            .@"f64.add" => rt_instr = RuntimeInstruction.@"f64.add",
            .@"f64.sub" => rt_instr = RuntimeInstruction.@"f64.sub",
            .@"f64.mul" => rt_instr = RuntimeInstruction.@"f64.mul",
            .@"f64.div" => rt_instr = RuntimeInstruction.@"f64.div",
            .@"f64.min" => rt_instr = RuntimeInstruction.@"f64.min",
            .@"f64.max" => rt_instr = RuntimeInstruction.@"f64.max",
            .@"f64.copysign" => rt_instr = RuntimeInstruction.@"f64.copysign",
            .@"i32.wrap_i64" => rt_instr = RuntimeInstruction.@"i32.wrap_i64",
            .@"i32.trunc_f32_s" => rt_instr = RuntimeInstruction.@"i32.trunc_f32_s",
            .@"i32.trunc_f32_u" => rt_instr = RuntimeInstruction.@"i32.trunc_f32_u",
            .@"i32.trunc_f64_s" => rt_instr = RuntimeInstruction.@"i32.trunc_f64_s",
            .@"i32.trunc_f64_u" => rt_instr = RuntimeInstruction.@"i32.trunc_f64_u",
            .@"i64.extend_i32_s" => rt_instr = RuntimeInstruction.@"i64.extend_i32_s",
            .@"i64.extend_i32_u" => rt_instr = RuntimeInstruction.@"i64.extend_i32_u",
            .@"i64.trunc_f32_s" => rt_instr = RuntimeInstruction.@"i64.trunc_f32_s",
            .@"i64.trunc_f32_u" => rt_instr = RuntimeInstruction.@"i64.trunc_f32_u",
            .@"i64.trunc_f64_s" => rt_instr = RuntimeInstruction.@"i64.trunc_f64_s",
            .@"i64.trunc_f64_u" => rt_instr = RuntimeInstruction.@"i64.trunc_f64_u",
            .@"f32.convert_i32_s" => rt_instr = RuntimeInstruction.@"f32.convert_i32_s",
            .@"f32.convert_i32_u" => rt_instr = RuntimeInstruction.@"f32.convert_i32_u",
            .@"f32.convert_i64_s" => rt_instr = RuntimeInstruction.@"f32.convert_i64_s",
            .@"f32.convert_i64_u" => rt_instr = RuntimeInstruction.@"f32.convert_i64_u",
            .@"f32.demote_f64" => rt_instr = RuntimeInstruction.@"f32.demote_f64",
            .@"f64.convert_i32_s" => rt_instr = RuntimeInstruction.@"f64.convert_i32_s",
            .@"f64.convert_i32_u" => rt_instr = RuntimeInstruction.@"f64.convert_i32_u",
            .@"f64.convert_i64_s" => rt_instr = RuntimeInstruction.@"f64.convert_i64_s",
            .@"f64.convert_i64_u" => rt_instr = RuntimeInstruction.@"f64.convert_i64_u",
            .@"f64.promote_f32" => rt_instr = RuntimeInstruction.@"f64.promote_f32",
            .@"i32.reinterpret_f32" => rt_instr = RuntimeInstruction.@"i32.reinterpret_f32",
            .@"i64.reinterpret_f64" => rt_instr = RuntimeInstruction.@"i64.reinterpret_f64",
            .@"f32.reinterpret_i32" => rt_instr = RuntimeInstruction.@"f32.reinterpret_i32",
            .@"f64.reinterpret_i64" => rt_instr = RuntimeInstruction.@"f64.reinterpret_i64",
            .@"i32.extend8_s" => rt_instr = RuntimeInstruction.@"i32.extend8_s",
            .@"i32.extend16_s" => rt_instr = RuntimeInstruction.@"i32.extend16_s",
            .@"i64.extend8_s" => rt_instr = RuntimeInstruction.@"i64.extend8_s",
            .@"i64.extend16_s" => rt_instr = RuntimeInstruction.@"i64.extend16_s",
            .@"i64.extend32_s" => rt_instr = RuntimeInstruction.@"i64.extend32_s",
            .trunc_sat => {
                const version = try readULEB128Mem(u32, &self.code);
                rt_instr = RuntimeInstruction{ .trunc_sat = version };
            },
        }

        return rt_instr;
    }
};

pub fn findFunctionEnd(code: []const u8) !InstructionMeta {
    var it = OpcodeIterator.init(code);
    var i: usize = 1;
    while (try it.next()) |meta| {
        if (meta.offset == 0 and meta.instruction == .end) return meta;
        if (meta.offset == 0) continue;

        switch (meta.instruction) {
            .block, .loop, .@"if" => i += 1,
            .end => i -= 1,
            else => {},
        }
        if (i == 0) return meta;
    }
    return error.CouldntFindEnd;
}

pub fn findExprEnd(code: []const u8) !InstructionMeta {
    var it = OpcodeIterator.init(code);
    var i: usize = 1;
    while (try it.next()) |meta| {
        switch (meta.instruction) {
            .end => i -= 1,
            else => {},
        }
        if (i == 0) return meta;
    }
    return error.CouldntFindExprEnd;
}

const InstructionMeta = struct {
    instruction: Opcode,
    offset: usize, // offset from start of function
};

const testing = std.testing;

pub fn readULEB128Mem(comptime T: type, ptr: *[]const u8) !T {
    var buf = std.io.fixedBufferStream(ptr.*);
    const value = try leb.readULEB128(T, buf.reader());
    ptr.*.ptr += buf.pos;
    ptr.*.len -= buf.pos;
    return value;
}

pub fn readILEB128Mem(comptime T: type, ptr: *[]const u8) !T {
    var buf = std.io.fixedBufferStream(ptr.*);
    const value = try leb.readILEB128(T, buf.reader());
    ptr.*.ptr += buf.pos;
    ptr.*.len -= buf.pos;
    return value;
}

pub fn readU32(ptr: *[]const u8) !u32 {
    var buf = std.io.fixedBufferStream(ptr.*);
    const rd = buf.reader();
    const value = try rd.readIntLittle(u32);

    ptr.*.ptr += buf.pos;
    ptr.*.len -= buf.pos;
    return value;
}

pub fn readU64(ptr: *[]const u8) !u64 {
    var buf = std.io.fixedBufferStream(ptr.*);
    const rd = buf.reader();
    const value = try rd.readIntLittle(u64);

    ptr.*.ptr += buf.pos;
    ptr.*.len -= buf.pos;
    return value;
}

pub fn readByte(ptr: *[]const u8) !u8 {
    var buf = std.io.fixedBufferStream(ptr.*);
    const rd = buf.reader();
    const value = try rd.readByte();

    ptr.*.ptr += buf.pos;
    ptr.*.len -= buf.pos;
    return value;
}

pub const Opcode = enum(u8) {
    @"unreachable" = 0x0,
    nop = 0x01,
    block = 0x02,
    loop = 0x03,
    @"if" = 0x04,
    @"else" = 0x05,
    end = 0x0b,
    br = 0x0c,
    br_if = 0x0d,
    br_table = 0x0e,
    @"return" = 0x0f,
    call = 0x10,
    call_indirect = 0x11,
    drop = 0x1a,
    select = 0x1b,
    @"local.get" = 0x20,
    @"local.set" = 0x21,
    @"local.tee" = 0x22,
    @"global.get" = 0x23,
    @"global.set" = 0x24,
    @"i32.load" = 0x28,
    @"i64.load" = 0x29,
    @"f32.load" = 0x2a,
    @"f64.load" = 0x2b,
    @"i32.load8_s" = 0x2c,
    @"i32.load8_u" = 0x2d,
    @"i32.load16_s" = 0x2e,
    @"i32.load16_u" = 0x2f,
    @"i64.load8_s" = 0x30,
    @"i64.load8_u" = 0x31,
    @"i64.load16_s" = 0x32,
    @"i64.load16_u" = 0x33,
    @"i64.load32_s" = 0x34,
    @"i64.load32_u" = 0x35,
    @"i32.store" = 0x36,
    @"i64.store" = 0x37,
    @"f32.store" = 0x38,
    @"f64.store" = 0x39,
    @"i32.store8" = 0x3a,
    @"i32.store16" = 0x3b,
    @"i64.store8" = 0x3c,
    @"i64.store16" = 0x3d,
    @"i64.store32" = 0x3e,
    @"memory.size" = 0x3f,
    @"memory.grow" = 0x40,
    @"i32.const" = 0x41,
    @"i64.const" = 0x42,
    @"f32.const" = 0x43,
    @"f64.const" = 0x44,
    @"i32.eqz" = 0x45,
    @"i32.eq" = 0x46,
    @"i32.ne" = 0x47,
    @"i32.lt_s" = 0x48,
    @"i32.lt_u" = 0x49,
    @"i32.gt_s" = 0x4a,
    @"i32.gt_u" = 0x4b,
    @"i32.le_s" = 0x4c,
    @"i32.le_u" = 0x4d,
    @"i32.ge_s" = 0x4e,
    @"i32.ge_u" = 0x4f,
    @"i64.eqz" = 0x50,
    @"i64.eq" = 0x51,
    @"i64.ne" = 0x52,
    @"i64.lt_s" = 0x53,
    @"i64.lt_u" = 0x54,
    @"i64.gt_s" = 0x55,
    @"i64.gt_u" = 0x56,
    @"i64.le_s" = 0x57,
    @"i64.le_u" = 0x58,
    @"i64.ge_s" = 0x59,
    @"i64.ge_u" = 0x5a,
    @"f32.eq" = 0x5b,
    @"f32.ne" = 0x5c,
    @"f32.lt" = 0x5d,
    @"f32.gt" = 0x5e,
    @"f32.le" = 0x5f,
    @"f32.ge" = 0x60,
    @"f64.eq" = 0x61,
    @"f64.ne" = 0x62,
    @"f64.lt" = 0x63,
    @"f64.gt" = 0x64,
    @"f64.le" = 0x65,
    @"f64.ge" = 0x66,
    @"i32.clz" = 0x67,
    @"i32.ctz" = 0x68,
    @"i32.popcnt" = 0x69,
    @"i32.add" = 0x6a,
    @"i32.sub" = 0x6b,
    @"i32.mul" = 0x6c,
    @"i32.div_s" = 0x6d,
    @"i32.div_u" = 0x6e,
    @"i32.rem_s" = 0x6f,
    @"i32.rem_u" = 0x70,
    @"i32.and" = 0x71,
    @"i32.or" = 0x72,
    @"i32.xor" = 0x73,
    @"i32.shl" = 0x74,
    @"i32.shr_s" = 0x75,
    @"i32.shr_u" = 0x76,
    @"i32.rotl" = 0x77,
    @"i32.rotr" = 0x78,
    @"i64.clz" = 0x79,
    @"i64.ctz" = 0x7a,
    @"i64.popcnt" = 0x7b,
    @"i64.add" = 0x7c,
    @"i64.sub" = 0x7d,
    @"i64.mul" = 0x7e,
    @"i64.div_s" = 0x7f,
    @"i64.div_u" = 0x80,
    @"i64.rem_s" = 0x81,
    @"i64.rem_u" = 0x82,
    @"i64.and" = 0x83,
    @"i64.or" = 0x84,
    @"i64.xor" = 0x85,
    @"i64.shl" = 0x86,
    @"i64.shr_s" = 0x87,
    @"i64.shr_u" = 0x88,
    @"i64.rotl" = 0x89,
    @"i64.rotr" = 0x8a,
    @"f32.abs" = 0x8b,
    @"f32.neg" = 0x8c,
    @"f32.ceil" = 0x8d,
    @"f32.floor" = 0x8e,
    @"f32.trunc" = 0x8f,
    @"f32.nearest" = 0x90,
    @"f32.sqrt" = 0x91,
    @"f32.add" = 0x92,
    @"f32.sub" = 0x93,
    @"f32.mul" = 0x94,
    @"f32.div" = 0x95,
    @"f32.min" = 0x96,
    @"f32.max" = 0x97,
    @"f32.copysign" = 0x98,
    @"f64.abs" = 0x99,
    @"f64.neg" = 0x9a,
    @"f64.ceil" = 0x9b,
    @"f64.floor" = 0x9c,
    @"f64.trunc" = 0x9d,
    @"f64.nearest" = 0x9e,
    @"f64.sqrt" = 0x9f,
    @"f64.add" = 0xa0,
    @"f64.sub" = 0xa1,
    @"f64.mul" = 0xa2,
    @"f64.div" = 0xa3,
    @"f64.min" = 0xa4,
    @"f64.max" = 0xa5,
    @"f64.copysign" = 0xa6,
    @"i32.wrap_i64" = 0xa7,
    @"i32.trunc_f32_s" = 0xa8,
    @"i32.trunc_f32_u" = 0xa9,
    @"i32.trunc_f64_s" = 0xaa,
    @"i32.trunc_f64_u" = 0xab,
    @"i64.extend_i32_s" = 0xac,
    @"i64.extend_i32_u" = 0xad,
    @"i64.trunc_f32_s" = 0xae,
    @"i64.trunc_f32_u" = 0xaf,
    @"i64.trunc_f64_s" = 0xb0,
    @"i64.trunc_f64_u" = 0xb1,
    @"f32.convert_i32_s" = 0xb2,
    @"f32.convert_i32_u" = 0xb3,
    @"f32.convert_i64_s" = 0xb4,
    @"f32.convert_i64_u" = 0xb5,
    @"f32.demote_f64" = 0xb6,
    @"f64.convert_i32_s" = 0xb7,
    @"f64.convert_i32_u" = 0xb8,
    @"f64.convert_i64_s" = 0xb9,
    @"f64.convert_i64_u" = 0xba,
    @"f64.promote_f32" = 0xbb,
    @"i32.reinterpret_f32" = 0xbc,
    @"i64.reinterpret_f64" = 0xbd,
    @"f32.reinterpret_i32" = 0xbe,
    @"f64.reinterpret_i64" = 0xbf,
    @"i32.extend8_s" = 0xc0,
    @"i32.extend16_s" = 0xc1,
    @"i64.extend8_s" = 0xc2,
    @"i64.extend16_s" = 0xc3,
    @"i64.extend32_s" = 0xc4,
    trunc_sat = 0xfc,
};
