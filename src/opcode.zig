const std = @import("std");
const leb = std.leb;

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
    @"ref.null" = 0xd0,
    @"ref.is_null" = 0xd1,
    @"ref.func" = 0xd2,
    misc = 0xfc,
};

pub const MiscOpcode = enum(u8) {
    @"i32.trunc_sat_f32_s",
    @"i32.trunc_sat_f32_u",
    @"i32.trunc_sat_f64_s",
    @"i32.trunc_sat_f64_u",
    @"i64.trunc_sat_f32_s",
    @"i64.trunc_sat_f32_u",
    @"i64.trunc_sat_f64_s",
    @"i64.trunc_sat_f64_u",
    @"memory.init",
    @"data.drop",
};

const OpcodeMeta = struct {
    instruction: Opcode,
    offset: usize, // offset from start of function
};

pub const OpcodeIterator = struct {
    function: []const u8,
    code: []const u8,

    pub fn init(function: []const u8) OpcodeIterator {
        return OpcodeIterator{
            .code = function,
            .function = function,
        };
    }

    pub fn next(self: *OpcodeIterator) !?OpcodeMeta {
        if (self.code.len == 0) return null;

        // 1. Get the instruction we're going to return and increment code
        const instr = std.meta.intToEnum(Opcode, self.code[0]) catch return error.IllegalOpcode;
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
                    _ = try readULEB128Mem(u32, &self.code);
                }
            },
            .call_indirect => {
                _ = try readULEB128Mem(u32, &self.code);
                _ = try readByte(&self.code); // is byte correct?
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
            .@"ref.null" => {
                _ = try readULEB128Mem(i32, &self.code);
            },
            .@"ref.func" => {
                _ = try readULEB128Mem(u32, &self.code);
            },
            .misc => self.code = self.code[1..],
            else => {},
        }

        return OpcodeMeta{
            .instruction = instr,
            .offset = offset,
        };
    }
};

pub fn findFunctionEnd(code: []const u8) !OpcodeMeta {
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

pub fn findExprEnd(code: []const u8) !OpcodeMeta {
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

    // The following is a bit of a kludge that should really
    // be fixed in either the std lib readILEB128 or using a
    // a fresh implementation. The issue is that the wasm spec
    // expects the "unused" bits in a negative ILEB128 to all be
    // one and the same bits in a positive ILEB128 to be zero.
    switch (T) {
        i32 => if (buf.pos == 5 and value < 0 and buf.buffer[4] & 0x70 != 0x70) return error.Overflow,
        i64 => if (buf.pos == 10 and value < 0 and buf.buffer[9] & 0x7e != 0x7e) return error.Overflow,
        else => @compileError("readILEB128Mem expects i32 or i64"),
    }

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
