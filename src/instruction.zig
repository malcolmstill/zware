const std = @import("std");
const leb = std.debug.leb;

pub const InstructionIterator = struct {
    function: []const u8,
    code: []const u8,

    pub fn init(function: []const u8) InstructionIterator {
        return InstructionIterator{
            .code = function,
            .function = function,
        };
    }

    pub fn next(self: *InstructionIterator) !?InstructionMeta {
        if (self.code.len == 0) return null;

        // 1. Get the instruction we're going to return and increment code
        const instr = @intToEnum(Instruction, self.code[0]);
        const offset = @ptrToInt(self.code.ptr) - @ptrToInt(self.function.ptr);
        self.code = self.code[1..];

        // 2. Find the start of the next instruction
        // TODO: complete this for all opcodes
        switch (instr) {
            .I32Const,
            .I64Const,
            .LocalGet,
            .LocalSet,
            .If,
            .Call,
            .BrIf,
            => _ = try readULEB128Mem(u32, &self.code),
            .F32Const, .F64Const => self.code = self.code[4..],
            else => {},
        }

        return InstructionMeta{
            .instruction = instr,
            .offset = offset,
        };
    }
};

// findEnd
//
// Finds the end instruction corresponding to the instruction at index 0
// of `code`. The offset is relative to `code[0]` (not the start of the function)
//
// Errors:
// - CouldntFindEnd if we run out of elements in `code`
// - NotBranchTarget if the instruction at `code[0]` isn't branch target
pub fn findEnd(code: []const u8) !InstructionMeta {
    var it = InstructionIterator.init(code);
    var i: usize = 1;
    while (try it.next()) |meta| {
        if (i == 0) return meta;
        if (meta.offset == 0) {
            switch (meta.instruction) {
                .Block, .Loop, .If => continue,
                else => return error.NotBranchTarget,
            }
        }

        switch (meta.instruction) {
            .Block, .Loop, .If => i += 1,
            .End => i -= 1,
            else => {},
        }
    }
    return error.CouldntFindEnd;
}

// findElse
//
// Similar to findEnd but finds the match else branch, if
// one exists
pub fn findElse(code: []const u8) !?InstructionMeta {
    var it = InstructionIterator.init(code);
    var i: usize = 1;
    while (try it.next()) |meta| {
        if (i == 0) return meta;
        if (meta.offset == 0) {
            switch (meta.instruction) {
                .If => continue,
                else => return error.NotBranchTarget,
            }
        }

        switch (meta.instruction) {
            .If => i += 1,
            .Else => i -= 1,
            else => {},
        }
    }
    return null;
}

const InstructionMeta = struct {
    instruction: Instruction,
    offset: usize, // offset from start of function
};

const testing = std.testing;

test "instruction iterator" {
    const ArenaAllocator = std.heap.ArenaAllocator;
    const Module = @import("module.zig").Module;
    var arena = ArenaAllocator.init(testing.allocator);
    defer _ = arena.deinit();

    const bytes = @embedFile("../test/fib.wasm");

    var module = Module.init(&arena.allocator, bytes);
    try module.decode();

    const func = module.codes.items[0];

    var it = InstructionIterator.init(func.code);
    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.LocalGet, .offset = 0 });
    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.I32Const, .offset = 2 });
    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.I32LtS, .offset = 4 });
    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.If, .offset = 5 });

    const if_end_meta = try findEnd(func.code[5..]);
    testing.expectEqual(if_end_meta.offset, 6);

    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.I32Const, .offset = 7 });
    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.Return, .offset = 9 });
    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.End, .offset = 10 });
    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.LocalGet, .offset = 11 });
    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.I32Const, .offset = 13 });
    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.I32Sub, .offset = 15 });
    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.Call, .offset = 16 });
    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.LocalGet, .offset = 18 });
    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.I32Const, .offset = 20 });
    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.I32Sub, .offset = 22 });
    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.Call, .offset = 23 });
    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.I32Add, .offset = 25 });
    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.Return, .offset = 26 });
    testing.expectEqual(try it.next(), InstructionMeta{ .instruction = Instruction.End, .offset = 27 });

    testing.expectEqual(try it.next(), null);
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
    ptr.*.ptr += buf.pos;
    ptr.*.len -= buf.pos;
    return value;
}

pub const Instruction = enum(u8) {
    Unreachable = 0x0,
    Nop = 0x01,
    Block = 0x02, // bt
    Loop = 0x03, // bt
    If = 0x04, // bt
    Else = 0x05,
    End = 0x0b,
    Br = 0x0c,
    BrIf = 0x0d,
    BrTable = 0x0e,
    Return = 0x0f,
    Call = 0x10,
    CallIndirect = 0x11,
    Drop = 0x1a,
    Select = 0x1b,
    LocalGet = 0x20,
    LocalSet = 0x21,
    LocalTee = 0x22,
    GlobalGet = 0x23,
    GlobalSet = 0x24,
    I32Load = 0x28,
    I64Load = 0x29,
    F32Load = 0x2a,
    F64Load = 0x2b,
    I32Load8S = 0x2c,
    I32Load8U = 0x2d,
    I32Load16S = 0x2e,
    I32Load16U = 0x2f,
    I64Load8S = 0x30,
    I64Load8U = 0x31,
    I64Load16S = 0x32,
    I64Load16U = 0x33,
    I64Load32S = 0x34,
    I64Load32U = 0x35,
    I32Store = 0x36,
    I64Store = 0x37,
    F32Store = 0x38,
    F64Store = 0x39,
    I32Store8 = 0x3a,
    I32Store16 = 0x3b,
    I64Store8 = 0x3c,
    I64Store16 = 0x3d,
    I64Store32 = 0x3e,
    MemorySize = 0x3f,
    MemoryGrow = 0x40,
    I32Const = 0x41,
    I64Const = 0x42,
    F32Const = 0x43,
    F64Const = 0x44,
    I32Eqz = 0x45,
    I32Eq = 0x46,
    I32Ne = 0x47,
    I32LtS = 0x48,
    I32LtU = 0x49,
    I32GtS = 0x4a,
    I32GtU = 0x4b,
    I32LeS = 0x4c,
    I32LeU = 0x4d,
    I32GeS = 0x4e,
    I32GeU = 0x4f,
    I64Eqz = 0x50,
    I64Eq = 0x51,
    I64Ne = 0x52,
    I64LtS = 0x53,
    I64LtU = 0x54,
    I64GtS = 0x55,
    I64GtU = 0x56,
    I64LeS = 0x57,
    I64LeU = 0x58,
    I64GeS = 0x59,
    I64GeU = 0x5a,
    F32Eq = 0x5b,
    F32Ne = 0x5c,
    F32Lt = 0x5d,
    F32Gt = 0x5e,
    F32Le = 0x5f,
    F32Ge = 0x60,
    F64Eq = 0x61,
    F64Ne = 0x62,
    F64Lt = 0x63,
    F64Gt = 0x64,
    F64Le = 0x65,
    F64Ge = 0x66,
    I32Clz = 0x67,
    I32Ctz = 0x68,
    I32Popcnt = 0x69,
    I32Add = 0x6a,
    I32Sub = 0x6b,
    I32Mul = 0x6c,
    I32DivS = 0x6d,
    I32DivU = 0x6e,
    I32RemS = 0x6f,
    I32RemU = 0x70,
    I32And = 0x71,
    I32Or = 0x72,
    I32Xor = 0x73,
    I32Shl = 0x74,
    I32ShrS = 0x75,
    I32ShrU = 0x76,
    I32Rotl = 0x77,
    I32Rotr = 0x78,
    I64Clz = 0x79,
    I64Ctz = 0x7a,
    I64Popcnt = 0x7b,
    I64Add = 0x7c,
    I64Sub = 0x7d,
    I64Mul = 0x7e,
    I64DivS = 0x7f,
    I64DivU = 0x80,
    I64RemS = 0x81,
    I64RemU = 0x82,
    I64And = 0x83,
    I64Or = 0x84,
    I64Xor = 0x85,
    I64Shl = 0x86,
    I64ShrS = 0x87,
    I64ShrU = 0x88,
    I64Rotl = 0x89,
    I64Rotr = 0x8a,
    F32Abs = 0x8b,
    F32Neg = 0x8c,
    F32Ceil = 0x8d,
    F32Floor = 0x8e,
    F32Trunc = 0x8f,
    F32Nearest = 0x90,
    F32Sqrt = 0x91,
    F32Add = 0x92,
    F32Sub = 0x93,
    F32Mul = 0x94,
    F32Div = 0x95,
    F32Min = 0x96,
    F32Max = 0x97,
    F32CopySign = 0x98,
    F64Abs = 0x99,
    F64Neg = 0x9a,
    F64Ceil = 0x9b,
    F64Floor = 0x9c,
    F64Trunc = 0x9d,
    F64Nearest = 0x9e,
    F64Sqrt = 0x9f,
    F64Add = 0xa0,
    F64Sub = 0xa1,
    F64Mul = 0xa2,
    F64Div = 0xa3,
    F64Min = 0xa4,
    F64Max = 0xa5,
    F64CopySign = 0xa6,
    I32WrapI64 = 0xa7,
    I32TruncF32S = 0xa8,
    I32TruncF32U = 0xa9,
    I32TruncF64S = 0xaa,
    I32TruncF64U = 0xab,
    I64ExtendI32S = 0xac,
    I64ExtendI32U = 0xad,
    I64TruncF32S = 0xae,
    I64TruncF32U = 0xaf,
    I64TruncF64S = 0xb0,
    I64TruncF64U = 0xb1,
    F32ConvertI32S = 0xb2,
    F32ConvertI32U = 0xb3,
    F32ConvertI64S = 0xb4,
    F32ConvertI64U = 0xb5,
    F32DemoteF64 = 0xb6,
    F64ConvertI32S = 0xb7,
    F64ConvertI32U = 0xb8,
    F64ConvertI64S = 0xb9,
    F64ConvertI64U = 0xba,
    F64PromoteF32 = 0xbb,
    I32ReinterpretF32 = 0xbc,
    I64ReinterpretF64 = 0xbd,
    F32ReinterpretI32 = 0xbe,
    F64ReinterpretI64 = 0xbf,
    I32Extend8S = 0xc0,
    I32Extend16S = 0xc1,
    I64Extend8S = 0xc2,
    I64Extend16S = 0xc3,
    I64Extend32S = 0xc4,
    // reserved
    // I32TruncSatF32S = 0xfc,
};
