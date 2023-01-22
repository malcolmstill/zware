const MiscOpcode = @import("opcode.zig").MiscOpcode;
const RefType = @import("valtype.zig").RefType;
const Instruction = @import("instance/vm.zig").VirtualMachine.Instruction;

pub const RrOpcode = enum(u8) {
    @"unreachable" = 0x0,
    nop = 0x01,
    block = 0x02,
    loop = 0x03,
    @"if" = 0x04,
    @"else" = 0x05,
    if_no_else = 0x06,
    end = 0x0b,
    br = 0x0c,
    br_if = 0x0d,
    br_table = 0x0e,
    @"return" = 0x0f,
    call = 0x10,
    call_indirect = 0x11,
    fast_call = 0x12,
    drop = 0x1a,
    select = 0x1b,
    @"local.get" = 0x20,
    @"local.set" = 0x21,
    @"local.tee" = 0x22,
    @"global.get" = 0x23,
    @"global.set" = 0x24,
    @"table.get" = 0x25,
    @"table.set" = 0x26,
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
    @"i32.pub const" = 0x41,
    @"i64.pub const" = 0x42,
    @"f32.pub const" = 0x43,
    @"f64.pub const" = 0x44,
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

pub fn nextIp(comptime Type: type) comptime_int {
    const word_size_bits = 8 * @sizeOf(usize);
    const bits = @bitSizeOf(Type);
    const round_up_bits = if (bits % word_size_bits == 0) 0 else word_size_bits - (bits % word_size_bits);
    return ((bits + round_up_bits) / word_size_bits) + 1;
}

// fn nextIp(comptime Type: type) comptime_int {
//     pub const word_size_bits = 8 * @sizeOf(usize);

//     comptime var bits = switch (@typeInfo(Type)) {
//         .Struct => |info| blk: {
//             comptime var i = 0;
//             inline for (info.fields) |field| {
//                 switch (@typeInfo(field.type)) {
//                     .Int => |int| i += int.bits,
//                     else => @compileError("Expected int"),
//                 }
//             }
//             break :blk i;
//         },
//         .Void => 0,
//         .Int => |int| int.bits,
//         else => @compileError("Unexpected type"),
//     };

//     pub const round_up_bits = if (bits % word_size_bits == 0) 0 else word_size_bits - (bits % word_size_bits);

//     return ((bits + round_up_bits) / word_size_bits) + 1;
// }

pub fn meta(ip: [*]Instruction, comptime Type: type) type {
    const start = ip + 1;

    return @as(@TypeOf(Type), @bitCast(start));
}

// fn getFieldType(comptime Type: type, field: []pub const u8) type {
//     switch (@typeInfo(Type)) {
//         .Struct => |info| blk: {
//             comptime var i = 0;
//             inline for (info.fields) |field| {
//                 switch (@typeInfo(field.type)) {
//                     .Int => |int| i += int.bits,
//                     else => @compileError("Expected int"),
//                 }
//             }
//             break :blk i;
//         },
//         .Void => 0,
//         .Int => |int| int.type,
//         else => @compileError("Unexpected type"),
//     };
// }

// fn immediate(comptime Struct: type, comptime field: []pub const u8) getFieldType(Struct, field) {
//     pub const struct_info = switch (@typeInfo(Type)) {
//         .Struct => |info| info,
//         else => @compileError("Expected struct"),
//     };

//     comptime var i = 0;
//     inline for (struct_info.fields) |field| {
//         switch (@typeInfo(field.type)) {
//             .Int => |int| i += int.bits,
//             else => @compileError("Expected int"),
//         }
//     }
// }

test {
    const std = @import("std");
    const testing = std.testing;

    try testing.expectEqual(1, nextIp(@"unreachable"));
    try testing.expectEqual(1, nextIp(nop));
    try testing.expectEqual(2, nextIp(block));
    try testing.expectEqual(2, nextIp(loop));
    try testing.expectEqual(3, nextIp(@"if"));
    try testing.expectEqual(2, nextIp(if_no_else));
    try testing.expectEqual(3, nextIp(fast_call));
    try testing.expectEqual(2, nextIp(br));
}

pub const Rr = union(RrOpcode) {
    @"unreachable": @"unreachable",
    nop: nop,
    block: block,
    loop: loop,
    @"if": @"if",
    @"else": @"else",
    if_no_else: if_no_else,
    end: end,
    br: br,
    br_if: br_if,
    br_table: br_table,
    @"return": @"return",
    call: call,
    call_indirect: call_indirect,
    fast_call: fast_call,
    drop: drop,
    select: select,
    @"local.get": @"local.get",
    @"local.set": @"local.set",
    @"local.tee": @"local.tee",
    @"global.get": @"global.get",
    @"global.set": @"global.set",
    @"table.get": @"table.get",
    @"table.set": @"table.set",
    @"i32.load": @"i32.load",
    @"i64.load": @"i64.load",
    @"f32.load": @"f32.load",
    @"f64.load": @"f64.load",
    @"i32.load8_s": @"i32.load8_s",
    @"i32.load8_u": @"i32.load8_u",
    @"i32.load16_s": @"i32.load16_s",
    @"i32.load16_u": @"i32.load16_u",
    @"i64.load8_s": @"i64.load8_s",
    @"i64.load8_u": @"i64.load8_u",
    @"i64.load16_s": @"i64.load16_s",
    @"i64.load16_u": @"i64.load16_u",
    @"i64.load32_s": @"i64.load32_s",
    @"i64.load32_u": @"i64.load32_u",
    @"i32.store": @"i32.store",
    @"i64.store": @"i64.store",
    @"f32.store": @"f32.store",
    @"f64.store": @"f64.store",
    @"i32.store8": @"i32.store8",
    @"i32.store16": @"i32.store16",
    @"i64.store8": @"i64.store8",
    @"i64.store16": @"i64.store16",
    @"i64.store32": @"i64.store32",
    @"memory.size": @"memory.size",
    @"memory.grow": @"memory.grow",
    @"i32.pub const": @"i32.pub const",
    @"i64.pub const": @"i64.pub const",
    @"f32.pub const": @"f32.pub const",
    @"f64.pub const": @"f64.pub const",
    @"i32.eqz": @"i32.eqz",
    @"i32.eq": @"i32.eq",
    @"i32.ne": @"i32.ne",
    @"i32.lt_s": @"i32.lt_s",
    @"i32.lt_u": @"i32.lt_u",
    @"i32.gt_s": @"i32.gt_s",
    @"i32.gt_u": @"i32.gt_u",
    @"i32.le_s": @"i32.le_s",
    @"i32.le_u": @"i32.le_u",
    @"i32.ge_s": @"i32.ge_s",
    @"i32.ge_u": @"i32.ge_u",
    @"i64.eqz": @"i64.eqz",
    @"i64.eq": @"i64.eq",
    @"i64.ne": @"i64.ne",
    @"i64.lt_s": @"i64.lt_s",
    @"i64.lt_u": @"i64.lt_u",
    @"i64.gt_s": @"i64.gt_s",
    @"i64.gt_u": @"i64.gt_u",
    @"i64.le_s": @"i64.le_s",
    @"i64.le_u": @"i64.le_u",
    @"i64.ge_s": @"i64.ge_s",
    @"i64.ge_u": @"i64.ge_u",
    @"f32.eq": @"f32.eq",
    @"f32.ne": @"f32.ne",
    @"f32.lt": @"f32.lt",
    @"f32.gt": @"f32.gt",
    @"f32.le": @"f32.le",
    @"f32.ge": @"f32.ge",
    @"f64.eq": @"f64.eq",
    @"f64.ne": @"f64.ne",
    @"f64.lt": @"f64.lt",
    @"f64.gt": @"f64.gt",
    @"f64.le": @"f64.le",
    @"f64.ge": @"f64.ge",
    @"i32.clz": @"i32.clz",
    @"i32.ctz": @"i32.ctz",
    @"i32.popcnt": @"i32.popcnt",
    @"i32.add": @"i32.add",
    @"i32.sub": @"i32.sub",
    @"i32.mul": @"i32.mul",
    @"i32.div_s": @"i32.div_s",
    @"i32.div_u": @"i32.div_u",
    @"i32.rem_s": @"i32.rem_s",
    @"i32.rem_u": @"i32.rem_u",
    @"i32.and": @"i32.and",
    @"i32.or": @"i32.or",
    @"i32.xor": @"i32.xor",
    @"i32.shl": @"i32.shl",
    @"i32.shr_s": @"i32.shr_s",
    @"i32.shr_u": @"i32.shr_u",
    @"i32.rotl": @"i32.rotl",
    @"i32.rotr": @"i32.rotr",
    @"i64.clz": @"i64.clz",
    @"i64.ctz": @"i64.ctz",
    @"i64.popcnt": @"i64.popcnt",
    @"i64.add": @"i64.add",
    @"i64.sub": @"i64.sub",
    @"i64.mul": @"i64.mul",
    @"i64.div_s": @"i64.div_s",
    @"i64.div_u": @"i64.div_u",
    @"i64.rem_s": @"i64.rem_s",
    @"i64.rem_u": @"i64.rem_u",
    @"i64.and": @"i64.and",
    @"i64.or": @"i64.or",
    @"i64.xor": @"i64.xor",
    @"i64.shl": @"i64.shl",
    @"i64.shr_s": @"i64.shr_s",
    @"i64.shr_u": @"i64.shr_u",
    @"i64.rotl": @"i64.rotl",
    @"i64.rotr": @"i64.rotr",
    @"f32.abs": @"f32.abs",
    @"f32.neg": @"f32.neg",
    @"f32.ceil": @"f32.ceil",
    @"f32.floor": @"f32.floor",
    @"f32.trunc": @"f32.trunc",
    @"f32.nearest": @"f32.nearest",
    @"f32.sqrt": @"f32.sqrt",
    @"f32.add": @"f32.add",
    @"f32.sub": @"f32.sub",
    @"f32.mul": @"f32.mul",
    @"f32.div": @"f32.div",
    @"f32.min": @"f32.min",
    @"f32.max": @"f32.max",
    @"f32.copysign": @"f32.copysign",
    @"f64.abs": @"f64.abs",
    @"f64.neg": @"f64.neg",
    @"f64.ceil": @"f64.ceil",
    @"f64.floor": @"f64.floor",
    @"f64.trunc": @"f64.trunc",
    @"f64.nearest": @"f64.nearest",
    @"f64.sqrt": @"f64.sqrt",
    @"f64.add": @"f64.add",
    @"f64.sub": @"f64.sub",
    @"f64.mul": @"f64.mul",
    @"f64.div": @"f64.div",
    @"f64.min": @"f64.min",
    @"f64.max": @"f64.max",
    @"f64.copysign": @"f64.copysign",
    @"i32.wrap_i64": @"i32.wrap_i64",
    @"i32.trunc_f32_s": @"i32.trunc_f32_s",
    @"i32.trunc_f32_u": @"i32.trunc_f32_u",
    @"i32.trunc_f64_s": @"i32.trunc_f64_s",
    @"i32.trunc_f64_u": @"i32.trunc_f64_u",
    @"i64.extend_i32_s": @"i64.extend_i32_s",
    @"i64.extend_i32_u": @"i64.extend_i32_u",
    @"i64.trunc_f32_s": @"i64.trunc_f32_s",
    @"i64.trunc_f32_u": @"i64.trunc_f32_u",
    @"i64.trunc_f64_s": @"i64.trunc_f64_s",
    @"i64.trunc_f64_u": @"i64.trunc_f64_u",
    @"f32.convert_i32_s": @"f32.convert_i32_s",
    @"f32.convert_i32_u": @"f32.convert_i32_u",
    @"f32.convert_i64_s": @"f32.convert_i64_s",
    @"f32.convert_i64_u": @"f32.convert_i64_u",
    @"f32.demote_f64": @"f32.demote_f64",
    @"f64.convert_i32_s": @"f64.convert_i32_s",
    @"f64.convert_i32_u": @"f64.convert_i32_u",
    @"f64.convert_i64_s": @"f64.convert_i64_s",
    @"f64.convert_i64_u": @"f64.convert_i64_u",
    @"f64.promote_f32": @"f64.promote_f32",
    @"i32.reinterpret_f32": @"i32.reinterpret_f32",
    @"i64.reinterpret_f64": @"i64.reinterpret_f64",
    @"f32.reinterpret_i32": @"f32.reinterpret_i32",
    @"f64.reinterpret_i64": @"f64.reinterpret_i64",
    @"i32.extend8_s": @"i32.extend8_s",
    @"i32.extend16_s": @"i32.extend16_s",
    @"i64.extend8_s": @"i64.extend8_s",
    @"i64.extend16_s": @"i64.extend16_s",
    @"i64.extend32_s": @"i64.extend32_s",
    @"ref.null": @"ref.null",
    @"ref.is_null": @"ref.is_null",
    @"ref.func": @"ref.func",
    misc: void,
};

pub const @"unreachable" = void;
pub const nop = void;
pub const block = packed struct {
    branch_target: [*]Instruction,
    param_arity: u16,
    return_arity: u16,
};
pub const loop = packed struct {
    branch_target: [*]Instruction,
    param_arity: u16,
    return_arity: u16,
};
pub const @"if" = packed struct {
    branch_target: [*]Instruction,
    else_ip: [*]Instruction,
    param_arity: u16,
    return_arity: u16,
};
pub const @"else" = void;
pub const if_no_else = packed struct {
    branch_target: [*]Instruction,
    param_arity: u16,
    return_arity: u16,
};
pub const end = void;
pub const br = u32;
pub const br_if = u32;
pub const br_table = packed struct {
    ls: Range,
    ln: u32,
};
pub const @"return" = void;
pub const call = *Instruction;
pub const call_indirect = packed struct {
    typeidx: u32,
    tableidx: u32,
};
pub const fast_call = packed struct {
    start: [*]Instruction,
    locals: u16,
    params: u16,
    results: u16,
    required_stack_space: u16,
};
pub const drop = void;
pub const select = void;
pub const @"local.get" = u32;
pub const @"local.set" = u32;
pub const @"local.tee" = u32;
pub const @"global.get" = u32;
pub const @"global.set" = u32;
pub const @"table.get" = u32; // tableidx
pub const @"table.set" = u32; // tableidx
pub const @"i32.load" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i64.load" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"f32.load" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"f64.load" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i32.load8_s" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i32.load8_u" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i32.load16_s" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i32.load16_u" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i64.load8_s" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i64.load8_u" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i64.load16_s" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i64.load16_u" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i64.load32_s" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i64.load32_u" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i32.store" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i64.store" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"f32.store" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"f64.store" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i32.store8" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i32.store16" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i64.store8" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i64.store16" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"i64.store32" = packed struct {
    alignment: u32,
    offset: u32,
};
pub const @"memory.size" = u32;
pub const @"memory.grow" = u32;
pub const @"i32.pub const" = i32;
pub const @"i64.pub const" = i64;
pub const @"f32.pub const" = f32;
pub const @"f64.pub const" = f64;
pub const @"i32.eqz" = void;
pub const @"i32.eq" = void;
pub const @"i32.ne" = void;
pub const @"i32.lt_s" = void;
pub const @"i32.lt_u" = void;
pub const @"i32.gt_s" = void;
pub const @"i32.gt_u" = void;
pub const @"i32.le_s" = void;
pub const @"i32.le_u" = void;
pub const @"i32.ge_s" = void;
pub const @"i32.ge_u" = void;
pub const @"i64.eqz" = void;
pub const @"i64.eq" = void;
pub const @"i64.ne" = void;
pub const @"i64.lt_s" = void;
pub const @"i64.lt_u" = void;
pub const @"i64.gt_s" = void;
pub const @"i64.gt_u" = void;
pub const @"i64.le_s" = void;
pub const @"i64.le_u" = void;
pub const @"i64.ge_s" = void;
pub const @"i64.ge_u" = void;
pub const @"f32.eq" = void;
pub const @"f32.ne" = void;
pub const @"f32.lt" = void;
pub const @"f32.gt" = void;
pub const @"f32.le" = void;
pub const @"f32.ge" = void;
pub const @"f64.eq" = void;
pub const @"f64.ne" = void;
pub const @"f64.lt" = void;
pub const @"f64.gt" = void;
pub const @"f64.le" = void;
pub const @"f64.ge" = void;
pub const @"i32.clz" = void;
pub const @"i32.ctz" = void;
pub const @"i32.popcnt" = void;
pub const @"i32.add" = void;
pub const @"i32.sub" = void;
pub const @"i32.mul" = void;
pub const @"i32.div_s" = void;
pub const @"i32.div_u" = void;
pub const @"i32.rem_s" = void;
pub const @"i32.rem_u" = void;
pub const @"i32.and" = void;
pub const @"i32.or" = void;
pub const @"i32.xor" = void;
pub const @"i32.shl" = void;
pub const @"i32.shr_s" = void;
pub const @"i32.shr_u" = void;
pub const @"i32.rotl" = void;
pub const @"i32.rotr" = void;
pub const @"i64.clz" = void;
pub const @"i64.ctz" = void;
pub const @"i64.popcnt" = void;
pub const @"i64.add" = void;
pub const @"i64.sub" = void;
pub const @"i64.mul" = void;
pub const @"i64.div_s" = void;
pub const @"i64.div_u" = void;
pub const @"i64.rem_s" = void;
pub const @"i64.rem_u" = void;
pub const @"i64.and" = void;
pub const @"i64.or" = void;
pub const @"i64.xor" = void;
pub const @"i64.shl" = void;
pub const @"i64.shr_s" = void;
pub const @"i64.shr_u" = void;
pub const @"i64.rotl" = void;
pub const @"i64.rotr" = void;
pub const @"f32.abs" = void;
pub const @"f32.neg" = void;
pub const @"f32.ceil" = void;
pub const @"f32.floor" = void;
pub const @"f32.trunc" = void;
pub const @"f32.nearest" = void;
pub const @"f32.sqrt" = void;
pub const @"f32.add" = void;
pub const @"f32.sub" = void;
pub const @"f32.mul" = void;
pub const @"f32.div" = void;
pub const @"f32.min" = void;
pub const @"f32.max" = void;
pub const @"f32.copysign" = void;
pub const @"f64.abs" = void;
pub const @"f64.neg" = void;
pub const @"f64.ceil" = void;
pub const @"f64.floor" = void;
pub const @"f64.trunc" = void;
pub const @"f64.nearest" = void;
pub const @"f64.sqrt" = void;
pub const @"f64.add" = void;
pub const @"f64.sub" = void;
pub const @"f64.mul" = void;
pub const @"f64.div" = void;
pub const @"f64.min" = void;
pub const @"f64.max" = void;
pub const @"f64.copysign" = void;
pub const @"i32.wrap_i64" = void;
pub const @"i32.trunc_f32_s" = void;
pub const @"i32.trunc_f32_u" = void;
pub const @"i32.trunc_f64_s" = void;
pub const @"i32.trunc_f64_u" = void;
pub const @"i64.extend_i32_s" = void;
pub const @"i64.extend_i32_u" = void;
pub const @"i64.trunc_f32_s" = void;
pub const @"i64.trunc_f32_u" = void;
pub const @"i64.trunc_f64_s" = void;
pub const @"i64.trunc_f64_u" = void;
pub const @"f32.convert_i32_s" = void;
pub const @"f32.convert_i32_u" = void;
pub const @"f32.convert_i64_s" = void;
pub const @"f32.convert_i64_u" = void;
pub const @"f32.demote_f64" = void;
pub const @"f64.convert_i32_s" = void;
pub const @"f64.convert_i32_u" = void;
pub const @"f64.convert_i64_s" = void;
pub const @"f64.convert_i64_u" = void;
pub const @"f64.promote_f32" = void;
pub const @"i32.reinterpret_f32" = void;
pub const @"i64.reinterpret_f64" = void;
pub const @"f32.reinterpret_i32" = void;
pub const @"f64.reinterpret_i64" = void;
pub const @"i32.extend8_s" = void;
pub const @"i32.extend16_s" = void;
pub const @"i64.extend8_s" = void;
pub const @"i64.extend16_s" = void;
pub const @"i64.extend32_s" = void;
pub const @"ref.null" = RefType;
pub const @"ref.is_null" = void;
pub const @"ref.func" = u32;
pub const misc = void;
pub const @"i32.trunc_sat_f32_s" = void;
pub const @"i32.trunc_sat_f32_u" = void;
pub const @"i32.trunc_sat_f64_s" = void;
pub const @"i32.trunc_sat_f64_u" = void;
pub const @"i64.trunc_sat_f32_s" = void;
pub const @"i64.trunc_sat_f32_u" = void;
pub const @"i64.trunc_sat_f64_s" = void;
pub const @"i64.trunc_sat_f64_u" = void;
pub const @"memory.init" = packed struct {
    dataidx: u32,
    memidx: u32,
};
pub const @"data.drop" = u32;
pub const @"memory.copy" = packed struct {
    src_memidx: u8,
    dest_memidx: u8,
};
pub const @"memory.fill" = u8;
pub const @"table.init" = packed struct {
    tableidx: u32,
    elemidx: u32,
};
pub const @"elem.drop" = packed struct {
    elemidx: u32,
};
pub const @"table.copy" = packed struct {
    dest_tableidx: u32,
    src_tableidx: u32,
};
pub const @"table.grow" = packed struct {
    tableidx: u32,
};
pub const @"table.size" = packed struct {
    tableidx: u32,
};
pub const @"table.fill" = packed struct {
    tableidx: u32,
};

pub const Range = packed struct {
    offset: usize = 0,
    count: usize = 0,
};
