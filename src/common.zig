const Instruction = @import("instruction.zig").Instruction;

pub const LimitType = enum(u8) {
    Min,
    MinMax,
};

pub const Limit = struct {
    min: u32,
    max: u32,
};

pub const Mutability = enum(u8) {
    Immutable,
    Mutable,
};

pub const ValueType = enum(u8) {
    I32 = 0x7F,
    I64 = 0x7E,
    F32 = 0x7D,
    F64 = 0x7C,
};

pub fn toValueType(comptime t: type) ValueType {
    return switch (t) {
        i32 => .I32,
        i64 => .I64,
        f32 => .F32,
        f64 => .F64,
        u32 => .I32,
        else => @compileError("toValueType: unsupported type: " ++ @typeName(t)),
    };
}

pub const FuncType = struct {
    params_offset: usize,
    params_count: usize,
    results_offset: usize,
    results_count: usize,
};

pub const Global = struct {
    value_type: ValueType,
    mutability: Mutability,
    code: []const u8,
};

pub const Import = struct {
    module: []const u8,
    name: []const u8,
    desc_tag: Tag,
    desc: u8,
};

pub const Export = struct {
    name: []const u8,
    tag: Tag,
    index: u32,
};

pub const Element = struct {};

// Code
//
// - locals: slice of locals definition [count type]*
// - locals_count: the total number of locals (this does not include params)
// - code: []u8 byte slice of wasm code (index 0 is immediately after
//         the locals definitions)
pub const Code = struct {
    locals: []const u8,
    locals_count: usize,
    code: []const u8,
};

pub const Segment = struct {
    index: u32,
    offset: []const u8,
    count: u32, // Number of elements in data (useful when data is not []u8)
    data: []const u8,
};

pub const Tag = enum(u8) {
    Func,
    Table,
    Mem,
    Global,
};
