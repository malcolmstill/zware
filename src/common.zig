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

pub const Code = struct {
    code: []const u8,
};

pub const Data = struct {
    mem_idx: u32,
    instr: Instruction,
    mem_offset: u32,
    data: []const u8,
};

pub const Tag = enum(u8) {
    Func,
    Table,
    Mem,
    Global,
};
