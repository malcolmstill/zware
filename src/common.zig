const Opcode = @import("instruction.zig").Opcode;
const Instruction = @import("function.zig").Instruction;

pub const LimitType = enum(u8) {
    Min,
    MinMax,
};

pub fn limitMatch(min_imported: u32, max_imported: ?u32, min_stated: u32, max_stated: ?u32) bool {
    // TODO: this is a bit confusing, clean it up
    if (min_imported < min_stated) return false;
    if (max_stated) |defined_max| {
        if (max_imported) |imported_max| {
            if (!(imported_max <= defined_max)) {
                return false;
            }
        } else {
            return false;
        }
    }
    return true;
}

// Table / Memory
pub const Limit = struct {
    min: u32,
    max: ?u32,
    import: ?u32,
};

pub const Mutability = enum(u8) {
    Immutable,
    Mutable,
};

pub const NumType = enum(u8) {
    I32 = 0x7F,
    I64 = 0x7E,
    F32 = 0x7D,
    F64 = 0x7C,
    V128 = 0x7B,
};

pub const RefType = enum(u8) {
    FuncRef = 0x70,
    ExternRef = 0x6F,
};

pub const ValueType = enum(u8) {
    I32 = 0x7F,
    I64 = 0x7E,
    F32 = 0x7D,
    F64 = 0x7C,
    V128 = 0x7B,
    FuncRef = 0x70,
    ExternRef = 0x6F,
};

pub fn valueTypeFromBlockType(block_type: i32) !ValueType {
    return switch (block_type) {
        -0x01 => .I32,
        -0x02 => .I64,
        -0x03 => .F32,
        -0x04 => .F64,
        else => error.UnexpectedBlockType,
    };
}

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
    params: []const ValueType,
    results: []const ValueType,
};

pub const Function = struct {
    typeidx: u32,
    import: ?u32,
};

pub const Global = struct {
    value_type: ValueType,
    mutability: Mutability,
    start: ?usize,
    import: ?u32,
};

pub const Import = struct {
    module: []const u8,
    name: []const u8,
    desc_tag: Tag,
    // desc: u8,
};

pub const Export = struct {
    name: []const u8,
    tag: Tag,
    index: u32,
};

pub const Element = struct {};

pub const Segment = struct {
    index: u32,
    start: usize, // Initialisation code start
    count: u32, // Number of elements in data (useful when data is not []u8)
    data: []const u8,
};

pub const ElementSegment = struct {
    reftype: RefType,
    init: usize, // Offset into element_init_offset of first init expression code offset
    count: u32, // Number of element_init_offset values for this segment (we have an array of initialisation functions)
    mode: ElementSegmentMode,
};

pub const ElementSegmentType = enum {
    Passive,
    Active,
    Declarative,
};

pub const ElementSegmentMode = union(ElementSegmentType) {
    Passive: void,
    Active: struct {
        tableidx: u32,
        offset: usize, // index of parsed code representing offset
    },
    Declarative: void,
};

pub const Tag = enum(u8) {
    Func,
    Table,
    Mem,
    Global,
};

pub const Range = struct {
    offset: usize = 0,
    count: usize = 0,
};

pub const LocalType = struct {
    count: u32,
    value_type: ValueType,
};
