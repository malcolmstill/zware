const Instruction = @import("instruction.zig").Instruction;

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
    code: ?[]const u8,
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
