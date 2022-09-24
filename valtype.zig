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
        -0x10 => .FuncRef,
        -0x11 => .ExternRef,
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
