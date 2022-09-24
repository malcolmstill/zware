const ValueType = @import("value_type.zig").ValueType;
const Mutability = @import("common.zig").Mutability;

pub const Global = struct {
    value_type: ValueType,
    mutability: Mutability,
    value: u64,
};
