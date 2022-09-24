const ValType = @import("valtype.zig").ValType;
const Mutability = @import("common.zig").Mutability;

pub const Global = struct {
    valtype: ValType,
    mutability: Mutability,
    value: u64,
};
