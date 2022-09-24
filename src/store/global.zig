const ValType = @import("../valtype.zig").ValType;
const Mutability = @import("../module.zig").Mutability;

pub const Global = struct {
    valtype: ValType,
    mutability: Mutability,
    value: u64,
};
