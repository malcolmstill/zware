const ValType = @import("valtype.zig").ValType;

pub const Limit = struct {
    min: u32,
    max: ?u32,
};

pub const Mutability = enum(u8) {
    Immutable,
    Mutable,
};

pub const FuncType = struct {
    params: []const ValType,
    results: []const ValType,
};

pub const Function = struct {
    typeidx: u32,
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
    valtype: ValType,
};
