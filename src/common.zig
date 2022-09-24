const ValueType = @import("value_type.zig").ValueType;
const RefType = @import("value_type.zig").RefType;
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
};

pub const MemType = struct {
    import: ?u32,
    limits: Limit,
};

pub const TableType = struct {
    import: ?u32,
    reftype: RefType,
    limits: Limit,
};

pub const Mutability = enum(u8) {
    Immutable,
    Mutable,
};

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

pub const DataSegment = struct {
    count: u32,
    data: []const u8,
    mode: DataSegmentMode,
};

pub const DataSegmentType = enum {
    Passive,
    Active,
};

pub const DataSegmentMode = union(DataSegmentType) {
    Passive: void,
    Active: struct {
        memidx: u32,
        offset: usize, // index of parsed code representing offset
    },
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
