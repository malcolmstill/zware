pub const Module = @import("module.zig").Module;
pub const Instance = @import("instance.zig").Instance;
pub const Store = @import("store.zig").Store;
pub const Memory = @import("memory.zig").Memory;
pub const ValueType = @import("common.zig").ValueType;

test "" {
    _ = @import("validator.zig");
    _ = @import("interpreter.zig");
    _ = @import("module.zig");
}
