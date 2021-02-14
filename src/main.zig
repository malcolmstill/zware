pub const Module = @import("module.zig").Module;
pub const ModuleInstance = @import("module.zig").ModuleInstance;
pub const Store = @import("interpreter/store.zig").Store;
pub const Memory = @import("interpreter/store.zig").Memory;

test "" {
    _ = @import("validator.zig");
    _ = @import("interpreter.zig");
    _ = @import("module.zig");
}
