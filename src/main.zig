pub const Module = @import("module.zig").Module;
pub const Store = @import("interpreter/store.zig").Store;

test "" {
    _ = @import("validator.zig");
    _ = @import("interpreter.zig");
    _ = @import("module.zig");
}
