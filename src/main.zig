pub const Module = @import("module.zig").Module;
pub const Instance = @import("instance.zig").Instance;
pub const VirtualMachine = @import("instance/vm.zig").VirtualMachine;
pub const WasmError = @import("instance/vm.zig").WasmError;
pub const Store = @import("store.zig").ArrayListStore;
pub const Function = @import("store/function.zig").Function;
pub const Global = @import("store/global.zig").Global;
pub const Memory = @import("store/memory.zig").Memory;
pub const ValType = @import("valtype.zig").ValType;
pub const wasi = @import("wasi/wasi.zig");

test {
    _ = @import("module/validator.zig");
    _ = @import("instance/vm.zig");
    _ = @import("module.zig");
}
