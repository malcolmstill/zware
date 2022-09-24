pub const Module = @import("module.zig").Module;
pub const Instance = @import("instance.zig").Instance;
pub const VirtualMachine = @import("vm.zig").VirtualMachine;
pub const Store = @import("store.zig").ArrayListStore;
pub const Function = @import("store/function.zig").Function;
pub const Global = @import("store/global.zig").Global;
pub const Memory = @import("store/memory.zig").Memory;
pub const ValType = @import("valtype.zig").ValType;
pub const WasmError = @import("store/function.zig").WasmError;

test {
    _ = @import("validator.zig");
    _ = @import("vm.zig");
    _ = @import("module.zig");
}
