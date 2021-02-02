pub const VM = struct {
    module: *Module, // current module
    function: u32, // current function index
    ip: u32,

    fn execute(self: *VM) !void {
        const code = self.module.codes.items[function];
        const instr = @intToEnum(Instruction, code[ip]);

        switch (instr) {
            else => unreachable,
        }
    }
};
