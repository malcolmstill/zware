const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "fib",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/fib.zig"),
            .target = target,
            .optimize = optimize,
        }),
        .use_llvm = true,
    });
    exe.root_module.addAnonymousImport("zware", .{
        .root_source_file = b.path("../../src/main.zig"),
    });
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    passthroughArgs(b, run_cmd);

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}

// zig 0.17.0 and 0.16.0 compatible args passthrough function
inline fn passthroughArgs(b: *Build, run: *Build.Step.Run) void {
    if (comptime @import("builtin").zig_version.order(std.SemanticVersion.parse("0.16.0") catch unreachable) == .gt) {
        run.addPassthruArgs();
    } else {
        if (b.args) |args| {
            for (args) |arg| run.addArg(arg);
        }
    }
}
