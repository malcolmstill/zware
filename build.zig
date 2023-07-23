const Builder = @import("std").build.Builder;

pub fn build(b: *Builder) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    var zware_module = b.createModule(.{
        .source_file = .{ .path = "src/main.zig" },
    });

    try b.modules.put(b.dupe("zware"), zware_module);

    const lib = b.addStaticLibrary(.{
        .name = "zware",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(lib);

    const main_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .optimize = optimize,
    });

    const run_main_tests = b.addRunArtifact(main_tests);
    const test_step = b.step("test", "Run library tests");
    test_step.dependOn(&run_main_tests.step);
}
