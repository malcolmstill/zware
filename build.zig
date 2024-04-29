const Build = @import("std").Build;

pub fn build(b: *Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const zware_module = b.createModule(.{
        .root_source_file = .{ .path = "src/main.zig" },
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
    const unittest_step = b.step("unittest", "Run the library unittests");
    unittest_step.dependOn(&run_main_tests.step);

    const testrunner = b.addExecutable(.{
        .name = "testrunner",
        .root_source_file = .{ .path = "test/testrunner/src/testrunner.zig" },
        .target = target,
        .optimize = optimize,
    });
    testrunner.root_module.addImport("zware", zware_module);

    const testsuite_step = b.step("testsuite", "Run all the testsuite tests");
    for (test_names) |test_name| {
        const run_test = b.addRunArtifact(testrunner);
        run_test.addFileArg(.{ .path = b.fmt("test/testsuite-generated/{s}.json", .{test_name}) });
        run_test.cwd = .{ .path = b.pathFromRoot("test/testsuite-generated") };
        const step = b.step(b.fmt("test-{s}", .{test_name}), b.fmt("Run the '{s}' test", .{test_name}));
        step.dependOn(&run_test.step);
        testsuite_step.dependOn(&run_test.step);
    }

    const test_step = b.step("test", "Run all the tests");
    test_step.dependOn(unittest_step);
    test_step.dependOn(testsuite_step);
}

const test_names = [_][]const u8 {
    "address",
    "align",
    "binary-leb128",
    "binary",
    "block",
    "br_if",
    "br_table",
    "br",
    "bulk",
    "call_indirect",
    "call",
    "comments",
    "const",
    "conversions",
    "custom",
    "data",
    "elem",
    "endianness",
    "exports",
    "f32_bitwise",
    "f32_cmp",
    "f32",
    "f64_bitwise",
    "f64_cmp",
    "f64",
    "fac",
    "float_exprs",
    "float_literals",
    "float_memory",
    "float_misc",
    "forward",
    "func_ptrs",
    "func",
    "global",
    "i32",
    "i64",
    "if",
    "imports",
    "inline-module",
    "int_exprs",
    "int_literals",
    "labels",
    "left-to-right",
    "linking",
    "load",
    "local_get",
    "local_set",
    "local_tee",
    "loop",
    "memory_copy",
    "memory_fill",
    "memory_grow",
    "memory_init",
    "memory_redundancy",
    "memory_size",
    "memory_trap",
    "memory",
    "names",
    "nop",
    "ref_func",
    "ref_is_null",
    "ref_null",
    "return",
    "select",
    "skip-stack-guard-page",
    "stack",
    "start",
    "store",
    "switch",
    "table_copy",
    "table_fill",
    "table_get",
    "table_grow",
    "table_init",
    "table_set",
    "table_size",
    "table-sub",
    "table",
    "token",
    "traps",
    "type",
    "unreachable",
    "unreached-invalid",
    "unreached-valid",
    "unwind",
    "utf8-custom-section-id",
    "utf8-import-field",
    "utf8-import-module",
    "utf8-invalid-encoding",
};
