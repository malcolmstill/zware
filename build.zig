const std = @import("std");
const Build = std.Build;

pub fn build(b: *Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const zware_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
    });

    try b.modules.put(b.dupe("zware"), zware_module);

    const main_mod = b.addModule("zware", .{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    const lib = b.addLibrary(.{
        .name = "zware",
        .root_module = main_mod,
    });
    b.installArtifact(lib);

    const main_tests = b.addTest(.{
        .root_module = main_mod,
        .use_llvm = true,
    });

    const run_main_tests = b.addRunArtifact(main_tests);
    const unittest_step = b.step("unittest", "Run the library unittests");
    unittest_step.dependOn(&run_main_tests.step);

    const wast2json = addWast2Json(b);

    const testrunner = b.addExecutable(.{
        .name = "testrunner",
        .root_module = b.addModule("testrunner", .{
            .root_source_file = b.path("test/testrunner/src/testrunner.zig"),
            .target = target,
            .optimize = optimize,
        }),
        .use_llvm = true,
    });
    testrunner.root_module.addImport("zware", zware_module);

    const testsuite_dep = b.dependency("testsuite", .{});

    const testsuite_step = b.step("testsuite", "Run all the testsuite tests");
    for (test_names) |test_name| {
        const run_wast2json = b.addRunArtifact(wast2json);
        run_wast2json.addFileArg(testsuite_dep.path(b.fmt("{s}.wast", .{test_name})));
        run_wast2json.addArg("-o");
        const json_file = run_wast2json.addOutputFileArg(b.fmt("{s}.json", .{test_name}));

        const run_test = b.addRunArtifact(testrunner);
        run_test.addFileArg(json_file);
        run_test.cwd = json_file.dirname();
        const step = b.step(b.fmt("test-{s}", .{test_name}), b.fmt("Run the '{s}' test", .{test_name}));
        step.dependOn(&run_test.step);
        testsuite_step.dependOn(&run_test.step);
    }

    // WASI testsuite integration - individual and aggregated test steps
    const wasi_testsuite_dep = b.dependency("wasi-testsuite", .{});

    // Aggregated step that runs all WASI tests at once
    const wasi_testsuite_step = b.step("wasi-testsuite", "Run WASI testsuite tests (all languages)");
    const run_wasi_tests = b.addSystemCommand(&.{
        "python3",
        "test-runner/wasi_test_runner.py",
        "-t",
        "tests/c/testsuite/wasm32-wasip1",
        "tests/rust/testsuite/wasm32-wasip1",
        "tests/assemblyscript/testsuite/wasm32-wasip1",
        "-r",
        b.pathFromRoot("test/wasi-testsuite/adapter/zware.py"),
    });
    run_wasi_tests.setCwd(wasi_testsuite_dep.path("."));
    run_wasi_tests.setEnvironmentVariable("ZWARE_RUN", b.getInstallPath(.bin, "zware-run"));
    run_wasi_tests.step.dependOn(b.getInstallStep());
    wasi_testsuite_step.dependOn(&run_wasi_tests.step);

    // Individual test steps - dynamically discovered from testsuite
    // Create a helper function to discover and register tests for a suite
    const TestSuite = struct {
        key: []const u8,
        path: []const u8,
    };

    const test_suites = [_]TestSuite{
        .{ .key = "c", .path = "tests/c/testsuite/wasm32-wasip1" },
        .{ .key = "rust", .path = "tests/rust/testsuite/wasm32-wasip1" },
        .{ .key = "as", .path = "tests/assemblyscript/testsuite/wasm32-wasip1" },
    };

    for (test_suites) |suite| {
        // Get the actual path to the testsuite directory
        const suite_dir_path = wasi_testsuite_dep.path(suite.path).getPath(b);

        // Open and scan the directory for .wasm files
        var suite_dir = std.fs.cwd().openDir(suite_dir_path, .{ .iterate = true }) catch continue;
        defer suite_dir.close();

        var test_list: std.ArrayList([]const u8) = .empty;
        defer test_list.deinit(b.allocator);

        var iter = suite_dir.iterate();
        while (iter.next() catch null) |entry| {
            if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".wasm")) {
                const test_name = entry.name[0 .. entry.name.len - 5]; // remove .wasm extension
                const test_name_owned = b.allocator.dupe(u8, test_name) catch continue;
                test_list.append(b.allocator, test_name_owned) catch continue;
            }
        }

        // Sort test names for deterministic ordering
        std.mem.sort([]const u8, test_list.items, {}, struct {
            fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
                return std.mem.lessThan(u8, lhs, rhs);
            }
        }.lessThan);

        // Create individual build steps for each discovered test
        for (test_list.items) |test_name| {
            const run_test = b.addSystemCommand(&.{
                "python3",
                b.pathFromRoot("test/wasi-testsuite/run-single-wasi-test.py"),
                suite.path,
                test_name,
                "-r",
                b.pathFromRoot("test/wasi-testsuite/adapter/zware.py"),
                "--test-runner",
                "test-runner/wasi_test_runner.py",
            });
            run_test.setCwd(wasi_testsuite_dep.path("."));
            run_test.setEnvironmentVariable("ZWARE_RUN", b.getInstallPath(.bin, "zware-run"));
            run_test.step.dependOn(b.getInstallStep());
            b.step(
                b.fmt("wasi-{s}-{s}", .{ suite.key, test_name }),
                b.fmt("Run WASI {s} test: {s}", .{ suite.key, test_name }),
            ).dependOn(&run_test.step);
        }
    }

    const test_step = b.step("test", "Run all the tests");
    test_step.dependOn(unittest_step);
    test_step.dependOn(testsuite_step);
    // Note: WASI testsuite is not included in default test step due to external dependencies
    // Run it explicitly with: zig build wasi-testsuite

    {
        const exe = b.addExecutable(.{
            .name = "zware-run",
            .root_module = b.addModule("zware-run", .{
                .root_source_file = b.path("tools/zware-run.zig"),
                .target = target,
                .optimize = optimize,
            }),
            .use_llvm = true,
        });
        exe.root_module.addImport("zware", zware_module);
        const install = b.addInstallArtifact(exe, .{});
        b.getInstallStep().dependOn(&install.step);
        const run = b.addRunArtifact(exe);
        run.step.dependOn(&install.step);
        if (b.args) |args| {
            run.addArgs(args);
        }
        b.step("run", "Run the cmdline runner zware-run").dependOn(&run.step);
    }

    {
        const exe = b.addExecutable(.{
            .name = "zware-gen",
            .root_module = b.addModule("zware-gen", .{
                .root_source_file = b.path("tools/zware-gen.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        exe.root_module.addImport("zware", zware_module);
        const install = b.addInstallArtifact(exe, .{});
        b.getInstallStep().dependOn(&install.step);
        const run = b.addRunArtifact(exe);
        run.step.dependOn(&install.step);
        if (b.args) |args| {
            run.addArgs(args);
        }
        b.step("gen", "Run the cmdline runner zware-gen").dependOn(&run.step);
    }
}

fn addWast2Json(b: *Build) *Build.Step.Compile {
    const wabt_dep = b.dependency("wabt", .{});

    const wabt_debug = b.option(enum {
        debug,
        no_debug,
    }, "wabt-debug", "build wabt with debug on") orelse .no_debug;

    const wabt_config_h = b.addConfigHeader(.{
        .style = .{ .cmake = wabt_dep.path("src/config.h.in") },
        .include_path = "wabt/config.h",
    }, .{
        .WABT_VERSION_STRING = "1.0.34",
        .WABT_DEBUG = @as(?isize, if (wabt_debug == .debug) 1 else null),
        .HAVE_SNPRINTF = 1,
        .HAVE_SSIZE_T = 1,
        .HAVE_STRCASECMP = 1,
        .COMPILER_IS_CLANG = 1,
        .SIZEOF_SIZE_T = @sizeOf(usize),
    });

    const wabt_lib = b.addLibrary(.{
        .name = "wabt",
        .root_module = b.createModule(.{
            .target = b.graph.host,
            .optimize = .Debug,
        }),
    });
    wabt_lib.addConfigHeader(wabt_config_h);
    wabt_lib.addIncludePath(wabt_dep.path("include"));
    wabt_lib.addCSourceFiles(.{
        .root = wabt_dep.path("."),
        .files = &wabt_files,
    });
    wabt_lib.linkLibCpp();

    const wast2json = b.addExecutable(.{
        .name = "wast2json",
        .root_module = b.createModule(.{
            .target = b.graph.host,
        }),
    });
    wast2json.addConfigHeader(wabt_config_h);
    wast2json.addIncludePath(wabt_dep.path("include"));
    wast2json.addCSourceFile(.{
        .file = wabt_dep.path("src/tools/wast2json.cc"),
    });
    wast2json.linkLibCpp();
    wast2json.linkLibrary(wabt_lib);
    return wast2json;
}

const test_names = [_][]const u8{
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

const wabt_files = [_][]const u8{
    "src/binary-reader-ir.cc",
    "src/binary-reader-logging.cc",
    "src/binary-reader.cc",
    "src/binary-writer-spec.cc",
    "src/binary-writer.cc",
    "src/binary.cc",
    "src/binding-hash.cc",
    "src/color.cc",
    "src/common.cc",
    "src/error-formatter.cc",
    "src/expr-visitor.cc",
    "src/feature.cc",
    "src/filenames.cc",
    "src/ir.cc",
    "src/leb128.cc",
    "src/lexer-source-line-finder.cc",
    "src/lexer-source.cc",
    "src/literal.cc",
    "src/opcode-code-table.c",
    "src/opcode.cc",
    "src/option-parser.cc",
    "src/resolve-names.cc",
    "src/shared-validator.cc",
    "src/stream.cc",
    "src/token.cc",
    "src/type-checker.cc",
    "src/utf8.cc",
    "src/validator.cc",
    "src/wast-lexer.cc",
    "src/wast-parser.cc",
};
