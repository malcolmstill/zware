const std = @import("std");
const mem = std.mem;
const fs = std.fs;
const fmt = std.fmt;
const process = std.process;
const zware = @import("zware");
const ArrayList = std.ArrayList;
const Module = zware.Module;
const Store = zware.Store;
const Instance = zware.Instance;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
var gpa = GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    defer _ = gpa.deinit();
    var alloc = gpa.allocator();

    var args = try process.argsWithAllocator(alloc);
    defer args.deinit();
    _ = args.skip();
    const filename = args.next() orelse return error.NoFilename;

    const program = try fs.cwd().readFileAlloc(alloc, filename, 0xFFFFFFF);
    defer alloc.free(program);

    var module = Module.init(alloc, program);
    defer module.deinit();
    try module.decode();

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("const std = @import(\"std\");\n", .{});
    try stdout.print("const zware = @import(\"zware\");\n\n", .{});

    // Generate loader
    try stdout.print("pub fn initHostFunctions(store: *zware.Store) !void {{\n", .{});
    for (module.functions.list.items) |function| {
        const import_index = function.import orelse continue;

        const function_import = module.imports.list.items[import_index];

        const function_type = module.types.list.items[function.typeidx];

        if (mem.eql(u8, function_import.module, "wasi_snapshot_preview1")) {
            try stdout.print("\ttry store.exposeHostFunction(\"{s}\", \"{s}\", zware.wasi.{s}, &[_]zware.ValType{{", .{ function_import.module, function_import.name, function_import.name });
        } else {
            try stdout.print("\ttry store.exposeHostFunction(\"{s}\", \"{s}\", {s}, &[_]zware.ValType{{", .{ function_import.module, function_import.name, function_import.name });
        }
        for (function_type.params, 0..) |param, i| {
            try stdout.print(".{s}", .{@tagName(param)});

            if (i < function_type.params.len - 1) try stdout.print(", ", .{});
        }
        try stdout.print("}}, &[_]zware.ValType{{", .{});
        for (function_type.results, 0..) |result, i| {
            try stdout.print(".{s}", .{@tagName(result)});

            if (i < function_type.results.len - 1) try stdout.print(", ", .{});
        }
        try stdout.print("}});\n", .{});
    }
    try stdout.print("}}\n\n", .{});

    // Generate stubs
    for (module.functions.list.items) |function| {
        const import_index = function.import orelse continue;

        const function_import = module.imports.list.items[import_index];

        const function_type = module.types.list.items[function.typeidx];

        // Do not generate a stub for WASI functions
        if (mem.eql(u8, function_import.module, "wasi_snapshot_preview1")) continue;

        try stdout.print("pub fn {s}(vm: *zware.VirtualMachine) zware.WasmError!void {{\n", .{function_import.name});

        if (function_type.params.len == 0 and function_type.results.len == 0) {
            try stdout.print("\t_ = vm;\n", .{});
        }

        // Insert pops. Note our first argument to a function is the _last_ argument that will be popped off
        // the stack, so we pop the last argument first which is why this is working backwards through params.
        for (function_type.params, 0..) |_, i| {
            const j = function_type.params.len - 1 - i;
            const param = function_type.params[j];
            try stdout.print("\tconst param{} = vm.popOperand({s});\n", .{ j, zigType(param) });
        }

        try stdout.print("\tstd.debug.print(\"Unimplemented: {s}(", .{function_import.name});
        for (function_type.params, 0..) |_, i| {
            try stdout.print("{{}}", .{});

            if (i < function_type.params.len - 1) try stdout.print(", ", .{});
        }
        try stdout.print(")\\n\", .{{", .{});
        for (function_type.params, 0..) |_, i| {
            try stdout.print("param{}", .{i});

            if (i < function_type.params.len - 1) try stdout.print(", ", .{});
        }
        try stdout.print("}});\n", .{});

        for (function_type.results) |_| {
            try stdout.print("\ttry vm.pushOperand(u64, 0);\n", .{});
        }

        try stdout.print("\t@panic(\"Unimplemented: {s}\");\n", .{function_import.name});
        try stdout.print("}}\n\n", .{});
    }

    // Generate api
    try stdout.print("pub const Api = struct {{\n", .{});
    try stdout.print("\tinstance: *zware.Instance,\n\n", .{});
    try stdout.print("\tconst Self = @This();\n\n", .{});
    try stdout.print("\tpub fn init(instance: *zware.Instance) Self {{\n", .{});
    try stdout.print("\t\treturn .{{ .instance = instance }};\n", .{});
    try stdout.print("\t}}\n\n", .{});
    for (module.exports.list.items, 0..) |exprt, j| {
        if (exprt.tag != .Func) continue;

        const function = module.functions.list.items[exprt.index];

        const function_type = module.types.list.items[function.typeidx];

        // Emit function definition
        try stdout.print("\tpub fn {s}(self: *Self", .{exprt.name});

        // Emit function params
        for (function_type.params, 0..) |param, i| {
            try stdout.print(", param{}: {s}", .{ i, zigType(param) });
        }

        try stdout.print(") !", .{});

        if (function_type.results.len == 0) {
            try stdout.print("void", .{});
        } else if (function_type.results.len == 1) {
            try stdout.print("{s}", .{zigType(function_type.results[0])});
        } else {
            try stdout.print("struct {{", .{});

            for (function_type.results, 0..) |_, i| {
                try stdout.print("result{}: u64,\n", .{i});
            }

            try stdout.print("}}", .{});
        }

        try stdout.print(" {{\n", .{});

        // Push params into input array
        try stdout.print("\t\tvar in = [_]u64{{", .{});
        for (function_type.params, 0..) |param, i| {
            switch (param) {
                .I32 => try stdout.print("@bitCast(@as(i64, param{}))", .{i}),
                .I64 => try stdout.print("@bitCast(param{})", .{i}),
                .F32 => try stdout.print("@bitCast(@as(f64, param{}))", .{i}),
                .F64 => try stdout.print("@bitCast(param{})", .{i}),
                .V128 => try stdout.print("FIXME{}", .{i}),
                .FuncRef => try stdout.print("@bitCast(param{})", .{i}),
                .ExternRef => try stdout.print("@bitCast(param{})", .{i}),
            }

            if (i < function_type.params.len - 1) try stdout.print(", ", .{});
        }
        try stdout.print("}};\n", .{});

        try stdout.print("\t\tvar out = [_]u64{{", .{});
        for (function_type.results, 0..) |_, i| {
            try stdout.print("0", .{});
            if (i < function_type.results.len - 1) try stdout.print(", ", .{});
        }
        try stdout.print("}};\n", .{});

        try stdout.print("\t\ttry self.instance.invoke(\"{s}\", in[0..], out[0..], .{{}});\n", .{exprt.name});

        // Return results
        if (function_type.results.len == 0) {
            //
        } else if (function_type.results.len == 1) {
            try stdout.print("\t\treturn ", .{});
            const i = 0;

            switch (function_type.results[0]) {
                .I32 => try stdout.print("@bitCast(@as(u32, @truncate(out[{}])))", .{i}),
                .I64 => try stdout.print("@bitCast(out[{}])", .{i}),
                .F32 => try stdout.print("@bitCast(@as(u32, @truncate(out[{}])))", .{i}),
                .F64 => try stdout.print("@bitCast(out[{}])", .{i}),
                .V128 => try stdout.print("FIXME{}", .{i}), // FIXME
                .FuncRef => try stdout.print("@bitCast(out[{}])", .{i}), // FIXME
                .ExternRef => try stdout.print("@bitCast(out[{}])", .{i}), // FIXME
            }

            try stdout.print(";\n", .{});
        } else {
            try stdout.print("\treturn .{{", .{});

            for (function_type.results, 0..) |result, i| {
                try stdout.print(".result{} = ", .{i});

                switch (result) {
                    .I32 => try stdout.print("@bitCast(@as(u32, @truncate(out[{}])))", .{i}),
                    .I64 => try stdout.print("@bitCast(out[{}])", .{i}),
                    .F32 => try stdout.print("@bitCast(@as(u32, @truncate(out[{}])))", .{i}),
                    .F64 => try stdout.print("@bitCast(out[{}])", .{i}),
                    .V128 => try stdout.print("FIXME{}", .{i}), // FIXME
                    .FuncRef => try stdout.print("@bitCast(out[{}])", .{i}), // FIXME
                    .ExternRef => try stdout.print("@bitCast(out[{}])", .{i}), // FIXME
                }

                try stdout.print(",\n", .{});
            }

            try stdout.print("}};\n", .{});
        }

        try stdout.print("\t}}\n", .{});

        if (j < module.exports.list.items.len - 1) try stdout.print("\n", .{});
    }
    try stdout.print("}};\n\n", .{});

    try bw.flush();
}

fn zigType(v: zware.ValType) []const u8 {
    return switch (v) {
        .I32 => "i32",
        .I64 => "i64",
        .F32 => "f32",
        .F64 => "f64",
        .V128 => "u128",
        .FuncRef => "anyopaque",
        .ExternRef => "anyopaque",
    };
}
