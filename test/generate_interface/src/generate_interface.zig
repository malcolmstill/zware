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
    var args = process.args();
    _ = args.skip();
    const filename = args.next() orelse return error.NoFilename;

    defer _ = gpa.deinit();
    var alloc = gpa.allocator();

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

        try stdout.print("\ttry store.addHostFunction(\"{s}\", \"{s}\", {s}, &[_]zware.ValType{{", .{function_import.module, function_import.name, function_import.name});
        for (function_type.params) |param| {
            try stdout.print(".{s}, ", .{@tagName(param)});
        }
        try stdout.print("}}, &[_]zware.ValType{{", .{});
        for (function_type.results) |result| {
            try stdout.print(".{s}, ", .{@tagName(result)});
        }        
        try stdout.print("}});\n", .{});
    }
    try stdout.print("}}\n\n", .{});

    // Generate stubs
    for (module.functions.list.items) |function| {
        const import_index = function.import orelse continue;

        const function_import = module.imports.list.items[import_index];

        const function_type = module.types.list.items[function.typeidx];

        try stdout.print("pub fn {s}(vm: *zware.VirtualMachine) zware.WasmError!void {{\n", .{function_import.name});

        if (function_type.params.len == 0) {
            try stdout.print("\t_ = vm;\n", .{});
        }

        for (function_type.params, 0..) |param, i| {
            const zig_type = switch (param) {
                .I32 => "i32",
                .I64 => "i64",
                .F32 => "f32",
                .F64 => "f64",
                .V128 => "v128", // FIXME
                .FuncRef => "funcref", // FIXME
                .ExternRef => "externref", // FIXME
            };

            try stdout.print("\tconst param{} = vm.popOperand({s});\n", .{i, zig_type});
        }

        try stdout.print("\tstd.debug.print(\"Unimplemented: {s}(", .{function_import.name});
        for (function_type.params) |_| {
            try stdout.print("{{}}, ", .{});
        }
        try stdout.print(")\\n\", .{{", .{});
        for (function_type.params, 0..) |_, i| {
            try stdout.print("param{}, ", .{i});
        }
        try stdout.print("}});\n", .{});
 
        try stdout.print("\t@panic(\"Unimplemented: {s}\");\n", .{function_import.name});
        try stdout.print("}}\n\n", .{});
    }

    // Generate stubs
    for (module.exports.list.items) |exprt| {
        if (exprt.tag != .Func) continue;

        const function = module.functions.list.items[exprt.index];

        const function_type = module.types.list.items[function.typeidx];

        // Emit function definition
        try stdout.print("pub fn {s}(instance: *zware.Instance", .{exprt.name}); 

        // Emit function params
        for (function_type.params, 0..) |_, i| {
            try stdout.print(", param{}: u64", .{i});
        }

        try stdout.print(") zware.WasmError!", .{});

        if (function_type.results.len == 0) {
            try stdout.print("void", .{});
        } else if (function_type.results.len == 1) {
            try stdout.print("u64", .{});
        } else {
            try stdout.print("struct {{", .{});

            for (function_type.results, 0..) |_, i| {
                try stdout.print("result{}: u64,\n", .{i});
            }

            try stdout.print("}}", .{});
        }

        try stdout.print(" {{\n", .{});

        // Push params into input array
        try stdout.print("\tvar in = [_]u64{{", .{});
        for (function_type.params, 0..) |_, i| {
            try stdout.print("param{}", .{i});
            if (i < function_type.params.len - 1) try stdout.print(", ", .{});
        }
        try stdout.print("}};\n", .{});


        try stdout.print("\tvar out = [_]u64{{", .{});
        for (function_type.results, 0..) |_, i| {
            try stdout.print("0", .{});
            if (i < function_type.results.len - 1) try stdout.print(", ", .{});
        }        
        try stdout.print("}};\n", .{});

        try stdout.print("\ttry instance.invoke(\"{s}\", in[0..], out[0..], .{{}});\n", .{exprt.name});

        // Return results
        if (function_type.results.len == 0) {
            //
        } else if (function_type.results.len == 1) {
            try stdout.print("\treturn out[0];\n", .{});
        } else {
            try stdout.print("\treturn .{{", .{});

            for (function_type.results, 0..) |_, i| {
                try stdout.print(".result{} = out[{}],\n", .{i, i});
            }

            try stdout.print("}};\n", .{});
        }

        try stdout.print("}}\n\n", .{});
    }

    try bw.flush();
}
