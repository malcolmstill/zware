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

    try stdout.print("const zware = @import(\"zware\");\n\n", .{});

    // Generate loader
    try stdout.print("pub fn initHostFunctions(module: []const u8, store: *zware.Store) !void {{\n", .{});
    for (module.functions.list.items) |function| {
        const import_index = function.import orelse continue;

        const function_import = module.imports.list.items[import_index];

        // const function_type = module.types.list.items[function.typeidx];

        try stdout.print("\ttry store.addHostFunction(module, \"{s}\", {s}, [_]ValType{{}}, [_]ValType{{}});\n", .{function_import.name, function_import.name});
    }
    try stdout.print("}}\n\n", .{});

    // Generate stubs
    for (module.functions.list.items) |function| {
        const import_index = function.import orelse continue;

        const function_import = module.imports.list.items[import_index];

        try stdout.print("pub fn {s}(vm: *VirtualMachine) WasmError!void {{\n", .{function_import.name});
        try stdout.print("\t@panic(\"Unimplemented: {s}\");\n", .{function_import.name});
        try stdout.print("}}\n\n", .{});
    }

    try bw.flush(); // don't forget to flush!
}
