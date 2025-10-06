const std = @import("std");
const zware = @import("zware");

fn oom(e: error{OutOfMemory}) noreturn {
    @panic(@errorName(e));
}

const ImportStub = struct {
    module: []const u8,
    name: []const u8,
    type: zware.FuncType,
};

const enable_leak_detection = false;
const global = struct {
    var allocator_instance = if (enable_leak_detection) std.heap.GeneralPurposeAllocator(.{
        .retain_metadata = true,
        //.verbose_log = true,
    }){} else std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const alloc = allocator_instance.allocator();
    var import_stubs: std.ArrayListUnmanaged(ImportStub) = .{};
};

pub fn main() !void {
    try main2();
    if (enable_leak_detection) {
        switch (global.allocator_instance.deinit()) {
            .ok => {},
            .leak => @panic("memory leak"),
        }
    }
}
fn main2() !void {
    defer global.import_stubs.deinit(global.alloc);

    const full_cmdline = try std.process.argsAlloc(global.alloc);
    defer std.process.argsFree(global.alloc, full_cmdline);

    if (full_cmdline.len <= 1) {
        const stderr_fd = std.fs.File.stderr();
        var stderr_buf: [4096]u8 = undefined;
        var stderr_writer = stderr_fd.writer(&stderr_buf);
        const stderr = &stderr_writer.interface;
        try stderr.writeAll("Usage: zware-run FILE.wasm FUNCTION\n");
        try stderr.flush();
        std.process.exit(0xff);
    }

    const pos_args = full_cmdline[1..];
    if (pos_args.len != 2) {
        std.log.err("expected {} positional cmdline arguments but got {}", .{ 2, pos_args.len });
        std.process.exit(0xff);
    }
    const wasm_path = pos_args[0];
    const wasm_func_name = pos_args[1];

    var store = zware.Store.init(global.alloc);
    defer store.deinit();

    const wasm_content = content_blk: {
        var file = std.fs.cwd().openFile(wasm_path, .{}) catch |e| {
            std.log.err("failed to open '{s}': {s}", .{ wasm_path, @errorName(e) });
            std.process.exit(0xff);
        };
        defer file.close();
        break :content_blk try file.readToEndAlloc(global.alloc, std.math.maxInt(usize));
    };
    defer global.alloc.free(wasm_content);

    var module = zware.Module.init(global.alloc, wasm_content);
    defer module.deinit();
    try module.decode();

    const export_funcidx = try getExportFunction(&module, wasm_func_name);
    const export_funcdef = module.functions.list.items[export_funcidx];
    const export_functype = try module.types.lookup(export_funcdef.typeidx);
    if (export_functype.params.len != 0) {
        std.log.err("calling a function with parameters is not implemented", .{});
        std.process.exit(0xff);
    }

    var instance = zware.Instance.init(global.alloc, &store, module);
    defer if (enable_leak_detection) instance.deinit();

    try populateMissingImports(&store, &module);

    var zware_error: zware.Error = undefined;
    instance.instantiateWithError(&zware_error) catch |err| switch (err) {
        error.SeeContext => {
            std.log.err("failed to instantiate the module: {f}", .{zware_error});
            std.process.exit(0xff);
        },
        else => |e| return e,
    };
    defer instance.deinit();

    var in = [_]u64{};
    const out_args = try global.alloc.alloc(u64, export_functype.results.len);
    defer global.alloc.free(out_args);
    try instance.invoke(wasm_func_name, &in, out_args, .{});
    std.log.info("{} output(s)", .{out_args.len});
    for (out_args, 0..) |out_arg, out_index| {
        std.log.info("output {} {f}", .{ out_index, fmtValue(export_functype.results[out_index], out_arg) });
    }
}

fn getExportFunction(module: *const zware.Module, func_name: []const u8) !usize {
    return module.getExport(.Func, func_name) catch |err| switch (err) {
        error.ExportNotFound => {
            const stderr_fd = std.fs.File.stderr();
            var stderr_buf: [4096]u8 = undefined;
            var stderr_writer = stderr_fd.writer(&stderr_buf);
            const stderr = &stderr_writer.interface;
            var export_func_count: usize = 0;
            for (module.exports.list.items) |exp| {
                if (exp.tag == .Func) {
                    export_func_count += 1;
                }
            }
            if (export_func_count == 0) {
                try stderr.print("error: this wasm binary has no function exports\n", .{});
            } else {
                try stderr.print(
                    "error: no export function named '{s}', pick from one of the following {} export(s):\n",
                    .{ func_name, export_func_count },
                );
                for (module.exports.list.items) |exp| {
                    if (exp.tag == .Func) {
                        try stderr.print("     {s}\n", .{exp.name});
                    }
                }
            }
            try stderr.flush();
            std.process.exit(0xff);
        },
    };
}

fn populateMissingImports(store: *zware.Store, module: *const zware.Module) !void {
    var import_funcidx: u32 = 0;
    var import_memidx: u32 = 0;
    for (module.imports.list.items, 0..) |import, import_index| {
        defer switch (import.desc_tag) {
            .Func => import_funcidx += 1,
            .Mem => import_memidx += 1,
            else => @panic("todo"),
        };

        if (store.import(import.module, import.name, import.desc_tag)) |_| {
            continue;
        } else |err| switch (err) {
            error.ImportNotFound => {},
        }

        switch (import.desc_tag) {
            .Func => {
                const funcdef = module.functions.list.items[import_funcidx];
                std.debug.assert(funcdef.import.? == import_index);
                const functype = try module.types.lookup(funcdef.typeidx);
                global.import_stubs.append(global.alloc, .{
                    .module = import.module,
                    .name = import.name,
                    .type = functype,
                }) catch |e| oom(e);
                store.exposeHostFunction(
                    import.module,
                    import.name,
                    onMissingImport,
                    global.import_stubs.items.len - 1,
                    functype.params,
                    functype.results,
                ) catch |e2| oom(e2);
            },
            .Mem => {
                const memdef = module.memories.list.items[import_memidx];
                std.debug.assert(memdef.import.? == import_index);
                try store.exposeMemory(import.module, import.name, memdef.limits.min, memdef.limits.max);
            },
            else => |tag| std.debug.panic("todo: handle import {s}", .{@tagName(tag)}),
        }
    }
}

fn onMissingImport(vm: *zware.VirtualMachine, context: usize) zware.WasmError!void {
    const stub = global.import_stubs.items[context];
    std.log.info("import function '{s}.{s}' called", .{ stub.module, stub.name });
    for (stub.type.params, 0..) |param_type, i| {
        const value = vm.popAnyOperand();
        std.log.info("    param {} {f}", .{ i, fmtValue(param_type, value) });
    }
    for (stub.type.results, 0..) |result_type, i| {
        std.log.info("    result {} {f}", .{ i, fmtValue(result_type, 0) });
        try vm.pushOperand(u64, 0);
    }
}

pub fn Native(comptime self: zware.ValType) type {
    return switch (self) {
        .I32 => i32,
        .I64 => i64,
        .F32 => f32,
        .F64 => f64,
        .V128 => u64,
        .FuncRef => u64,
        .ExternRef => u64,
    };
}

fn cast(comptime val_type: zware.ValType, value: u64) Native(val_type) {
    return switch (val_type) {
        .I32 => @bitCast(@as(u32, @intCast(value))),
        .I64 => @bitCast(value),
        .F32 => @bitCast(@as(u32, @intCast(value))),
        .F64 => @bitCast(value),
        .V128 => value,
        .FuncRef => value,
        .ExternRef => value,
    };
}

fn fmtValue(val_type: zware.ValType, value: u64) FmtValue {
    return .{ .type = val_type, .value = value };
}
const FmtValue = struct {
    type: zware.ValType,
    value: u64,
    pub fn format(
        self: FmtValue,
        writer: anytype,
    ) !void {
        switch (self.type) {
            inline else => |t2| try writer.print("({s}) {}", .{ @tagName(t2), cast(t2, self.value) }),
        }
    }
};
