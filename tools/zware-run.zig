const std = @import("std");
const zware = @import("zware");
const wasi = @import("wasi");

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

const Config = struct {
    wasm_path: []const u8,
    function_name: ?[]const u8 = null,  // null means auto-detect
    wasm_args: []const []const u8,
    env_vars: std.StringHashMap([]const u8),
    dir_mappings: std.StringHashMap([]const u8),

    fn deinit(self: *Config) void {
        self.env_vars.deinit();
        self.dir_mappings.deinit();
    }
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
fn parseArgs(args: []const []const u8) !Config {
    var env_vars = std.StringHashMap([]const u8).init(global.alloc);
    errdefer env_vars.deinit();

    var dir_mappings = std.StringHashMap([]const u8).init(global.alloc);
    errdefer dir_mappings.deinit();

    var wasm_args: std.ArrayList([]const u8) = .empty;
    defer wasm_args.deinit(global.alloc);

    var wasm_path: ?[]const u8 = null;
    var function_name: ?[]const u8 = null;
    var i: usize = 0;

    while (i < args.len) : (i += 1) {
        const arg = args[i];

        if (std.mem.eql(u8, arg, "--env")) {
            i += 1;
            if (i >= args.len) {
                std.log.err("--env requires KEY=VALUE argument", .{});
                std.process.exit(0xff);
            }
            const env_spec = args[i];
            const eq_pos = std.mem.indexOf(u8, env_spec, "=") orelse {
                std.log.err("invalid --env format, expected KEY=VALUE", .{});
                std.process.exit(0xff);
            };
            try env_vars.put(env_spec[0..eq_pos], env_spec[eq_pos + 1..]);
        } else if (std.mem.eql(u8, arg, "--dir")) {
            i += 1;
            if (i >= args.len) {
                std.log.err("--dir requires GUEST::HOST argument", .{});
                std.process.exit(0xff);
            }
            const dir_spec = args[i];
            const sep_pos = std.mem.indexOf(u8, dir_spec, "::") orelse {
                std.log.err("invalid --dir format, expected GUEST::HOST", .{});
                std.process.exit(0xff);
            };
            try dir_mappings.put(dir_spec[0..sep_pos], dir_spec[sep_pos + 2..]);
        } else if (std.mem.eql(u8, arg, "-f") or std.mem.eql(u8, arg, "--function")) {
            i += 1;
            if (i >= args.len) {
                std.log.err("-f/--function requires function name argument", .{});
                std.process.exit(0xff);
            }
            function_name = args[i];
        } else if (std.mem.eql(u8, arg, "--version")) {
            std.debug.print("zware-run 0.0.1\n", .{});
            std.process.exit(0);
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            printUsage();
            std.process.exit(0);
        } else if (wasm_path == null) {
            wasm_path = arg;
        } else {
            // All arguments after wasm_path are program arguments
            try wasm_args.append(global.alloc, arg);
        }
    }

    if (wasm_path == null) {
        printUsage();
        std.process.exit(0xff);
    }

    return Config{
        .wasm_path = wasm_path.?,
        .function_name = function_name,
        .wasm_args = try wasm_args.toOwnedSlice(global.alloc),
        .env_vars = env_vars,
        .dir_mappings = dir_mappings,
    };
}

fn printUsage() void {
    const stderr_fd = std.fs.File.stderr();
    var stderr_buf: [4096]u8 = undefined;
    var stderr_writer = stderr_fd.writer(&stderr_buf);
    const stderr = &stderr_writer.interface;
    stderr.writeAll(
        \\Usage: zware-run [OPTIONS] FILE.wasm [ARGS...]
        \\
        \\Options:
        \\  -f, --function NAME   Specify function to call (default: _start)
        \\  --env KEY=VALUE       Set environment variable for WASI
        \\  --dir GUEST::HOST     Map directory for WASI preopens
        \\  --version             Show version
        \\  --help, -h            Show this help
        \\
        \\If no function is specified with -f, attempts to call _start (WASI entry point).
        \\All arguments after FILE.wasm are passed to the WASM program.
        \\WASI imports are always available regardless of which function is called.
        \\
        \\Examples:
        \\  zware-run -f fib fib.wasm             # Call fib() function
        \\  zware-run program.wasm arg1 arg2      # Call _start with arguments
        \\  zware-run --env FOO=bar program.wasm  # Call _start with env var
        \\  zware-run --dir /::. program.wasm arg1 arg2   # Call _start with dir mapping and args
        \\
    ) catch {};
    stderr.flush() catch {};
}

// WASI wrapper functions - these adapt WASI functions to the host function signature
fn wasi_args_get(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.args_get(vm);
}
fn wasi_args_sizes_get(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.args_sizes_get(vm);
}
fn wasi_environ_get(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.environ_get(vm);
}
fn wasi_environ_sizes_get(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.environ_sizes_get(vm);
}
fn wasi_clock_time_get(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.clock_time_get(vm);
}
fn wasi_fd_close(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.fd_close(vm);
}
fn wasi_fd_fdstat_get(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.fd_fdstat_get(vm);
}
fn wasi_fd_fdstat_set_flags(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.fd_fdstat_set_flags(vm);
}
fn wasi_fd_filestat_get(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.fd_filestat_get(vm);
}
fn wasi_fd_prestat_get(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.fd_prestat_get(vm);
}
fn wasi_fd_prestat_dir_name(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.fd_prestat_dir_name(vm);
}
fn wasi_fd_read(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.fd_read(vm);
}
fn wasi_fd_readdir(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.fd_readdir(vm);
}
fn wasi_fd_seek(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.fd_seek(vm);
}
fn wasi_fd_tell(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.fd_tell(vm);
}
fn wasi_fd_write(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.fd_write(vm);
}
fn wasi_path_create_directory(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.path_create_directory(vm);
}
fn wasi_path_filestat_get(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.path_filestat_get(vm);
}
fn wasi_path_open(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.path_open(vm);
}
fn wasi_path_readlink(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.path_readlink(vm);
}
fn wasi_poll_oneoff(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.poll_oneoff(vm);
}
fn wasi_proc_exit(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.proc_exit(vm);
}
fn wasi_random_get(vm: *zware.VirtualMachine, _: usize) zware.WasmError!void {
    return zware.wasi.random_get(vm);
}

// Expose all WASI imports to the store
fn setupWasiImports(store: *zware.Store) !void {
    const wasi_module = "wasi_snapshot_preview1";

    // args_get(argv: **u8, argv_buf: *u8) -> errno
    try store.exposeHostFunction(wasi_module, "args_get", wasi_args_get, 0,
        &[_]zware.ValType{.I32, .I32}, &[_]zware.ValType{.I32});

    // args_sizes_get(argc: *u32, argv_buf_size: *u32) -> errno
    try store.exposeHostFunction(wasi_module, "args_sizes_get", wasi_args_sizes_get, 0,
        &[_]zware.ValType{.I32, .I32}, &[_]zware.ValType{.I32});

    // environ_get(environ: **u8, environ_buf: *u8) -> errno
    try store.exposeHostFunction(wasi_module, "environ_get", wasi_environ_get, 0,
        &[_]zware.ValType{.I32, .I32}, &[_]zware.ValType{.I32});

    // environ_sizes_get(environc: *u32, environ_buf_size: *u32) -> errno
    try store.exposeHostFunction(wasi_module, "environ_sizes_get", wasi_environ_sizes_get, 0,
        &[_]zware.ValType{.I32, .I32}, &[_]zware.ValType{.I32});

    // clock_time_get(id: u32, precision: u64, timestamp: *u64) -> errno
    try store.exposeHostFunction(wasi_module, "clock_time_get", wasi_clock_time_get, 0,
        &[_]zware.ValType{.I32, .I64, .I32}, &[_]zware.ValType{.I32});

    // fd_close(fd: i32) -> errno
    try store.exposeHostFunction(wasi_module, "fd_close", wasi_fd_close, 0,
        &[_]zware.ValType{.I32}, &[_]zware.ValType{.I32});

    // fd_fdstat_get(fd: i32, stat: *fdstat) -> errno
    try store.exposeHostFunction(wasi_module, "fd_fdstat_get", wasi_fd_fdstat_get, 0,
        &[_]zware.ValType{.I32, .I32}, &[_]zware.ValType{.I32});

    // fd_fdstat_set_flags(fd: i32, flags: u16) -> errno
    try store.exposeHostFunction(wasi_module, "fd_fdstat_set_flags", wasi_fd_fdstat_set_flags, 0,
        &[_]zware.ValType{.I32, .I32}, &[_]zware.ValType{.I32});

    // fd_filestat_get(fd: i32, stat: *filestat) -> errno
    try store.exposeHostFunction(wasi_module, "fd_filestat_get", wasi_fd_filestat_get, 0,
        &[_]zware.ValType{.I32, .I32}, &[_]zware.ValType{.I32});

    // fd_prestat_get(fd: i32, prestat: *prestat) -> errno
    try store.exposeHostFunction(wasi_module, "fd_prestat_get", wasi_fd_prestat_get, 0,
        &[_]zware.ValType{.I32, .I32}, &[_]zware.ValType{.I32});

    // fd_prestat_dir_name(fd: i32, path: *u8, path_len: u32) -> errno
    try store.exposeHostFunction(wasi_module, "fd_prestat_dir_name", wasi_fd_prestat_dir_name, 0,
        &[_]zware.ValType{.I32, .I32, .I32}, &[_]zware.ValType{.I32});

    // fd_read(fd: i32, iovs: *const iovec, iovs_len: u32, nread: *u32) -> errno
    try store.exposeHostFunction(wasi_module, "fd_read", wasi_fd_read, 0,
        &[_]zware.ValType{.I32, .I32, .I32, .I32}, &[_]zware.ValType{.I32});

    // fd_readdir(fd: i32, buf: *u8, buf_len: u32, cookie: u64, bufused: *u32) -> errno
    try store.exposeHostFunction(wasi_module, "fd_readdir", wasi_fd_readdir, 0,
        &[_]zware.ValType{.I32, .I32, .I32, .I64, .I32}, &[_]zware.ValType{.I32});

    // fd_seek(fd: i32, offset: i64, whence: u8, newoffset: *u64) -> errno
    try store.exposeHostFunction(wasi_module, "fd_seek", wasi_fd_seek, 0,
        &[_]zware.ValType{.I32, .I64, .I32, .I32}, &[_]zware.ValType{.I32});

    // fd_tell(fd: i32, offset: *u64) -> errno
    try store.exposeHostFunction(wasi_module, "fd_tell", wasi_fd_tell, 0,
        &[_]zware.ValType{.I32, .I32}, &[_]zware.ValType{.I32});

    // fd_write(fd: i32, iovs: *const ciovec, iovs_len: u32, nwritten: *u32) -> errno
    try store.exposeHostFunction(wasi_module, "fd_write", wasi_fd_write, 0,
        &[_]zware.ValType{.I32, .I32, .I32, .I32}, &[_]zware.ValType{.I32});

    // path_create_directory(fd: i32, path: *const u8, path_len: u32) -> errno
    try store.exposeHostFunction(wasi_module, "path_create_directory", wasi_path_create_directory, 0,
        &[_]zware.ValType{.I32, .I32, .I32}, &[_]zware.ValType{.I32});

    // path_filestat_get(fd: i32, flags: u32, path: *const u8, path_len: u32, buf: *filestat) -> errno
    try store.exposeHostFunction(wasi_module, "path_filestat_get", wasi_path_filestat_get, 0,
        &[_]zware.ValType{.I32, .I32, .I32, .I32, .I32}, &[_]zware.ValType{.I32});

    // path_open(fd: i32, dirflags: u32, path: *const u8, path_len: u32, oflags: u32, fs_rights_base: u64, fs_rights_inheriting: u64, fdflags: u32, opened_fd: *i32) -> errno
    try store.exposeHostFunction(wasi_module, "path_open", wasi_path_open, 0,
        &[_]zware.ValType{.I32, .I32, .I32, .I32, .I32, .I64, .I64, .I32, .I32}, &[_]zware.ValType{.I32});

    // path_readlink(fd: i32, path: *const u8, path_len: u32, buf: *u8, buf_len: u32, bufused: *u32) -> errno
    try store.exposeHostFunction(wasi_module, "path_readlink", wasi_path_readlink, 0,
        &[_]zware.ValType{.I32, .I32, .I32, .I32, .I32, .I32}, &[_]zware.ValType{.I32});

    // poll_oneoff(in: *const subscription, out: *event, nsubscriptions: u32, nevents: *u32) -> errno
    try store.exposeHostFunction(wasi_module, "poll_oneoff", wasi_poll_oneoff, 0,
        &[_]zware.ValType{.I32, .I32, .I32, .I32}, &[_]zware.ValType{.I32});

    // proc_exit(rval: i32) -> !
    try store.exposeHostFunction(wasi_module, "proc_exit", wasi_proc_exit, 0,
        &[_]zware.ValType{.I32}, &[_]zware.ValType{});

    // random_get(buf: *u8, buf_len: u32) -> errno
    try store.exposeHostFunction(wasi_module, "random_get", wasi_random_get, 0,
        &[_]zware.ValType{.I32, .I32}, &[_]zware.ValType{.I32});
}

fn main2() !void {
    defer global.import_stubs.deinit(global.alloc);

    const full_cmdline = try std.process.argsAlloc(global.alloc);
    defer std.process.argsFree(global.alloc, full_cmdline);

    var config = try parseArgs(full_cmdline[1..]);
    defer config.deinit();

    const wasm_path = config.wasm_path;

    var store = zware.Store.init(global.alloc);
    defer store.deinit();

    // Setup WASI imports (always available)
    try setupWasiImports(&store);

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

    // Determine which function to call
    const func_name = config.function_name orelse "_start";
    const export_funcidx = try getExportFunction(&module, func_name);
    const export_funcdef = module.functions.list.items[export_funcidx];
    const export_functype = try module.types.lookup(export_funcdef.typeidx);
    if (export_functype.params.len != 0) {
        std.log.err("calling a function with parameters is not implemented", .{});
        std.process.exit(0xff);
    }

    var instance = zware.Instance.init(global.alloc, &store, module);
    defer if (enable_leak_detection) instance.deinit();

    // Setup WASI arguments
    const wasi_args = try global.alloc.alloc([:0]u8, config.wasm_args.len + 1);
    defer global.alloc.free(wasi_args);

    // First arg is the program name (wasm path)
    wasi_args[0] = try global.alloc.dupeZ(u8, wasm_path);
    for (config.wasm_args, 0..) |arg, i| {
        wasi_args[i + 1] = try global.alloc.dupeZ(u8, arg);
    }

    for (wasi_args) |arg| {
        try instance.wasi_args.append(global.alloc, arg);
    }

    // Setup WASI environment variables
    var env_iter = config.env_vars.iterator();
    while (env_iter.next()) |entry| {
        try instance.wasi_env.put(global.alloc, entry.key_ptr.*, entry.value_ptr.*);
    }

    // Setup WASI preopens (directory mappings)
    // Start with stdin (0), stdout (1), stderr (2) - these are standard
    // Custom directories start at fd 3
    var preopen_fd: i32 = 3;
    var dir_iter = config.dir_mappings.iterator();
    while (dir_iter.next()) |entry| {
        const guest_path = entry.key_ptr.*;
        const host_path = entry.value_ptr.*;

        const dir = std.fs.cwd().openDir(host_path, .{}) catch |e| {
            std.log.err("failed to open directory '{s}': {s}", .{ host_path, @errorName(e) });
            std.process.exit(0xff);
        };

        try instance.addWasiPreopen(preopen_fd, guest_path, dir.fd);
        preopen_fd += 1;
    }

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
    try instance.invoke(func_name, &in, out_args, .{});
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
