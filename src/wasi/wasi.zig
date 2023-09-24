const std = @import("std");
const mem = std.mem;
const fs = std.fs;
const os = std.os;
const math = std.math;
const wasi = std.os.wasi;

const VirtualMachine = @import("../instance/vm.zig").VirtualMachine;
const WasmError = @import("../instance/vm.zig").WasmError;

// I've largely used the implementations from https://github.com/andrewrk/zig-wasi (shoutout to Andrew and Jacob!).
//
// Thought: could we move the wasi implementations into the zig standard library, or are they too specific to a particular
// WebAssembly implementation? E.g. some of the wasi functions only seem to depend on a slice of memory + the zig std lib
// functions.

pub fn args_get(vm: *VirtualMachine) WasmError!void {
    const argv_buf_ptr = vm.popOperand(u32);
    const argv_ptr = vm.popOperand(u32);

    const memory = try vm.inst.getMemory(0);
    const data = memory.memory();

    var argv_buf_i: usize = 0;
    for (vm.wasi_args.items, 0..) |arg, i| {
        const argv_i_ptr = argv_buf_ptr + argv_buf_i;
        const arg_len = arg.len + 1;

        mem.copy(u8, data[argv_i_ptr .. argv_i_ptr + arg_len], arg[0..arg_len]);
        argv_buf_i += arg_len;

        try memory.write(u32, argv_ptr, 4 * @as(u32, @intCast(i)), @as(u32, @intCast(argv_i_ptr)));
    }

    try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
}

pub fn args_sizes_get(vm: *VirtualMachine) WasmError!void {
    const argv_buf_size_ptr = vm.popOperand(u32);
    const argc_ptr = vm.popOperand(u32);

    const memory = try vm.inst.getMemory(0);

    const argc = vm.wasi_args.items.len;
    try memory.write(u32, argc_ptr, 0, @as(u32, @intCast(argc)));

    var buf_size: usize = 0;
    for (vm.wasi_args.items) |arg| {
        buf_size += arg.len + 1;
    }
    try memory.write(u32, argv_buf_size_ptr, 0, @as(u32, @intCast(buf_size)));

    try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
}

pub fn clock_time_get(vm: *VirtualMachine) WasmError!void {
    const timestamp_ptr = vm.popOperand(u32);
    const precision = vm.popOperand(i64); // FIXME: we should probably be using this
    _ = precision;
    const clock_id = vm.popOperand(i32); // FIXME: we should probably be using this
    _ = clock_id;

    const memory = try vm.inst.getMemory(0);

    const timestamp = toWasiTimestamp(std.time.nanoTimestamp());

    try memory.write(u64, timestamp_ptr, 0, timestamp);

    try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
}

pub fn fd_close(vm: *VirtualMachine) WasmError!void {
    const fd = vm.popOperand(i32);

    const host_fd = vm.getHostFd(fd);
    os.close(host_fd);

    try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
}

pub fn fd_fdstat_get(vm: *VirtualMachine) WasmError!void {
    const stat_ptr = vm.popOperand(u32);
    const fd = vm.popOperand(i32);

    const memory = try vm.inst.getMemory(0);

    const host_fd = vm.getHostFd(fd);
    const file = fs.File{ .handle = host_fd };
    const stat = file.stat() catch |err| {
        try vm.pushOperand(u64, @intFromEnum(toWasiError(err)));
        return;
    };

    try memory.write(u16, stat_ptr, 0x00, @intFromEnum(toWasiFileType(stat.kind)));
    try memory.write(u16, stat_ptr, 0x02, 0);
    try memory.write(u64, stat_ptr, 0x08, math.maxInt(u64));
    try memory.write(u64, stat_ptr, 0x10, math.maxInt(u64));

    try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
}

// FIXME: implement
pub fn fd_fdstat_set_flags(vm: *VirtualMachine) WasmError!void {
    const param0 = vm.popOperand(i32);
    const param1 = vm.popOperand(i32);
    std.debug.print("Unimplemented: fd_fdstat_set_flags({}, {})\n", .{ param0, param1 });
    try vm.pushOperand(u64, 0);
    @panic("Unimplemented: fd_fdstat_set_flags");
}

pub fn fd_filestat_get(vm: *VirtualMachine) WasmError!void {
    const stat_ptr = vm.popOperand(u32);
    const fd = vm.popOperand(i32);

    const memory = try vm.inst.getMemory(0);

    const host_fd = vm.getHostFd(fd);
    const file = std.fs.File{ .handle = host_fd };
    const stat = file.stat() catch |err|
    {
        try vm.pushOperand(u64, @intFromEnum(toWasiError(err)));
        return;
    };

    try memory.write(u64, stat_ptr, 0, 0); // device id
    try memory.write(u64, stat_ptr, 8, stat.inode); // inode
    try memory.write(u64, stat_ptr, 16, @intFromEnum(toWasiFileType(stat.kind))); // filetype
    try memory.write(u64, stat_ptr, 24, 1); // nlink - hard links refering to this file count
    try memory.write(u64, stat_ptr, 32, stat.size); // size in bytes
    try memory.write(u64, stat_ptr, 40, @as(u64, @intCast(stat.atime))); // atime - last access time
    try memory.write(u64, stat_ptr, 48, @as(u64, @intCast(stat.mtime))); // mtime - last modified time
    try memory.write(u64, stat_ptr, 56, @as(u64, @intCast(stat.ctime))); // ctime - last status change time

    try vm.pushOperand(u64, @intFromEnum(std.os.wasi.errno_t.SUCCESS));
}

pub fn fd_prestat_get(vm: *VirtualMachine) WasmError!void {
    const prestat_ptr = vm.popOperand(u32);
    const fd = vm.popOperand(i32);

    const memory = try vm.inst.getMemory(0);

    if (vm.lookupWasiPreopen(fd)) |preopen| {
        const some_other_ptr = try memory.read(u32, prestat_ptr, 0);
        const name_len_ptr = try memory.read(u32, prestat_ptr, 4);
        try memory.write(u32, some_other_ptr, 0, 0);
        try memory.write(u32, name_len_ptr, 0, @as(u32, @intCast(preopen.name.len)));

        try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
    } else {
        try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.BADF));
    }
}

pub fn fd_prestat_dir_name(vm: *VirtualMachine) WasmError!void {
    const path_len = vm.popOperand(u32); // FIXME: we should probably be using this
    _ = path_len;
    const path_ptr = vm.popOperand(u32);
    const fd = vm.popOperand(i32);

    const memory = try vm.inst.getMemory(0);

    const preopen = vm.lookupWasiPreopen(fd) orelse return WasmError.Trap;
    try memory.copy(path_ptr, preopen.name);

    try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
}

pub fn fd_read(vm: *VirtualMachine) WasmError!void {
    const n_read_ptr = vm.popOperand(u32);
    const iovs_len = vm.popOperand(u32);
    const iovs_ptr = vm.popOperand(u32);
    const fd = vm.popOperand(i32);

    const memory = try vm.inst.getMemory(0);
    const data = memory.memory();

    const host_fd = vm.getHostFd(fd);

    var i: u32 = 0;
    var total_read: usize = 0;
    while (i < iovs_len) : (i += 1) {
        const offset: u32 = i * 8; // Each iov is 8 bytes...
        const iov_i_ptr = try memory.read(u32, iovs_ptr, offset); // 4 bytes (u32) for the ith ptr of where to read into
        const iov_i_len = try memory.read(u32, iovs_ptr, offset + 4); // 4 bytes (u32) for the length of data to read

        const buf = data[iov_i_ptr .. iov_i_ptr + iov_i_len];

        // read data from fd into buffer defined by iov
        const read = os.read(host_fd, buf) catch |err| {
            try vm.pushOperand(u64, @intFromEnum(toWasiError(err)));
            return;
        };

        total_read += read;
        if (read != buf.len) break;
    }

    try memory.write(u32, n_read_ptr, 0, @as(u32, @intCast(total_read)));

    try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
}

pub fn fd_seek(vm: *VirtualMachine) WasmError!void {
    const new_offset_ptr = vm.popOperand(u32);
    const relative_to: wasi.whence_t = @enumFromInt(vm.popOperand(i32));
    const offset = vm.popOperand(i64);
    const fd = vm.popOperand(i32);

    switch (relative_to) {
        wasi.whence_t.CUR => {
            os.lseek_CUR(fd, offset) catch |err| {
                try vm.pushOperand(u64, @intFromEnum(toWasiError(err)));
                return;
            };
        },
        wasi.whence_t.END => {
            os.lseek_END(fd, offset) catch |err| {
                try vm.pushOperand(u64, @intFromEnum(toWasiError(err)));
                return;
            };
        },
        wasi.whence_t.SET => {
            os.lseek_SET(fd, @intCast(offset)) catch |err| {
                try vm.pushOperand(u64, @intFromEnum(toWasiError(err)));
                return;
            };
        },
    }

    const new_offset = os.lseek_CUR_get(fd) catch |err| {
        try vm.pushOperand(u64, @intFromEnum(toWasiError(err)));
        return;
    };

    const memory = try vm.inst.getMemory(0);
    try memory.write(u64, new_offset_ptr, 0, new_offset);

    try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
}

pub fn fd_write(vm: *VirtualMachine) WasmError!void {
    const ret_ptr = vm.popOperand(u32);
    const iovs_len = vm.popOperand(u32);
    const iovs_ptr = vm.popOperand(u32);
    const fd = vm.popOperand(i32);

    const memory = try vm.inst.getMemory(0);
    const data = memory.memory();

    const host_fd = vm.getHostFd(fd);

    var n: usize = 0;
    var i: u32 = 0;
    while (i < iovs_len) : (i += 1) {
        const offset: u32 = i * 8;
        const iov_i_ptr = try memory.read(u32, iovs_ptr, offset);
        const iov_i_len = try memory.read(u32, iovs_ptr, offset + 4);

        const bytes = data[iov_i_ptr .. iov_i_ptr + iov_i_len];

        const written = os.write(host_fd, bytes) catch |err| {
            try vm.pushOperand(u64, @intFromEnum(toWasiError(err)));
            return;
        };

        n += written;

        if (written != bytes.len) break;
    }

    try memory.write(u32, ret_ptr, 0, @as(u32, @intCast(n)));

    try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
}

// FIXME: implement
pub fn path_create_directory(vm: *VirtualMachine) WasmError!void {
    const param0 = vm.popOperand(i32);
    const param1 = vm.popOperand(i32);
    const param2 = vm.popOperand(i32);
    std.debug.print("Unimplemented: path_create_directory({}, {}, {})\n", .{ param0, param1, param2 });
    try vm.pushOperand(u64, 0);
    @panic("Unimplemented: path_create_directory");
}

pub fn path_filestat_get(vm: *VirtualMachine) WasmError!void {
    const stat_ptr = vm.popOperand(u32);
    const path_len = vm.popOperand(u32);
    const path_ptr = vm.popOperand(u32);
    const flags = vm.popOperand(u32); // FIXME: we should probably be using this
    _ = flags;
    const fd = vm.popOperand(i32);

    const memory = try vm.inst.getMemory(0);
    const data = memory.memory();

    const sub_path = data[path_ptr .. path_ptr + path_len];

    const host_fd = vm.getHostFd(fd);
    const dir: fs.Dir = .{ .fd = host_fd };
    const stat = dir.statFile(sub_path) catch |err| {
        try vm.pushOperand(u64, @intFromEnum(toWasiError(err)));
        return;
    };

    try memory.write(u64, stat_ptr, 0, 0);
    try memory.write(u64, stat_ptr, 0x08, stat.inode);
    try memory.write(u64, stat_ptr, 0x10, @intFromEnum(toWasiFileType(stat.kind)));
    try memory.write(u64, stat_ptr, 0x18, 1);
    try memory.write(u64, stat_ptr, 0x20, stat.size);
    try memory.write(u64, stat_ptr, 0x28, toWasiTimestamp(stat.atime));
    try memory.write(u64, stat_ptr, 0x30, toWasiTimestamp(stat.mtime));
    try memory.write(u64, stat_ptr, 0x38, toWasiTimestamp(stat.ctime));

    try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
}

pub fn path_open(vm: *VirtualMachine) WasmError!void {
    const fd_ptr = vm.popOperand(u32);
    const fs_flags = vm.popOperand(u32);
    const fs_rights_inheriting = vm.popOperand(u64); // FIXME: we should probably be using this
    _ = fs_rights_inheriting;
    const fs_rights_base = vm.popOperand(u64);
    const oflags = vm.popOperand(u32);
    const path_len = vm.popOperand(u32);
    const path_ptr = vm.popOperand(u32);
    const dir_flags = vm.popOperand(u32); // FIXME: we should probably be using this
    _ = dir_flags;
    const dir_fd = vm.popOperand(i32);

    const memory = try vm.inst.getMemory(0);
    const data = memory.memory();

    const sub_path = data[path_ptr .. path_ptr + path_len];

    const host_fd = vm.getHostFd(dir_fd);

    var flags: u32 = @as(u32, if (oflags & wasi.O.CREAT != 0) os.O.CREAT else 0) |
        @as(u32, if (oflags & wasi.O.DIRECTORY != 0) os.O.DIRECTORY else 0) |
        @as(u32, if (oflags & wasi.O.EXCL != 0) os.O.EXCL else 0) |
        @as(u32, if (oflags & wasi.O.TRUNC != 0) os.O.TRUNC else 0) |
        @as(u32, if (fs_flags & wasi.FDFLAG.APPEND != 0) os.O.APPEND else 0) |
        @as(u32, if (fs_flags & wasi.FDFLAG.DSYNC != 0) os.O.DSYNC else 0) |
        @as(u32, if (fs_flags & wasi.FDFLAG.NONBLOCK != 0) os.O.NONBLOCK else 0) |
        @as(u32, if (fs_flags & wasi.FDFLAG.SYNC != 0) os.O.SYNC else 0);

    if ((fs_rights_base & wasi.RIGHT.FD_READ != 0) and
        (fs_rights_base & wasi.RIGHT.FD_WRITE != 0))
    {
        flags |= os.O.RDWR;
    } else if (fs_rights_base & wasi.RIGHT.FD_WRITE != 0) {
        flags |= os.O.WRONLY;
    } else if (fs_rights_base & wasi.RIGHT.FD_READ != 0) {
        flags |= os.O.RDONLY;
    }

    const mode = 0o644;
    const opened_fd = os.openat(host_fd, sub_path, flags, mode) catch |err| {
        try vm.pushOperand(u64, @intFromEnum(toWasiError(err)));
        return;
    };

    try memory.write(i32, fd_ptr, 0, opened_fd);

    try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
}

// FIXME: implement
pub fn poll_oneoff(vm: *VirtualMachine) WasmError!void {
    const param0 = vm.popOperand(i32);
    const param1 = vm.popOperand(i32);
    const param2 = vm.popOperand(i32);
    const param3 = vm.popOperand(i32);
    std.debug.print("Unimplemented: poll_oneoff({}, {}, {}, {})\n", .{ param0, param1, param2, param3 });
    try vm.pushOperand(u64, 0);
    @panic("Unimplemented: poll_oneoff");
}

pub fn proc_exit(vm: *VirtualMachine) WasmError!void {
    const param0 = vm.popOperand(i32);
    const code: u32 = std.math.absCast(param0);
    std.os.exit(@truncate(code));
}

pub fn random_get(vm: *VirtualMachine) WasmError!void {
    const buf_len = vm.popOperand(u32);
    const buf_ptr = vm.popOperand(u32);

    const memory = try vm.inst.getMemory(0);
    const data = memory.memory();

    std.crypto.random.bytes(data[buf_ptr .. buf_ptr + buf_len]);

    try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
}

// FIXME: figure out complete mapping of errors we expect from
//        zig std lib and how they map to the complete wasi.errno_t
//        struct. Such a complete mapping would be useful to add to
//        the zig std lib?
fn toWasiError(err: anyerror) wasi.errno_t {
    return switch (err) {
        error.AccessDenied => .ACCES,
        error.DiskQuota => .DQUOT,
        error.InputOutput => .IO,
        error.FileTooBig => .FBIG,
        error.NoSpaceLeft => .NOSPC,
        error.BrokenPipe => .PIPE,
        error.NotOpenForWriting => .BADF,
        error.SystemResources => .NOMEM,
        error.FileNotFound => .NOENT,
        error.PathAlreadyExists => .EXIST,
        error.IsDir => .ISDIR,
        else => std.debug.panic("WASI: Unhandled zig stdlib error: {s}", .{@errorName(err)}),
    };
}

// FIXME: can we write a program that exercises all of the zig file kinds and wasi filetypes?
//        Again, such a mapping would be useful to add to the zig std lib?
fn toWasiFileType(kind: fs.File.Kind) wasi.filetype_t {
    return switch (kind) {
        .block_device => .BLOCK_DEVICE,
        .character_device => .CHARACTER_DEVICE,
        .directory => .DIRECTORY,
        .sym_link => .SYMBOLIC_LINK,
        .file => .REGULAR_FILE,
        .unknown => .UNKNOWN,

        .named_pipe,
        .unix_domain_socket,
        .whiteout,
        .door,
        .event_port,
        => .UNKNOWN,
    };
}

fn toWasiTimestamp(ns: i128) u64 {
    return @as(u64, @intCast(ns));
}
