const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const fs = std.fs;
const posix = std.posix;
const math = std.math;
const wasi = std.os.wasi;
const native_os = builtin.os.tag;

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

        @memcpy(data[argv_i_ptr .. argv_i_ptr + arg_len], arg[0..arg_len]);
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
    posix.close(host_fd);

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

    try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
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
        const read = posix.read(host_fd, buf) catch |err| {
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
            posix.lseek_CUR(fd, offset) catch |err| {
                try vm.pushOperand(u64, @intFromEnum(toWasiError(err)));
                return;
            };
        },
        wasi.whence_t.END => {
            posix.lseek_END(fd, offset) catch |err| {
                try vm.pushOperand(u64, @intFromEnum(toWasiError(err)));
                return;
            };
        },
        wasi.whence_t.SET => {
            posix.lseek_SET(fd, @intCast(offset)) catch |err| {
                try vm.pushOperand(u64, @intFromEnum(toWasiError(err)));
                return;
            };
        },
    }

    const new_offset = posix.lseek_CUR_get(fd) catch |err| {
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

        const written = posix.write(host_fd, bytes) catch |err| {
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

    const fs_flags: wasi.fdflags_t = @bitCast(@as(u16, @truncate(vm.popOperand(u32))));

    const fs_rights_inheriting = vm.popOperand(u64); // FIXME: we should probably be using this
    _ = fs_rights_inheriting;

    const fs_rights_base: wasi.rights_t = @bitCast(vm.popOperand(u64));

    const oflags: wasi.oflags_t = @bitCast(@as(u16, @truncate(vm.popOperand(u32))));

    const path_len = vm.popOperand(u32);
    const path_ptr = vm.popOperand(u32);
    const dir_flags = vm.popOperand(u32); // FIXME: we should probably be using this
    _ = dir_flags;
    const dir_fd = vm.popOperand(i32);

    const memory = try vm.inst.getMemory(0);
    const data = memory.memory();

    const sub_path = data[path_ptr .. path_ptr + path_len];

    const host_fd = vm.getHostFd(dir_fd);

    const flags = posix.O{
        .CREAT = oflags.CREAT,
        .DIRECTORY = oflags.DIRECTORY,
        .EXCL = oflags.EXCL,
        .TRUNC = oflags.TRUNC,

        .APPEND = fs_flags.APPEND,
        .DSYNC = fs_flags.DSYNC,
        .NONBLOCK = fs_flags.NONBLOCK,
        .SYNC = fs_flags.SYNC,

        .ACCMODE = if (fs_rights_base.FD_READ and fs_rights_base.FD_WRITE) blk: {
            break :blk .RDWR;
        } else if (fs_rights_base.FD_WRITE) blk: {
            break :blk .WRONLY;
        } else if (fs_rights_base.FD_READ) blk: {
            break :blk .RDONLY;
        } else unreachable,
    };

    const mode = 0o644;
    const opened_fd = posix.openat(host_fd, sub_path, flags, mode) catch |err| {
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
    const code: u32 = @abs(param0);
    posix.exit(@truncate(code));
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
        error.NotOpenForReading => .BADF,
        error.SystemResources => .NOMEM,
        error.FileNotFound => .NOENT,
        error.PathAlreadyExists => .EXIST,
        error.IsDir => .ISDIR,
        error.Unseekable => .SPIPE, // ESPIPE: Illegal seek (e.g., on pipes/sockets)
        error.InvalidArgument => .INVAL,
        error.PermissionDenied => .PERM,
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

const _ = std.testing.refAllDecls();

/// fd_tell - Get the current offset of a file descriptor
/// Returns the current position within the file
pub fn fd_tell(vm: *VirtualMachine) WasmError!void {
    const offset_ptr = vm.popOperand(u32);
    const fd = vm.popOperand(i32);

    const host_fd = vm.getHostFd(fd);
    const current_offset = posix.lseek_CUR_get(host_fd) catch |err| {
        try vm.pushOperand(u64, @intFromEnum(toWasiError(err)));
        return;
    };

    const memory = try vm.inst.getMemory(0);
    try memory.write(u64, offset_ptr, 0, current_offset);

    try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
}

/// Convert native directory entry type to WASI filetype
fn toWasiFiletype(native_type: u8) wasi.filetype_t {
    return switch (native_type) {
        std.posix.DT.BLK => .BLOCK_DEVICE,
        std.posix.DT.CHR => .CHARACTER_DEVICE,
        std.posix.DT.DIR => .DIRECTORY,
        std.posix.DT.LNK => .SYMBOLIC_LINK,
        std.posix.DT.REG => .REGULAR_FILE,
        std.posix.DT.SOCK => .SOCKET_STREAM,
        std.posix.DT.FIFO => .UNKNOWN, // WASI has no FIFO type
        else => .UNKNOWN,
    };
}

/// Write a WASI dirent entry to memory
/// Uses std.os.wasi.dirent_t for spec-compliant structure layout
fn writeWasiDirent(
    mem_data: []u8,
    memory: anytype,
    offset: u32,
    next_cookie: u64,
    inode: u64,
    name: []const u8,
    filetype: wasi.filetype_t,
) !void {
    // WASI dirent_t structure (from std.os.wasi):
    //   next: dircookie_t (u64) - offset 0
    //   ino: inode_t (u64) - offset 8
    //   namlen: dirnamlen_t (u32) - offset 16
    //   type: filetype_t (u8) - offset 20
    //   [3 bytes padding to align to 24 bytes]
    //   name follows immediately after (not null-terminated)
    try memory.write(u64, offset, 0, next_cookie);
    try memory.write(u64, offset + 8, 0, inode);
    try memory.write(u32, offset + 16, 0, @as(u32, @intCast(name.len)));
    mem_data[offset + 20] = @intFromEnum(filetype);
    // Padding bytes 21-23 are implicitly handled
    const name_offset = offset + @sizeOf(wasi.dirent_t);
    @memcpy(mem_data[name_offset..][0..name.len], name);
}

/// fd_readdir - Read directory entries from a directory file descriptor
/// Reads directory entries into the provided buffer using the cookie for pagination.
/// The cookie value 0 starts from the beginning; subsequent calls use the d_next
/// value from the last entry to continue reading.
///
/// Uses std.os.wasi types for spec-compliant WASI dirent structure:
///   next: dircookie_t (u64) - Cookie for next entry
///   ino: inode_t (u64) - Inode number
///   namlen: dirnamlen_t (u32) - Length of the name
///   type: filetype_t (u8) - File type
///   [3 bytes padding]
///   name: [namlen]u8 - Name (not null-terminated)
///
/// Platform support:
///   - Linux: getdents64 syscall (no libc required)
///   - macOS/iOS/BSD: getdirentries/getdents via std.c (requires libc)
///   - Windows: NtQueryDirectoryFile via std.os.windows
pub fn fd_readdir(vm: *VirtualMachine) WasmError!void {
    const bufused_ptr = vm.popOperand(u32);
    const cookie = vm.popOperand(u64);
    const buf_len = vm.popOperand(u32);
    const buf_ptr = vm.popOperand(u32);
    const fd = vm.popOperand(i32);

    const memory = try vm.inst.getMemory(0);
    const mem_data = memory.memory();
    const host_fd = vm.getHostFd(fd);

    // Size of WASI dirent header (from std.os.wasi.dirent_t)
    const dirent_header_size: u32 = @sizeOf(wasi.dirent_t);

    switch (native_os) {
        .linux => {
            // Linux implementation using getdents64 syscall (no libc needed)
            _ = std.os.linux.lseek(host_fd, 0, std.os.linux.SEEK.SET);

            var entry_idx: u64 = 0;
            var bytes_used: u32 = 0;

            while (true) {
                var kernel_buf: [8192]u8 = undefined;
                const nread = std.os.linux.getdents64(host_fd, &kernel_buf, kernel_buf.len);
                if (nread == 0) break;
                if (@as(isize, @bitCast(nread)) < 0) break;

                var kernel_offset: usize = 0;
                while (kernel_offset < nread) {
                    const linux_entry: *align(1) std.os.linux.dirent64 = @ptrCast(&kernel_buf[kernel_offset]);
                    const name = mem.sliceTo(@as([*:0]u8, @ptrCast(&linux_entry.name)), 0);

                    kernel_offset += linux_entry.reclen;

                    // Skip . and ..
                    if (mem.eql(u8, name, ".") or mem.eql(u8, name, "..")) continue;

                    // Skip entries before the cookie position
                    if (entry_idx < cookie) {
                        entry_idx += 1;
                        continue;
                    }

                    const wasi_entry_size: u32 = dirent_header_size + @as(u32, @intCast(name.len));

                    if (bytes_used + wasi_entry_size > buf_len) {
                        try memory.write(u32, bufused_ptr, 0, bytes_used);
                        try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
                        return;
                    }

                    try writeWasiDirent(
                        mem_data,
                        memory,
                        buf_ptr + bytes_used,
                        entry_idx + 1,
                        linux_entry.ino,
                        name,
                        toWasiFiletype(linux_entry.type),
                    );

                    bytes_used += wasi_entry_size;
                    entry_idx += 1;
                }
            }

            try memory.write(u32, bufused_ptr, 0, bytes_used);
            try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
        },

        .macos, .ios, .tvos, .watchos, .visionos => {
            // Darwin implementation using getdirentries via libc
            posix.lseek_SET(host_fd, 0) catch {
                try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.BADF));
                return;
            };

            var entry_idx: u64 = 0;
            var bytes_used: u32 = 0;
            var seek: i64 = 0;

            while (true) {
                var kernel_buf: [8192]u8 align(@alignOf(std.c.dirent)) = undefined;
                const nread = std.c.getdirentries(host_fd, &kernel_buf, kernel_buf.len, &seek);
                if (nread <= 0) break;

                var kernel_offset: usize = 0;
                while (kernel_offset < @as(usize, @intCast(nread))) {
                    const entry: *align(1) std.c.dirent = @ptrCast(&kernel_buf[kernel_offset]);
                    const name = @as([*]const u8, @ptrCast(&entry.name))[0..entry.namlen];

                    kernel_offset += entry.reclen;

                    // Skip . and ..
                    if (mem.eql(u8, name, ".") or mem.eql(u8, name, "..")) continue;

                    if (entry_idx < cookie) {
                        entry_idx += 1;
                        continue;
                    }

                    const wasi_entry_size: u32 = dirent_header_size + @as(u32, @intCast(name.len));

                    if (bytes_used + wasi_entry_size > buf_len) {
                        try memory.write(u32, bufused_ptr, 0, bytes_used);
                        try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
                        return;
                    }

                    try writeWasiDirent(
                        mem_data,
                        memory,
                        buf_ptr + bytes_used,
                        entry_idx + 1,
                        entry.ino,
                        name,
                        toWasiFiletype(entry.type),
                    );

                    bytes_used += wasi_entry_size;
                    entry_idx += 1;
                }
            }

            try memory.write(u32, bufused_ptr, 0, bytes_used);
            try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
        },

        .freebsd, .openbsd, .netbsd, .dragonfly => {
            // BSD implementation using getdents via libc
            posix.lseek_SET(host_fd, 0) catch {
                try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.BADF));
                return;
            };

            var entry_idx: u64 = 0;
            var bytes_used: u32 = 0;

            while (true) {
                var kernel_buf: [8192]u8 align(@alignOf(std.c.dirent)) = undefined;
                const nread = std.c.getdents(host_fd, &kernel_buf, kernel_buf.len);
                if (nread <= 0) break;

                var kernel_offset: usize = 0;
                while (kernel_offset < @as(usize, @intCast(nread))) {
                    const entry: *align(1) std.c.dirent = @ptrCast(&kernel_buf[kernel_offset]);
                    const name = @as([*]const u8, @ptrCast(&entry.name))[0..entry.namlen];

                    kernel_offset += entry.reclen;

                    // Skip . and ..
                    if (mem.eql(u8, name, ".") or mem.eql(u8, name, "..")) continue;

                    if (entry_idx < cookie) {
                        entry_idx += 1;
                        continue;
                    }

                    const wasi_entry_size: u32 = dirent_header_size + @as(u32, @intCast(name.len));

                    if (bytes_used + wasi_entry_size > buf_len) {
                        try memory.write(u32, bufused_ptr, 0, bytes_used);
                        try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
                        return;
                    }

                    try writeWasiDirent(
                        mem_data,
                        memory,
                        buf_ptr + bytes_used,
                        entry_idx + 1,
                        entry.fileno,
                        name,
                        toWasiFiletype(entry.type),
                    );

                    bytes_used += wasi_entry_size;
                    entry_idx += 1;
                }
            }

            try memory.write(u32, bufused_ptr, 0, bytes_used);
            try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
        },

        .windows => {
            // Windows implementation using NtQueryDirectoryFile
            const w = std.os.windows;

            var entry_idx: u64 = 0;
            var bytes_used: u32 = 0;
            var first_iter = true;

            while (true) {
                var io: w.IO_STATUS_BLOCK = undefined;
                var dir_buf: [4096]u8 align(@alignOf(w.FILE_BOTH_DIR_INFORMATION)) = undefined;

                const rc = w.ntdll.NtQueryDirectoryFile(
                    @ptrFromInt(@as(usize, @bitCast(@as(isize, host_fd)))),
                    null,
                    null,
                    null,
                    &io,
                    &dir_buf,
                    dir_buf.len,
                    .FileBothDirectoryInformation,
                    w.FALSE, // ReturnSingleEntry
                    null, // FileName filter
                    if (first_iter) w.TRUE else w.FALSE, // RestartScan
                );
                first_iter = false;

                if (rc != .SUCCESS) break;
                if (io.Information == 0) break;

                var buf_offset: usize = 0;
                while (buf_offset < io.Information) {
                    const dir_info: *align(2) w.FILE_BOTH_DIR_INFORMATION = @ptrCast(@alignCast(&dir_buf[buf_offset]));

                    // Get name as UTF-8
                    const name_wtf16 = @as([*]u16, @ptrCast(&dir_info.FileName))[0 .. dir_info.FileNameLength / 2];
                    var name_buf: [256]u8 = undefined;
                    const name_len = std.unicode.wtf16LeToWtf8(&name_buf, name_wtf16);
                    const name = name_buf[0..name_len];

                    // Move to next entry
                    if (dir_info.NextEntryOffset != 0) {
                        buf_offset += dir_info.NextEntryOffset;
                    } else {
                        buf_offset = io.Information; // Force exit
                    }

                    // Skip . and ..
                    if (mem.eql(u8, name, ".") or mem.eql(u8, name, "..")) continue;

                    // Skip entries before cookie
                    if (entry_idx < cookie) {
                        entry_idx += 1;
                        continue;
                    }

                    // Determine file type
                    const filetype: wasi.filetype_t = blk: {
                        const attrs = dir_info.FileAttributes;
                        if (attrs & w.FILE_ATTRIBUTE_DIRECTORY != 0) break :blk .DIRECTORY;
                        if (attrs & w.FILE_ATTRIBUTE_REPARSE_POINT != 0) break :blk .SYMBOLIC_LINK;
                        break :blk .REGULAR_FILE;
                    };

                    const wasi_entry_size: u32 = dirent_header_size + @as(u32, @intCast(name.len));

                    if (bytes_used + wasi_entry_size > buf_len) {
                        try memory.write(u32, bufused_ptr, 0, bytes_used);
                        try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
                        return;
                    }

                    // Windows doesn't have inodes, use FileIndex as a pseudo-inode
                    const pseudo_inode: u64 = @as(u64, dir_info.FileIndex);

                    try writeWasiDirent(
                        mem_data,
                        memory,
                        buf_ptr + bytes_used,
                        entry_idx + 1,
                        pseudo_inode,
                        name,
                        filetype,
                    );

                    bytes_used += wasi_entry_size;
                    entry_idx += 1;
                }
            }

            try memory.write(u32, bufused_ptr, 0, bytes_used);
            try vm.pushOperand(u64, @intFromEnum(wasi.errno_t.SUCCESS));
        },

        else => {
            @compileError("fd_readdir not implemented for this platform");
        },
    }
}
