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
const ArenaAllocator = std.heap.ArenaAllocator;
var gpa = GeneralPurposeAllocator(.{}){};

const native_os = @import("builtin").os.tag;

pub fn main() !void {
    defer _ = gpa.deinit();
    
    var args = switch (native_os) {
        .windows, .wasi => try process.ArgIterator.initWithAllocator(gpa.allocator()),
        else => process.ArgIterator.init(),
    };
    defer args.deinit();
    
    _ = args.skip();
    const directory = args.next() orelse return error.NoFilename;

    var wasm_files = ArrayList([]const u8).init(gpa.allocator());
    defer wasm_files.deinit();

    var dir = try std.fs.cwd().openDir(directory, .{
        .no_follow = true,
        .iterate = true,
    });
    defer dir.close();

    var it = dir.iterate();

    var file_count: usize = 0;
    while (try it.next()) |entry| {
        if (entry.kind == .file) file_count += 1;
        if (mem.endsWith(u8, entry.name, ".wasm")) {
            std.log.info("{s}", .{entry.name});
            var arena = ArenaAllocator.init(gpa.allocator());
            defer _ = arena.deinit();
            const alloc = arena.allocator();

            const program = try dir.readFileAlloc(alloc, entry.name, 0xFFFFFFF);
            if (program.len == 0) continue;

            var store: Store = Store.init(alloc);

            var module = Module.init(alloc, program);
            module.decode() catch continue;

            var instance = Instance.init(alloc, &store, module);
            instance.instantiate() catch continue;
        }
    }
}
