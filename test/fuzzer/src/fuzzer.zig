const std = @import("std");
const mem = std.mem;
const fs = std.fs;
const process = std.process;
const zware = @import("zware");
const ArrayList = std.ArrayList;
const Module = zware.Module;
const Store = zware.Store;
const Instance = zware.Instance;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;
var gpa = GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    var args = process.args();
    _ = args.skip();
    const directory = args.next() orelse return error.NoFilename;

    defer _ = gpa.deinit();

    var name_arena = ArenaAllocator.init(gpa.allocator());
    defer _ = name_arena.deinit();
    const name_alloc = name_arena.allocator();

    var wasm_files = ArrayList([]const u8).init(gpa.allocator());
    defer wasm_files.deinit();

    var dir = try std.fs.cwd().openIterableDir(directory, .{});
    defer dir.close();

    var it = dir.iterate();

    var file_count: usize = 0;
    while (try it.next()) |entry| {
        if (entry.kind == .File) file_count += 1;
        if (mem.endsWith(u8, entry.name, ".wasm")) {
            const s = try name_alloc.alloc(u8, entry.name.len);
            mem.copy(u8, s, entry.name);
            try wasm_files.append(s);
        }
    }

    var prng = std.rand.DefaultPrng.init(0x12345678);
    const random = prng.random();

    while (true) {
        var arena = ArenaAllocator.init(gpa.allocator());
        defer _ = arena.deinit();
        const alloc = arena.allocator();

        // 1. Randomly choose .wasm from testsuite-generated
        random.shuffle([]const u8, wasm_files.items);
        const wasm_file = wasm_files.items[0];

        std.log.info("loading {s}", .{wasm_file});

        // 2. Load file
        const program = try dir.dir.readFileAlloc(alloc, wasm_file, 0xFFFFFFF);

        // 3. TODO: flip some bits

        var store: Store = Store.init(alloc);

        var module = Module.init(alloc, program);
        module.decode() catch continue;

        var new_inst = Instance.init(alloc, &store, module);
        const index = store.addInstance(new_inst) catch continue;
        var inst = store.instance(index) catch continue;
        inst.instantiate(index) catch continue;
    }
}
