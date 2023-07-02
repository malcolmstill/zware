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

var prng = std.rand.DefaultPrng.init(0x12345678);
var program: []u8 = undefined;
var fuzzed_name: []u8 = undefined;
var fuzzed_dir: [:0]const u8 = undefined;

pub fn main() !void {
    var args = process.args();
    _ = args.skip();
    const directory = args.next() orelse return error.NoFilename;
    fuzzed_dir = args.next() orelse return error.NoFilename;

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
        if (entry.kind == .file) file_count += 1;
        if (mem.endsWith(u8, entry.name, ".wasm")) {
            const s = try name_alloc.alloc(u8, entry.name.len);
            mem.copy(u8, s, entry.name);
            try wasm_files.append(s);
        }
    }

    if (file_count == 0) return error.NoWasmFiles;

    const random = prng.random();

    const flips = [8]u8{
        0b00000001,
        0b00000010,
        0b00000100,
        0b00001000,
        0b00010000,
        0b00100000,
        0b01000000,
        0b10000000,
    };

    var i: usize = 0;
    while (true) : (i += 1) {
        if (i % 5000 == 0) {
            std.log.info("i = {}", .{i});
        }
        var arena = ArenaAllocator.init(gpa.allocator());
        defer _ = arena.deinit();
        const alloc = arena.allocator();

        // 1. Randomly choose .wasm from testsuite-generated
        random.shuffle([]const u8, wasm_files.items);
        const wasm_file = wasm_files.items[0];

        // 2. Load file
        program = try dir.dir.readFileAlloc(alloc, wasm_file, 0xFFFFFFF);
        if (program.len == 0) continue;

        // 3. Flip a bit (maybe we should flip bits somewhat proportional to the file size)
        const byte_to_change = random.uintLessThan(usize, program.len);
        const bit_to_change = random.uintLessThan(u6, 8);
        program[byte_to_change] = program[byte_to_change] ^ flips[bit_to_change];
        fuzzed_name = try fmt.allocPrint(name_alloc, "{}_{}.{s}", .{ byte_to_change, bit_to_change, wasm_file });
        defer name_alloc.free(fuzzed_name);

        var store: Store = Store.init(alloc);

        var module = Module.init(alloc, program);
        module.decode() catch continue;

        var instance = Instance.init(alloc, &store, module);
        instance.instantiate() catch continue;
    }
}

pub fn panic(msg: []const u8, error_return_trace: ?*std.builtin.StackTrace, ret_addr: ?usize) noreturn {
    var out_dir = std.fs.cwd().openDir(fuzzed_dir, .{}) catch unreachable;
    defer out_dir.close();

    std.log.info("Fuzzer found bug: {s}", .{fuzzed_name});
    out_dir.writeFile(fuzzed_name, program) catch unreachable;
    std.builtin.default_panic(msg, error_return_trace, ret_addr);
}
