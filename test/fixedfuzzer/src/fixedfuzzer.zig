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
const FixedBufferAllocator = std.heap.FixedBufferAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;
var gpa = GeneralPurposeAllocator(.{}){};

var prng = std.rand.DefaultPrng.init(0x12345678);
var program: []u8 = undefined;
var fuzzed_name: []u8 = undefined;
var fuzzed_dir: [:0]const u8 = undefined;

var name_memory: [30 * 1024 * 1024]u8 = undefined;
var file_memory: [30 * 1024 * 1024]u8 = undefined;
var engine_memory: [30 * 1024 * 1024]u8 = undefined;

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
        if (entry.kind == .File) file_count += 1;
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
        var name_buffer = FixedBufferAllocator.init(name_memory[0..]);
        var name_buffer_alloc = name_buffer.allocator();
        if (i % 5000 == 0) {
            std.debug.print("i = {}\n", .{i});
        }
        var file_buffer = FixedBufferAllocator.init(file_memory[0..]);
        var file_buffer_alloc = file_buffer.allocator();
        var engine_buffer = FixedBufferAllocator.init(engine_memory[0..]);
        var engine_buffer_alloc = engine_buffer.allocator();

        // 1. Randomly choose .wasm from testsuite-generated
        random.shuffle([]const u8, wasm_files.items);
        const wasm_file = wasm_files.items[0];

        // 2. Load file
        program = try dir.dir.readFileAlloc(file_buffer_alloc, wasm_file, 0xFFFFFFF);
        if (program.len == 0) continue;
        fuzzed_name = try fmt.allocPrint(name_buffer_alloc, "{s}", .{wasm_file});
        // 3. Flip a bit (maybe we should flip bits somewhat proportional to the file size)

        const num_bytes = 1; // random.uintLessThan(usize, program.len) + 1;
        var j: usize = 0;
        while (j < num_bytes) : (j += 1) {
            const byte_to_change = random.uintLessThan(usize, program.len);
            const bit_to_change = random.uintLessThan(u6, 8);
            program[byte_to_change] = program[byte_to_change] ^ flips[bit_to_change];
            const old_name = fuzzed_name;
            fuzzed_name = try fmt.allocPrint(name_buffer_alloc, "{}_{}.{s}", .{ byte_to_change, bit_to_change, fuzzed_name });
            name_buffer_alloc.free(old_name);
        }
        defer name_buffer_alloc.free(fuzzed_name);

        // std.log.info("fuzzed name = {s}", .{fuzzed_name});

        var store: Store = Store.init(engine_buffer_alloc);

        var module = Module.init(engine_buffer_alloc, program);
        module.decode() catch continue;

        var instance = Instance.init(engine_buffer_alloc, &store, module);
        instance.instantiate() catch continue;
    }
}

pub fn panic(msg: []const u8, error_return_trace: ?*std.builtin.StackTrace) noreturn {
    var out_dir = std.fs.cwd().openDir(fuzzed_dir, .{}) catch unreachable;
    defer out_dir.close();

    std.log.info("Fuzzer found bug: {s}", .{fuzzed_name});
    out_dir.writeFile(fuzzed_name, program) catch unreachable;
    std.builtin.default_panic(msg, error_return_trace);
}
