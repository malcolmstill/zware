<h1 align="center">foxwren</h1>

<div align="center">
  <img src="https://github.com/malcolmstill/web-assembly-logo/blob/master/dist/icon/web-assembly-icon-128px.png" alt="WebAssembly logo: a purple-blue square containing the uppercase letters W A. The square has a semicirclular notch on the top edge in the middle" />
  <br />
  <strong>A WebAssembly runtime environment (written in Zig)</strong>
</div>

## About

`foxwren` is a library for executing WebAssembly embedded in [Zig](https://ziglang.org) programs.

## Example

From `examples/fib`:

```zig
const std = @import("std");
const foxwren = @import("foxwren");
const Module = foxwren.Module;
const Store = foxwren.Store;
const Instance = foxwren.Instance;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;
var gpa = GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    defer _ = gpa.deinit();

    var arena = ArenaAllocator.init(&gpa.allocator);
    defer _ = arena.deinit();

    const bytes = @embedFile("../../../test/fib.wasm");

    var store: Store = Store.init(&arena.allocator);

    var module = Module.init(&arena.allocator, bytes);
    try module.decode();

    var new_inst = Instance.init(&arena.allocator, &store, module);
    const index = try store.addInstance(new_inst);
    var inst = try store.instance(index);
    try inst.instantiate(index);

    const n = 28;
    var in = [1]u64{n};
    var out = [1]u64{0};
    try inst.invoke("fib", in[0..], out[0..], .{});
    std.debug.warn("fib({}) = {}\n", .{ n, @bitCast(i32, @truncate(u32, out[0])) });
}
```

## Requirements

### Compile-time

- Zig 0.8.1

### Run-time

- None, zig generates static binaries:

```bash
➜  foxwren git:(master) ✗ ldd fib
        not a dynamic executable
```

## Goals

- To allow the author, gentle reader, to understand WebAssembly
- Embed WebAssembly programs in other zig programs
- Be fast enough to be useful
- Implement some of the proposed extensions
- Implement WASI

## Status

- The project is very much alpha quality
- The full WebAssembly MVP spec testsuite passes
- Currently, only the MVP spec is implemented without any extensions other than multiple-return values.

## Running the testsuite

1. Build the test runner

```
zig build --build-file test/build.zig --prefix ./
```

2. Run

```
sh test/run-generated.sh
```