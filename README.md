<h1 align="center">zware</h1>

<div align="center">
  <img src="https://github.com/malcolmstill/web-assembly-logo/blob/master/dist/icon/web-assembly-icon-128px.png" alt="WebAssembly logo: a purple-blue square containing the uppercase letters W A. The square has a semicirclular notch on the top edge in the middle" />
  <br />
  <strong>Zig WebAssembly Runtime Engine</strong>
</div>

## About

`zware` is a library for executing WebAssembly embedded in [Zig](https://ziglang.org) programs.

## Example

From `examples/fib`:

```zig
const std = @import("std");
const zware = @import("zware");
const Module = zware.Module;
const Store = zware.Store;
const Instance = zware.Instance;
const GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator;
const ArenaAllocator = std.heap.ArenaAllocator;
var gpa = GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const bytes = @embedFile("fib.wasm");

    var store = Store.init(alloc);
    defer store.deinit();

    var module = Module.init(alloc, bytes);
    defer module.deinit();
    try module.decode();

    var instance = Instance.init(alloc, &store, module);
    try instance.instantiate();
    defer instance.deinit();

    const n = 39;
    var in = [1]u64{n};
    var out = [1]u64{0};
    try instance.invoke("fib", in[0..], out[0..], .{});
    std.debug.print("fib({}) = {}\n", .{ n, @bitCast(i32, @truncate(u32, out[0])) });
}
```

## Requirements

### Compile-time

- Zig 0.10.0

### Run-time

- None, zig generates static binaries:

```bash
➜  zware git:(master) ✗ ldd fib
        not a dynamic executable
```

## Goals

- Embed WebAssembly programs in other zig programs
- Be fast enough to be useful
- Implement WASI

## Status

- The project is very much alpha quality
- WebAssembly 2.0 supported (apart from the vector / SIMD support which is WIP)
- The WebAssembly official testsuite passes and zware includes a fuzzer

## Running the testsuite

1. Build the test runner

```
zig build --build-file test/testrunner/build.zig --prefix ./
```

2. Run

```
sh test/run-generated.sh
```
