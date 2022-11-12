#

In https://github.com/andrewrk/zig-wasi, Andrew is experimenting with
a WebAssembly interpreter with the explicit goal of executing the
zig compiler compiled to wasm. As I understand, this is part of a plan to 
remove the stage1 C++ compiler.

While zware isn't suitable for this (the interpreter actually needs to
be written in C and zware makes heavy use of tail calls that are not
supported on every platform), trying to get zware to run the zig compiler
compiled to wasm is just too much of a cool example for me not to
try and recreate.

## Steps for generating zig.wasm (haven't actually got this working yet)

1. Clone the https://github.com/ziglang/zig repo
2. `mkdir build`
3. `cd build`
4. `cmake ..`
5. `cd ..`
6. `git checkout wasi-bootstrap`
6. `build/stage3/bin/zig build -p wasi -Dwasi-bootstrap --zig-lib-dir ./lib`

I think 6. requires specifying `--zig-lib-dir` because otherwise we use
the std lib in `build/stage3/lib` but we want some code that is in the
toplevel `wasi-bootsrap` `lib/`