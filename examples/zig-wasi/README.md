# Steps

1. Compile zig compiler to .wasm

```zsh
./zig build -p wasi -Dwasi-bootstrap --verbose
```