<h1 align="center">foxwren</h1>

<div align="center">
  <img src="https://github.com/malcolmstill/web-assembly-logo/blob/master/dist/icon/web-assembly-icon-128px.png" alt="WebAssembly logo: a purple-blue square containing the uppercase letters W A. The square has a semicirclular notch on the top edge in the middle" />
  <br />
  <strong>A WebAssembly runtime environment (written in Zig)</strong>
</div>

## About

`foxwren` is a library for executing WebAssembly embedded in [Zig](https://ziglang.org) programs.

## Goals

- To allow the author, gentle reader, to understand WebAssembly
- Embed WebAssembly programs in other zig programs
- Be fast enough to be useful

## Future goals

- Implement some of the proposed extensions
- Implement WASI

## Status

- The project is very much alpha quality
- The majority of the WebAssembly MVP spec testsuite passes (and is integrated into the CI) with a few exceptions and the type-checking validator code is not complete and so the `assert_invalid` tests are not included yet. See #13 and #82.
- Currently, only the MVP spec is implemented without any extensions other than multiple-return values.
