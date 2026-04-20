# rust-ffi

`wasm-bindgen` wrapper exposing `enso-parser` (from `lib/rust/parser/`) to
JavaScript. Built as a `cdylib` + `rlib`. Output is consumed by
`ydoc-shared/src/ast/` on the client.

## Gotcha: wasm-bindgen version pinning

`wasm-bindgen = "=0.2.100"` is an **exact** pin. It must match the version baked
into the `rules_rust_wasm_bindgen` Bazel toolchain. Patch-release bumps can
change an internal format and break the GUI build — **never bump this with `~`
or `^`**. Update the Bazel toolchain and this line in lockstep.

## Build

- Cargo path (used by Vite via `vite-plugin-wasm`): handled automatically by
  pnpm workflows.
- Bazel path: `BUILD.bazel` in this directory.

## Adding a new API

1. Add a `#[wasm_bindgen]` function here.
2. Extend TypeScript bindings in `ydoc-shared/src/ast/`.
3. Rebuild WASM (`corepack pnpm run -r compile` from repo root is the simplest).

Keep the surface tiny — the WASM binary ends up in the GUI bundle and every
exported symbol costs kilobytes.
