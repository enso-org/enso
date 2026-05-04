# enso-parser

The Enso language parser. Produces the AST consumed by every component of the
toolchain — the engine (via JNI + generated Java types), the GUI (via WASM), and
debug/analysis tooling.

## Pipeline (read `src/lib.rs` module docs for the full story)

1. **Lex** — `src/lexer.rs` produces `Token`s.
2. **Build macro registry** — hardcoded macros (`if…then…else`, lambda `->`,
   parens, etc.) are registered into a `MacroMatchTree`. There is no
   user-defined macro support.
3. **Split by macro segments** — token stream is segmented based on macro
   trigger tokens; nesting is hierarchical.
4. **Resolve RHS / LHS patterns** — tokens are partitioned between each macro's
   last segment and its parent.
5. **Emit AST** — `src/syntax/tree/` is the final `Tree` structure.

## Subcrates

Each has its own `CLAUDE.md`:

- `jni/` — `cdylib` loaded by the engine's JVM (`libenso_parser.so` / `.dll` /
  `.dylib`). Thin FFI surface.
- `generate-java/` — Binary that reflects the parser AST (via `enso-reflect`)
  and generates Java classes + deserializers. Rerun when AST changes.
- `debug/` — CLI + S-expression dump for inspecting parser output. `debug/fuzz/`
  contains fuzz targets.
- `macros/` — `proc-macro` crate used only by the parser itself.
- `schema/` — Writes the AST's metamodel as JSON for external tools.
- `src/syntax/tree/visitor/` — Derive-macro crate (optional, via `debug`
  feature) that generates visitor boilerplate over the `Tree` type.

## Feature flags

- `debug` — enables the visitor derive crate; required for `parser-debug` and
  the generate-java pipeline.
- `nightly` — unlocks bench/test code that requires nightly features.

## AST as a contract

The parser's AST is **the** source of truth for:

- Engine Java types (regenerate via `generate-java`).
- Client-side TypeScript AST (derived via `app/ydoc-shared/parser-codegen/` and
  consumed from the WASM build in `app/rust-ffi/`).

Any breaking change to the AST rippled into both targets. Ship changes as a
single coordinated set: parser → regenerate Java → rebuild WASM → update
consumers.

## Testing

- Unit tests in `src/` via `#[cfg(test)] mod tests`.
- Snapshot-style tests in `debug/` via `insta`.
- Fuzzing in `debug/fuzz/`.

## Non-WASM vs WASM

Dev/test deps differ by target (see `Cargo.toml`): host builds use `rand` +
`enso-metamodel` tests; WASM builds use `wasm-bindgen-test`.
