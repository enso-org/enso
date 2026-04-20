# ydoc-shared

Types and primitives shared between `ydoc-server` and its clients (GUI,
inspect). **Also owns the in-browser Enso AST module.**

## What lives here

- `src/ast/` — The Enso AST as used on the client. Parsing happens via the Rust
  parser compiled to WASM (`rust-ffi` package). This AST is the _authoritative_
  structural representation for the GUI — the project-view binds reactive Vue
  state to it. Exported as `ydoc-shared/ast`.
- `src/languageServer.ts` + `src/languageServer/`,
  `src/languageServerTypes.ts` + `src/languageServerTypes/` — Type definitions
  and client helpers for the Enso LS JSON-RPC protocol.
- `src/yjsModel.ts` — The canonical Yjs document shape used for a single opened
  module (text + metadata + awareness).
- `src/binaryProtocol.ts` — Custom binary framing used on top of Yjs (metadata
  round-tripping without flattening to text).
- `src/ensoFile.ts` — `.enso` file format helpers (on-disk representation
  reconstructed client-side).
- `src/uuid.ts`, `src/util/` — small helpers.
- `parser-codegen/` — scripts/inputs that derive some of the AST TS types from
  the Rust parser's metamodel. Re-run when the parser output format changes.

## WASM dependency

The AST module depends on `rust-ffi` (`workspace:*`). Changes to
`lib/rust/parser/` that touch the metamodel propagate here via the WASM build;
don't forget to rebuild the FFI package (`corepack pnpm run -r compile` from
repo root).

## Exports

Entry points in `package.json`:

- `ydoc-shared/ast` → `src/ast/index.ts`
- `ydoc-shared/<name>` → `src/<name>.ts` for any top-level file

Import by the most specific subpath available — don't import `ydoc-shared` with
a bare specifier.

## Test

Vitest only (`pnpm run test:unit`). Tests live in `src/__tests__/`.
