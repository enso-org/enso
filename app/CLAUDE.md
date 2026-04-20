# app/

Desktop app and browser GUI. **This is a pnpm workspace** — package membership
is defined in `pnpm-workspace.yaml` at the repo root, not here. Run TS/JS tasks
with `corepack pnpm` from the repo root, not inside each package.

## Packages

The packages form three layers — the graph editor, the collab/yjs layer, and the
Electron shell — plus a few grammars and shims.

### GUI and shell

- `gui/` — The main GUI (Vue). Organized by feature subtree: `src/project-view/`
  for the ProjectView feature (graph editor, code editor, visualizations) and
  `src/dashboard/` for the Dashboard feature (auth, cloud, project browser).
  Dashboard is still in React as a historical artifact and is being migrated to
  Vue. Shared UI/infrastructure lives at `src/` directly. See `gui/CLAUDE.md`.
- `electron-client/` — Electron main process. Packages the GUI + backend bundle,
  handles auto-update, file associations, custom protocol (`enso://`). The
  published binary name is just `enso`.
- `common/` — TS utilities shared by both the dashboard and the Electron client
  (access tokens, services, download helpers, i18n text).

### Ydoc (collaborative editing) stack

- `ydoc-shared/` — Shared AST + language-server types + binary protocol.
  Consumed by both client and server halves. Exports `/ast`.
- `ydoc-server/` — Node-side Yjs server: adapts the Enso LS text protocol into
  Yjs CRDT updates, merges edits from multiple clients.
- `ydoc-server-polyglot/` — Same server but bundled (esbuild, CommonJS) to run
  **inside** the GraalVM JVM engine process via GraalJS. Lets the engine host
  the ydoc server without a separate Node runtime.
- `ydoc-channel/` — Small Yjs-based bidirectional message channel library, used
  by both client and server.
- `ydoc-inspect/` — Dev-only proxy that logs Ydoc/LS traffic for debugging
  (`pnpm run dev:inspect`).

### Grammars (CodeMirror/Lezer)

- `lezer-markdown/` — Fork/patch of `@lezer/markdown` with Enso-specific
  extensions.
- `lang-markdown/` — CodeMirror language package wrapping the markdown grammar
  (documentation editor).
- `table-expression/` — Lezer grammar for the table-expression mini-language
  shown in the node inspector.

### Rust bridge

- `rust-ffi/` — `wasm-bindgen` wrapper around `enso-parser` (from
  `lib/rust/parser/`). Built to WASM and consumed by `ydoc-shared` for
  client-side parsing.

### Project Manager

- `project-manager-shim/` — TypeScript Project Manager used by **both dev/watch
  and the packaged Electron build**. Named "shim" for historical reasons — the
  old Scala PM (`lib/scala/project-manager`) is no longer wired into any build,
  and this is now the real thing.

## Build order gotcha

The TS packages declare each other via `workspace:*` references and must be
compiled in dependency order. Root `pnpm compile` (`pnpm run -r compile`)
handles this; don't run individual package `tsc` calls before their deps have
emitted `dist/`.

## Testing

- Unit tests: `vitest` (each package has its own config).
- Integration tests: Playwright. GUI integration tests live in
  `app/gui/integration-test/`; the Electron client has its own at
  `app/electron-client/tests/`.

## Licenses

Everything in this directory is **AGPL-3.0** (`gui/LICENSE`), not Apache like
the engine. Keep this in mind when importing engine code or vendoring
dependencies.
