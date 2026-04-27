# Enso

Enso Analytics is a visual/textual programming environment for data preparation
and analysis. This repository contains **two top-level products that share this
monorepo**:

- **Enso Engine** — the Enso language implementation: parser, compiler,
  interpreter (Truffle/GraalVM), language server, and the Enso standard library.
  Written in Scala, Java, and Rust.
- **Enso IDE** — a desktop application (Electron) with a visual graph editor and
  a dashboard for project/cloud management. Written in TypeScript; Vue is the
  main UI framework. The Dashboard feature is still in React as a historical
  artifact and is being progressively migrated to Vue.

The two products are glued together by several generated/shared artifacts: the
Rust parser is compiled to **both** WASM (for the GUI) and a JNI `cdylib` (for
the JVM), and its AST types are code-generated into Java so the engine can
deserialize parser output.

## Top-level layout

- `app/` — Desktop app (Electron), GUI (Vue; the Dashboard feature is still
  React while it's being ported), ydoc server, markdown/table CodeMirror
  grammars, Rust→WASM bindings. pnpm monorepo.
- `engine/` — The Enso language engine. Mixed Scala/Java under `sbt`. Runtime
  uses GraalVM Truffle.
- `lib/rust/` — Rust workspace libraries (parser, prelude, reflect/metamodel,
  zst). Some crates ship to crates.io.
- `lib/java/` — JPMS-friendly wrappers around third-party Java libraries, plus
  interpreter DSL, persistance, and ydoc server.
- `lib/scala/` — Scala support libraries (pkg, project-manager, editions,
  logging, etc.).
- `build_tools/` — The `./run` CLI (Rust). Historically the canonical build
  orchestrator; now legacy and being replaced by Bazel. Still in active use for
  many flows.
- `distribution/` — Packaged output layout, templates, and the **Enso standard
  library sources** under `distribution/lib/Standard/`.
- `std-bits/` — Java helpers that the Enso standard library calls via host
  interop.
- `test/` — Enso-language test projects (one per standard library module).
- `tools/` — Auxiliary dev tools (IGV plugin, http test helper, CI scripts,
  benchmark analysis).
- `docs/` — Extensive developer documentation (RFCs, style guides, runtime
  internals, LSP protocol). Start at `docs/README.md`.
- `project/` — SBT plugins and build helpers for the engine.
- `internal/`, `bazel_scripts/`, `toolchains/`, `nix/`, `patches/` — Build
  plumbing.

## Build systems

Four coexist; **which one to use depends on what you're building**:

- **Cargo** — for everything under the `[workspace]` in root `Cargo.toml`
  (build_tools + lib/rust + app/rust-ffi).
- **pnpm + Vite/esbuild** — for the TypeScript monorepo (packages listed in
  `pnpm-workspace.yaml`).
- **SBT** — for the Scala/Java engine (`build.sbt`). See
  `docs/sbt-cheatsheet.md`.
- **Bazel** — the **target** build system. Actively being rolled out across the
  repo; `BUILD.bazel` files live alongside `Cargo.toml` / `package.json`. Prefer
  Bazel targets for new build functionality, and migrate existing flows to Bazel
  when you touch them.

The `./run` (or `run.cmd` / `run.ps1`) script at the repo root dispatches to the
Enso build CLI (in `build_tools/cli/`) — the **legacy** end-to-end orchestrator.
It still works (and transparently calls Bazel when available, Cargo otherwise),
but the plan is to replace it with Bazel targets. Don't extend `./run` with new
functionality unless Bazel can't cover the case yet.

## Languages and toolchains

- Rust: stable channel pinned in `rust-toolchain.toml` (currently 1.90.0) with
  `wasm32-unknown-unknown` target. Workspace `Cargo.toml` pins dependency
  versions — members reference them with `{ workspace = true }`. `rustfmt.toml`
  and `clippy.toml` are repo-wide; follow them.
- Scala/Java: GraalVM-based. See `docs/infrastructure/` for `sbt`, native-image,
  dual-JVM, and GraalVM upgrade guides.
- TypeScript: TS `catalog:` version in `pnpm-workspace.yaml`. Use
  `corepack pnpm` (not bare `pnpm`/`npm`).

## Conventions worth knowing up front

- Root `CHANGELOG.md` is user-facing and PR-linked — add an entry when shipping
  a user-visible change.
- Licensing is split: Engine = Apache-2.0, IDE = AGPL-3.0 (see
  `app/gui/LICENSE`).
- The Rust workspace follows `docs/style-guide/rust.md`; additional house rules
  live in `~/.claude/skills/rust-guidelines` (load it before Rust edits).
- When a module, crate, or package lacks a CLAUDE.md, create one when you start
  working in it — see `~/.claude/skills/project-structure`.

## Cross-cutting gotchas

- The Rust parser is the source of truth for the AST. Changing it means
  regenerating Java bindings (`lib/rust/parser/generate-java/`) and re-bundling
  WASM (`app/rust-ffi/`). The engine and IDE both consume it.
- "Polyglot" has multiple meanings here: (1) GraalVM polyglot — Enso calling
  JS/Python/Java at runtime; (2) Enso Polyglot Bridge (EPB) — an internal
  sub-language for single-threaded language contexts; (3) `ydoc-server-polyglot`
  — the ydoc server bundled to run inside the GraalVM/JVM process. Don't
  conflate them.
- The "standard library" has two halves: pure Enso sources in
  `distribution/lib/Standard/` and Java helpers in `std-bits/`. Adding a new
  stdlib primitive usually touches both.
