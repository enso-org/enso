# lib/rust

Rust workspace libraries, declared as members of the root `Cargo.toml`
workspace. These are general-purpose Rust building blocks — the "tools" layer
(`build_tools/`) and the parser-integration shim (`app/rust-ffi/`) live
elsewhere but share the same workspace.

## Crates

- `parser/` — The Enso **parser**. Produces AST for the whole toolchain. Has
  subcrates for JNI bindings, Java codegen, schema generation, fuzzing, and
  proc-macros.
- `prelude/` — Haskell-style prelude pulling `derive_more`, re-exports, and
  idiom helpers. Crate name: `enso-prelude`. Published to crates.io.
- `reflect/` + `reflect/macros/` — Runtime reflection over Rust types. Feeds the
  metamodel.
- `metamodel/` + `metamodel/lexpr/` — Language-agnostic description of the data
  types exposed via `reflect`. The `java` feature powers parser Java codegen;
  `lexpr` produces S-expression serialization (used by parser debug tooling).
- `zst/` — Zero-sized type newtype utilities, clearer replacement for bare
  `PhantomData`. Published.
- `macro-utils/` — `proc-macro2`/`syn`/`quote` helpers. Published.
- `launcher-shims/` — Tiny proxy binaries used only by the launcher self-upgrade
  test suite.

## Conventions

- All crates inherit workspace lints (`[lints] workspace = true`) and the
  workspace rustfmt (`rustfmt.toml`).
- Crate names use `enso-` prefix when publishable (`enso-parser`,
  `enso-prelude`, `enso-reflect`, `enso-zst`, `enso-macro-utils`,
  `enso-metamodel`).
- Edition `2024` everywhere. Workspace pins dependency versions; members
  reference them with `{ workspace = true }`.
- Follow the Enso Rust style guide in `docs/style-guide/rust.md` and the
  stricter house rules under the `rust-guidelines` skill.

## Adding a crate

1. Create `lib/rust/<name>/` with a `Cargo.toml` inheriting workspace lints.
2. Add the path to `[workspace].members` in root `Cargo.toml`.
3. Create a sibling `CLAUDE.md` (required — see rust-guidelines skill).
4. If it's publishable, set `publish = true` and pick a stable, concrete
   description.
