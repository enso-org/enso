# enso-prelude

A Haskell-style prelude for Enso's Rust code. Pulls in `derive_more`,
`derive-where`, `serde`, and a set of re-exports that the rest of our Rust
modules rely on. Most files start with `use enso_prelude::*;` (prelude-style
star import is explicitly allowed by the rust-guidelines).

Published to crates.io (`publish = true`), so public-API changes need a version
bump. `publish` doesn't mean bleeding-edge — be conservative about exports.

## Subcrate

- `macros/` — `enso-macros`, the proc-macro half of the prelude (derives and
  helpers that can't live in a `rlib`).

## Contents at a glance

- Re-exports: `std::collections`, common `serde` traits, `Rc`/`Arc` helpers.
- `derive_more` / `derive-where` re-exports so consumers don't have to import
  them directly.
- Small ergonomic traits (`Into*` / `Map*` style helpers).

When adding an export, prefer making it unambiguous — prelude imports should
never shadow `std` with subtly different behavior.
