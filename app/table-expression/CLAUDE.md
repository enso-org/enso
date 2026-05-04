# lezer-enso-table-expr

Lezer grammar for the **table-expression** mini-language shown in the Enso GUI's
node editor (e.g. filter expressions, calculated column formulas). Not a full
Enso parser — just the small subset used in inline widgets.

## Build

Grammar source is generated via `@lezer/generator` at compile time (see
`package.json` devDependency); the TypeScript wrapper in `src/index.ts` glues it
into a CodeMirror `LanguageSupport`.

## Scope

Keep this grammar minimal. If a new syntactic construct is needed here and the
full Enso parser already supports it, prefer consuming the Rust parser (via
`rust-ffi` / `ydoc-shared/ast`) rather than duplicating grammar logic.
