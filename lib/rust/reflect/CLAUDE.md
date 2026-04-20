# enso-reflect

Compile-time derivable type reflection. A `#[derive(Reflect)]` produces a
runtime description of a Rust type (name, variants, fields, generics) that
`enso-metamodel` can consume.

Primary client: **the parser**. The parser's AST is derived `Reflect`, and that
description drives both the Java code generator
(`lib/rust/parser/generate-java/`) and the schema generator
(`lib/rust/parser/schema/`).

## Features

- `graphviz` — re-exports `enso-metamodel/graphviz` so consumers can render a
  type's reflected graph.
- The derive is always on (nothing to gate behind a feature).

## Subcrate

- `macros/` — `enso-reflect-macros`, the `#[derive(Reflect)]` implementation.
