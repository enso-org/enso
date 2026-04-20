# enso-parser-syntax-tree-visitor

Proc-macro crate that generates visitor boilerplate for the parser's `Tree`
enum. Gated behind the `debug` feature of `enso-parser`.

Emits `VisitMut`/`Visit`-style traits so downstream tooling (pretty-printers,
the Java/TS code generators, analysis passes) can traverse the AST without
hand-maintaining N match arms.

This crate sits deep under `parser/src/syntax/tree/` because it's conceptually
part of the parser's tree, but it's a separate workspace member so it can
compile first (proc-macros must).
