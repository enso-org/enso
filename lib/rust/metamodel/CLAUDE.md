# enso-metamodel

Language-agnostic description of Rust data types. Works in tandem with
`enso-reflect`: reflected type descriptions are inserted into a metamodel, then
projected onto a target language.

## Targets (features)

- `java` — emit Java class/interface hierarchy that matches the Rust type graph
  (used by `enso-parser-generate-java`).
- `rust` — round-trip to Rust sources (used for testing and tooling).
- `graphviz` — Graphviz rendering of the type graph.

Features are additive — callers turn on only what they need. `default = []`.

## Subcrate

- `lexpr/` — `enso-metamodel-lexpr`. S-expression (via the `lexpr` crate)
  serialization of metamodel-described values. Used by the parser debug CLI to
  print AST dumps a human can read.
