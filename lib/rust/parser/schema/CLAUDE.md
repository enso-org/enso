# enso-parser-schema

Serializes the parser's AST metamodel (obtained via `enso-reflect`) to a JSON
schema describing every type, variant, and field. Downstream consumers (notably
`app/ydoc-shared/parser-codegen/`) read this schema to generate TypeScript types
for the AST produced by the WASM parser build.

Keep this binary deterministic — changes in its output should reflect real AST
changes, not refactoring noise. Consumers often diff the schema to detect
breaking changes.
