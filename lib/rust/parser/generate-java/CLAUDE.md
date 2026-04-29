# enso-parser-generate-java

Binary that reads the Enso parser's AST type definitions (via `enso-reflect`),
projects them through `enso-metamodel`'s Java backend, and emits Java classes +
deserializers that the engine uses to consume parser output.

## When to run

After **any** change to the parser AST in `../src/syntax/`. The engine will not
compile against the old generated Java otherwise. SBT typically triggers it
automatically — check `project/Cargo.scala` if the invocation looks wrong.

## How it works

- `enso-parser` with `features = ["debug"]` exposes type metadata at runtime.
- `enso-reflect` walks the `#[derive(Reflect)]` tree.
- `enso-metamodel` (with `java` feature) translates that into Java-shaped
  classes.
- This crate orchestrates the walk and writes files.

Graphviz output is possible via `enso-reflect/graphviz` (useful for visualizing
the AST graph during refactors).
