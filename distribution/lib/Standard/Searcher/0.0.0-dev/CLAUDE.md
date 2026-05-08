# Standard.Searcher

Hand-curated category metadata for the Enso IDE's component searcher (the "add
new component" palette). Contains documentation entries with `## Examples`
blocks describing common workflows; methods themselves are typically
`Unimplemented` because the IDE only consumes the docstrings, never the runtime
behavior.

**End-user workflows should not import this library**, and generated code should
never call into it.

## Main entry points (IDE metadata only)

- `Main.text_input`, `Main.input_number`, `Main.table_input` — placeholders for
  "create text/number/table" categories.
- `Data_Science.*`, `Network.*`, `System.*` — category groupings.

These are markers for the IDE; their bodies typically panic with
`Unimplemented`.

## Common usage

There is none for user code. The IDE reads `## Examples` doc blocks here to
populate searcher categories.

## Layout

- `src/Data_Science.enso`, `src/Network.enso`, `src/System.enso` — themed
  category modules.

## Things to avoid in generated code

- **Do not import `Standard.Searcher`.** It exposes no usable runtime behavior.
- Do not call any function from this library — they panic at runtime.

## Where to read more

- `src/Main.enso` — entry-point categories.
