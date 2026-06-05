# Standard.Visualization

Preprocessors and helpers used by the Enso IDE to render values as charts,
tables, geo maps, etc. The IDE invokes these automatically — **user code should
not import this library**. The agent should not generate code that imports from
it either.

## Main entry points (IDE-facing only)

- `Preprocessor.default_preprocessor value` — IDE entry: encode an arbitrary
  value as JSON for visualization.
- `Geo_Map.process_to_json_text value` — IDE entry for the geo-map widget.
- `Histogram.process_to_json_text value`,
  `Scatter_Plot.process_to_json_text value` — IDE entries for chart widgets.
- `Table.<helpers>` — lazy/chunked rendering helpers for the IDE table widget.
- `AI.<helpers>` — prompt-building utilities for the IDE's AI assistant.

## Common usage

There is none for user code. The IDE calls these functions; user-defined
components and workflows should not.

## Layout

- `src/Preprocessor.enso` — default visualization preprocessor.
- `src/Helpers.enso` — display utilities (truncation, formatting, timing).
- `src/Table.enso` — IDE-side table rendering.
- `src/Geo_Map.enso`, `src/Histogram.enso`, `src/Scatter_Plot.enso` —
  per-visualization preprocessors.
- `src/AI.enso` — AI assistant prompt construction.
- `src/Widgets.enso`, `src/Id.enso`, `src/Warnings.enso`, `src/File_Upload.enso`
  — supporting IDE utilities.

## Things to avoid in generated code

- **Do not import `Standard.Visualization` in user code.** It is internal to the
  IDE; the JSON shape is not stable.
- Do not call `Preprocessor.*` or `*.process_to_json_text` — the IDE calls them
  on your behalf when a node is visualized.
- Do not depend on the JSON encoding produced; treat it as IDE-internal.

## Where to read more

- `src/Main.enso` and `src/Preprocessor.enso` — entry points.
- `test/Visualization_Tests/src/` — IDE-side regression tests.
