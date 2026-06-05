# Standard.Examples

Example values and fixtures used by `## Examples` doc blocks across the stdlib.
End-user workflows rarely import this — its purpose is to make doc examples
runnable.

## Main entry points

- `Examples.csv`, `Examples.xls`, `Examples.xlsx`, `Examples.xlsb` — sample
  spreadsheet files (return `File` references).
- `Examples.json`, `Examples.json_object` — example JSON values.
- `Examples.image` — sample image file (used by `Standard.Image` doc blocks).
- `Examples.inventory_table`, `Examples.popularity_table`,
  `Examples.transactions_table` — sample tables.
- `Examples.integer_column`, `Examples.text_column_1` — sample columns.
- `Examples.Example_Error_Type`, `Examples.My_Error`, `Examples.throw_error`,
  `Examples.throw_panic` — example error fixtures.

## Common usage

```
import Standard.Examples

table = Examples.inventory_table

img = Image.read Examples.image

doc_value = Examples.json
```

## Layout

- `src/Main.enso` — defines and re-exports every example value (this library
  exposes everything from `Main.enso`, with no separate top-level files).
- `data/` — sample CSV, XLSX, XLSB files and other static fixtures.

## Things to avoid in generated code

- Importing `Standard.Examples` in production workflows — the example data files
  are shipped with the engine but are not intended as a stable dataset.
- Mutating example values — they are shared across many doc blocks.

## Where to read more

- `src/Main.enso` — full list of example values with their definitions.
- `test/Examples_Tests/src/Examples_Spec.enso` — verifies the examples work.
