# Standard.Tableau

Tableau Hyper extract (`.hyper`) file format support: read and write Hyper files
for use with Tableau Desktop, Tableau Server, or Tableau Cloud.

## Main entry points

- `Hyper_File` — open a Hyper file by path; lists schemas and tables.
- `Hyper_Table` — a single table inside a Hyper file; supports `read`.
- `Hyper_Column` — column metadata (name, type, nullability).
- `Tableau_Format` — file format SPI; lets `Table.write path …Hyper_File`
  produce a `.hyper` file.

## Common usage

```
import Standard.Tableau.Hyper_File.Hyper_File

hyper = Hyper_File.new "extract.hyper"
schemas = hyper.schemas
tables = hyper.tables

extract = hyper.read "Extract"

namespaced = (Hyper_File.new "extract.hyper").read "MyTable" schema="MySchema"

my_table.write "out.hyper" (..Hyper_File "MySchema" "MyTable")
```

## Layout

- `src/Hyper_File.enso` — open and explore `.hyper` files.
- `src/Hyper_Table.enso` — read individual tables.
- `src/Hyper_Column.enso` — column metadata.
- `src/Tableau_Format.enso` — file format SPI for `Table.write`.
- `src/Hyper_Errors.enso` — error types.

## Things to avoid in generated code

- Reading from `Hyper_File` without first checking that the schema/table exists
  — `Table_Not_Found` errors are unrecoverable mid-workflow.
- Holding an open `Hyper_File` across long-running operations; Tableau's Hyper
  engine is process-bound.

## Where to read more

- `src/Hyper_File.enso`, `src/Hyper_Table.enso` — full method docs.
- `test/Tableau_Tests/src/Read_Spec.enso`, `Write_Spec.enso`,
  `Structure_Spec.enso` — examples.
