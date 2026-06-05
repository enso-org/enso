# Standard.Table

A columnar table library for in-memory and database-backed data. Provides
`Table` and `Column` types with filtering, sorting, aggregation, joins, pivots,
and reading/writing CSV/Excel/Parquet.

## Main entry points

- `Table.new` — create a table from `[[name, vector], …]`.
- `Table.from_rows` — create a table from header row + row vectors.
- `Table.from_objects` — build a table from a vector of objects.
- `Table.at` — get a column by name or index.
- `Table.set` — add or update a column.
- `Table.filter` — filter rows by condition (supports `..Equal`, `..Greater`,
  `..Less`, `..Between`, `..Is_In`, … via `Filter_Condition` autoscope).
- `Table.select_columns` / `Table.remove_columns` — pick or drop columns.
- `Table.sort` — sort rows.
- `Table.aggregate` — group rows; `Aggregate_Column` has `..Count`, `..Sum`,
  `..Average`, `..Group_By`, `..First`, … (autoscope).
- `Table.cross_tab` — pivot rows to columns with an aggregation.
- `Table.join` — join two tables; supports inner, left, right, full.
- `Column` — column type with `.map`, `.filter`, `.zip`, etc.

## Common usage

```
table = Table.new [['Name', ['John', 'Paul']], ['Age', [25, 35]]]

filtered = table.filter 'Age' (..Greater 30)

sorted = table.sort ['Age']

grouped = table.aggregate ['Name'] [..Count, ..Sum 'Age']

pivot = (Table.new [['Id', ['A','A']], ['B', ['Name','Country']], ['C', ['Ada','UK']]])
    . cross_tab ['Id'] 'B' (..First 'C')
```

## Layout

- `src/Table.enso` — `Table` type and operations.
- `src/Column.enso` — `Column` type.
- `src/Aggregate_Column.enso` — aggregation specs (`..Count`, `..Sum`, …).
- `src/Value_Type.enso` — column type system.
- `src/Delimited/` — CSV/TSV reading and writing.
- `src/Excel/` — Excel format handling.
- `src/Parquet/` — Parquet support.
- `src/Extensions/` — type-extension methods (e.g. `Vector.to_table`).

## Things to avoid in generated code

- Joins with mismatched key types silently produce mismatch markers — verify
  column types are compatible before joining.

## Where to read more

- `src/Table.enso` — type definition with method docs.
- `src/Aggregate_Column.enso` — list of aggregation constructors.
- `test/Table_Tests/src/In_Memory/` — comprehensive in-memory examples.
- `test/Table_Tests/src/Common_Table_Operations/` — operations that work on both
  in-memory and database tables.
