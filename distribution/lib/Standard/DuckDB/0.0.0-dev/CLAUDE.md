# Standard.DuckDB

In-process analytical database backend for `Standard.Database`. Use for fast
local SQL on in-memory or on-disk DuckDB databases, plus native Parquet
read/write.

## Main entry points

- `DuckDB.In_Memory` — `Database.connect` argument for an ephemeral in-memory
  database.
- `DuckDB.From_File path` — `Database.connect` argument for an on-disk
  `.duckdb` file.
- `DuckDB_Connection` — the connection type returned by `Database.connect`.
- `DuckDB_Format` — file-format SPI for `.duckdb` files.
- `DuckDB_GeoFormat` — spatial file-format support.
- `Parquet_Format` — read/write Parquet files via `Data.read` / `Table.write`.

## Common usage

```
conn = Database.connect DuckDB.In_Memory

t = Table.new [['x', [1, 2, 3]], ['y', ['a','b','c']]]
db_t = t.select_into_database_table conn "my_table"

q = conn.query "my_table"
filtered = q.filter 'x' (..Greater 1)
result = filtered.read

file_conn = Database.connect (DuckDB.From_File "/tmp/mydb.duckdb")
```

## Layout

- `src/Main.enso` — public API re-exports.
- `src/DuckDB.enso` — `DuckDB.In_Memory`, `DuckDB.From_File`.
- `src/DuckDB_Connection.enso` — connection type.
- `src/DuckDB_Format.enso` — file format.
- `src/DuckDB_GeoFormat.enso` — spatial file format.
- `src/File_Formats/` — Parquet and other format handlers.
- `src/Internal/` — **private**; dialect, type mappings.

## Things to avoid in generated code

- `src/Internal/` paths.
- Anything marked `private: true`.
- Assuming an `In_Memory` database persists — close the connection and the
  data is gone. Use `From_File` for persistence.
- Mixing in-memory `Table` and `DB_Table` operations expecting them to
  interoperate without `.read` / `.select_into_database_table` boundaries.

## Where to read more

- `src/DuckDB.enso` — connection constructors with doc-block examples.
- `test/DuckDB_Tests/src/DuckDB_Spec.enso` — full test suite.
- `distribution/lib/Standard/Database/CLAUDE.md` — `DB_Table` semantics.
