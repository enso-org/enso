# Standard.Database

SQL abstraction for relational databases. Returns a deferred `DB_Table` (query
plan, not data) that mirrors the in-memory `Table` API; call `.read` to
materialize. Backends (SQLite, Postgres, DuckDB, Snowflake, SQL Server, …) ship
in their own libraries — `Standard.DuckDB`, `Standard.Snowflake`,
`Standard.Microsoft`, etc.

## Main entry points

- `Database.connect <details>` — open a connection. `<details>` is a backend
  type, e.g. `DuckDB.In_Memory`, `Postgres.Postgres host port db creds`,
  `SQLServer_Details.SQLServer …`.
- `connection.tables` — list tables in the connection.
- `connection.query "table_name"` — get a deferred `DB_Table` for an existing
  table.
- `connection.query (..Raw_SQL "SELECT …")` — run raw SQL.
- `db_table.read` — materialize a deferred table to in-memory `Table`.
- `db_table.filter`, `.sort`, `.aggregate`, `.join`, `.set`, `.at` — same
  interface as in-memory `Table`; the operations build SQL.
- `db_table.select_into_database_table connection "name"` — bulk-load in-memory
  rows into the database.

## Common usage

```
conn = Database.connect DuckDB.In_Memory

db_table = conn.query "employees"
filtered = db_table.filter 'department' (..Equal "Sales")
in_memory = filtered.read

count = db_table.aggregate ['department'] [..Count]

raw = conn.query (..Raw_SQL "SELECT id, name FROM users WHERE active = true")
```

## Layout

- `src/Connection/` — `Connection`, connection options, credentials.
- `src/DB_Table.enso` — deferred table type.
- `src/DB_Column.enso` — deferred column type.
- `src/SQL_Query.enso` — query construction (`..Table_Name`, `..Raw_SQL`).
- `src/SQL_Statement.enso` — prepared SQL statement.
- `src/Dialect.enso` — dialect dispatch (each backend implements one).

## Things to avoid in generated code

- Assuming all backends support the same SQL — operations that work on one
  backend may fail on another (verify by looking at the backend library's test
  suite).
- Reaching into raw SQL when the high-level API would do — generated SQL is
  dialect-aware, raw SQL is not.

## Where to read more

- `src/Connection/Connection.enso` — connection methods.
- `src/DB_Table.enso` — deferred table operations.
- `test/Table_Tests/src/Common_Table_Operations/` — the same suite runs against
  both in-memory and database backends.
- `test/DuckDB_Tests/`, `test/Snowflake_Tests/`, `test/Microsoft_Tests/` —
  backend-specific examples.
