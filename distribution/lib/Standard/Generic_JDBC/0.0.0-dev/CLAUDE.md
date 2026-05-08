# Standard.Generic_JDBC

Bridge to arbitrary JDBC-compliant databases via raw connection strings.
Use when the target database has no first-class Enso connector (H2, niche
PostgreSQL/MySQL drivers, etc.). Less ergonomic than the dedicated backends
because results materialize to in-memory `Table` rather than deferred
`DB_Table`.

## Main entry points

- `Generic_JDBC_Connection` — the connection type and entry point.
- `Generic_JDBC_Connection.connect url properties=[] quote_char=""` — open a
  connection by JDBC URL.
- `connection.read query` — run a SQL query (`SQL_Query.Raw_SQL …`),
  return an in-memory `Table`.
- `connection.tables` — list available tables.

## Common usage

```
conn = Generic_JDBC_Connection.connect "jdbc:h2:~/my_database"

result = conn.read (..Raw_SQL "SELECT * FROM my_table")

tables = conn.tables
```

## Layout

- `src/Main.enso` — re-exports `Generic_JDBC_Connection`.
- `src/Generic_JDBC_Connection.enso` — connection type and operations.
- `src/Internal/` — **private** implementation details.

## Things to avoid in generated code

- `src/Internal/` paths.
- Anything marked `private: true`.
- Treating the result as a deferred `DB_Table` — `Generic_JDBC` returns
  in-memory `Table`s, so there's no query pushdown.
- Forgetting to close the connection on long-running workflows.
- Assuming case-insensitive identifiers — depends on the underlying driver.

## Where to read more

- `src/Generic_JDBC_Connection.enso` — full type with doc-block examples.
- `test/Generic_JDBC_Tests/src/` — H2 and JDBC connection tests.
