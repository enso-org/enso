# Standard.Snowflake

Snowflake cloud data warehouse backend for `Standard.Database`. Supports
username/password and key-pair authentication; queries and bulk-load.

## Main entry points

- `Snowflake_Details` — `Database.connect` argument:
  `Snowflake_Details.Snowflake account credentials database schema warehouse`.
- `Snowflake_Connection` — connection type returned by `Database.connect`.
- `Key_Pair_Credentials` — private-key authentication helper for `credentials=`.
- `Snowflake_Data_Link` — persisted data-link reference.
- `File_Format` — Snowflake-specific file format options.

## Common usage

```
from Standard.Snowflake import Snowflake_Details

creds = Credentials.Username_And_Password "user" (Enso_Secret.get "snowflake_password")
details = Snowflake_Details.Snowflake account="my_account" credentials=creds database="MY_DB" schema="PUBLIC" warehouse="COMPUTE_WH"
conn = Database.connect details

db_table = conn.query "ORDERS"
in_mem = db_table.read

t = Table.new [["col1", [1, 2, 3]]]
t.select_into_database_table conn "MY_NEW_TABLE"
```

## Layout

- `src/Snowflake_Connection.enso` — connection type.
- `src/Connection/Snowflake_Details.enso` — connection constructor.
- `src/Connection/Key_Pair_Credentials.enso` — key-pair auth.
- `src/File_Format.enso` — Snowflake-specific options.
- `src/Errors.enso` — Snowflake error types.

## Things to avoid in generated code

- Forgetting the `warehouse` parameter when the account uses multiple
  warehouses; without it queries either fail or hit a default that isn't what
  you want.
- Treating Snowflake identifiers as case-insensitive — they are folded to
  uppercase by default; quote names that must preserve case.

## Where to read more

- `src/Connection/Snowflake_Details.enso` — connection options.
- `test/Snowflake_Tests/src/Snowflake_Spec.enso` — integration test suite.
- `distribution/lib/Standard/Database/CLAUDE.md` — generic DB API.
