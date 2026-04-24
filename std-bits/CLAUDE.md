# std-bits

Java helper libraries that the **Enso standard library** calls via host interop.
Each subdir mirrors a standard-library module name.

## Modules

- `base/` — `Standard.Base`. Helpers for file I/O, HTTP, datetime, regex,
  numeric, text. The biggest and most churn-heavy.
- `table/` — `Standard.Table`. Columnar data operations (joins, aggregations,
  parsers).
- `database/` — `Standard.Database`. JDBC plumbing used by `Generic_JDBC`,
  Postgres, etc.
- `aws/`, `google/`, `microsoft/`, `snowflake/`, `tableau/`, `duckdb/`, `saas/`
  — SDK integrations.
- `image/` — OpenCV-backed image helpers (pair with `lib/java/opencv-wrapper`).
- `benchmarks/` — Benchmark harness helpers used by
  `engine/runtime-benchmarks/`.
- `tests/` — Shared test utilities reused by `test/*_Tests`.

## How Enso calls it

The standard library uses Enso's polyglot-Java host interop (`Polyglot.import`)
to reach these classes at runtime. Adding a method here makes it immediately
callable from the matching `distribution/lib/Standard/<Module>/` sources — no
Truffle wiring required. To call these, use `polyglot java import ` syntax.

## Shape

- Plain Java modules (JPMS). Some use `module-info.java`.
- No Truffle / Graal imports allowed — these are pure host-side libraries.
- Keep APIs small and typed — the Enso side calls them by name, so refactoring
  here is expensive.

## Changing an API

A rename or signature change silently breaks the standard library (no
compile-time check across the host/polyglot boundary). Always grep the matching
`distribution/lib/Standard/<Module>/` directory for callers, and run the
matching `test/<Module>_Tests` suite before merging.

## Test

Each subdir ships a matching test project under `test/` (e.g. `Base_Tests`,
`Table_Tests`). Run with
`sbt 'runEngineDistribution --run test/<Module>_Tests'`.
