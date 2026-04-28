# test/

Enso-language test suites. Each subdir is an **Enso project** (has its own
`package.yaml`) that exercises a specific standard-library module or
cross-cutting concern.

## Layout

One directory per suite, typically:

```
<Module>_Tests/
├── package.yaml
├── src/
│   └── Main.enso      # test entry point
└── data/              # fixtures (optional)
```

## Suites

- `Base_Tests/`, `Base_Internal_Tests/` — `Standard.Base`. Requires
  `http-test-helper/` running (see `Base_Tests/README.md`).
- `Table_Tests/`, `Table_Internal_Tests/` — `Standard.Table`.
- `AWS_Tests/`, `Snowflake_Tests/`, `Microsoft_Tests/`, `Google_Test/`,
  `Tableau_Tests/`, `DuckDB_Tests/`, `Generic_JDBC_Tests/`, `Cloud_Tests/`,
  `Saas_Tests/` — provider suites. Most need live credentials via env vars.
- `Geo_Tests/`, `Image_Tests/` — media suites.
- `Examples_Tests/` — runs the `Standard.Examples` workflows and asserts their
  outputs.
- `Visualization_Tests/` — exercises the visualization builtins.
- `Test_Tests/` — the testing framework tests itself.
- `Benchmarks/` — standard-library microbenchmarks.
- `Helpers/` — shared fixture utilities imported by the other suites.

## Running

```
# single suite
sbt 'runEngineDistribution --run test/Base_Tests'

# with env
sbt 'runEngineDistribution --env KEY=VAL --run test/Base_Tests'
```

Some suites require external services (databases, cloud credentials). Their
READMEs list what's needed. Expect most providers to gate on environment
variables — skipped tests are visible in the report, not silently dropped.

## Adding tests

1. Import the relevant `Standard.*` module.
2. Use `Standard.Test.Bundles`/`Test_Suite` to declare groups.
3. Add assertions via `Test.specify` / `Test.expect`.

## Changing Java helpers

Every change to `std-bits/<module>/` should run the matching
`test/<Module>_Tests` — the polyglot boundary has no compile-time check.
