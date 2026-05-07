# Playwright tests

The `test` directory contains a set of end-to-end integration tests written
using [Playwright Test](https://playwright.dev/) framework.

## Prerequisite

Running end-to-test tests requires a locally build IDE, that should be present
in `dist/ide` directory.

### Test account

In order to run tests locally you have to create credentials file under
`app/electron-client/playwright/.auth/user.json`:

```bash
enso> mkdir -p app/electron-client/playwright/.auth && touch app/electron-client/playwright/.auth/user.json && chmod 600 app/electron-client/playwright/.auth/user.json && echo "{\"user\": \"$ENSO_TEST_USER\",\"password\":\"$ENSO_TEST_PASS\"}" > app/electron-client/playwright/.auth/user.json
```

The test account with `$ENSO_TEST_USER`/`$ENSO_TEST_PASS` credentials should be
created **before** running the test suite.

### Playwright

```bash
enso> corepack pnpm install
enso> corepack pnpm exec playwright install
```

## Running end-to-end testing

```bash
enso> corepack pnpm -r --filter enso ide-integration-test
```

will run a full suite of end-to-end tests.

### Debugging

Debugging is possible by adding a `--debug` flag:

```bash
enso> corepack pnpm -r --filter enso ide-integration-test --debug
```

The command will start the usual electron app but with the possibility to
pause/continue and add breakpoints.

### Selective tests

It is possible to run only a specific test suite by including its name at the
end of the arguments.

```bash
enso> corepack pnpm -r --filter enso ide-integration-test tests/gettingStarted.spec.ts
```

## AI tests

Two AI-driven specs are gated on env vars and skipped silently otherwise.

### `aiNode.spec.ts` — quick smoke (~1 min)

Requires the `claude` CLI on `PATH` and authenticated. Set `ENSO_TEST_AI=1`.

```bash
enso> ENSO_TEST_AI=1 corepack pnpm -r --filter enso ide-integration-test tests/aiNode.spec.ts
```

### `aiChallengePrep.spec.ts` — Preppin' Data challenge inputs (5–15 min/test)

Long-running e2e tests that drive AI nodes through a full Preppin' Data
challenge.

These tests need the `claude` CLI **and** the original challenge inputs
downloaded by hand (no fixtures are committed). Source URLs:

- Week 32 — Pokemon Card Organising:
  <https://preppindata.blogspot.com/2024/08/2024-week-32-pokemon-card-organising.html>
- Week 51 — Strictly Positive Improvements:
  <https://preppindata.blogspot.com/2024/12/2024-week-51-strictly-positive.html>
  (input is identical to Challenge 42)

Drop the downloaded files into a single directory (flat — no subfolders):

```
$ENSO_TEST_AI_CHALLENGES_DIR/
  Gym Leader Set Cards.xlsx          # week 32 (sheets: Trainer Cards, Pokemon Cards, Leader Order)
  Pokemon Input.xlsx                 # week 32 (only the `Pokemon` sheet is used)
  strictly_come_dancing_series_1_to_21_tables.csv   # week 51
```

Then:

```bash
enso> ENSO_TEST_AI_CHALLENGES_DIR=/abs/path/to/preppin-data \
        corepack pnpm -r --filter enso ide-integration-test tests/aiChallengePrep.spec.ts
```

Per-test skips fire when only one challenge's files are present, so a developer
who has downloaded only week 51 can still run that one. If Preppin' Data
publishes the inputs under different filenames, edit the `WEEK_32_FILES` /
`WEEK_51_FILES` constants in the spec or rename the local copy.

#### Effectiveness metrics (optional)

Set `ENSO_AI_CHALLENGES_METRICS_DIR=/abs/path` alongside the dataset env var to
collect per-run AI-effectiveness telemetry. On a **successful** test the spec
appends one CSV row per run to `<dir>/<sanitized-test-name>.csv`, with totals
plus semicolon-separated per-AI-node breakdowns of duration, input/output
tokens, and running context size. The row's `commit` column carries the current
HEAD SHA when the working tree is clean and the literal `WIP` otherwise, so runs
from in-progress branches are still recorded but won't be confused with
clean-tree benchmarks. Failed tests append nothing.
