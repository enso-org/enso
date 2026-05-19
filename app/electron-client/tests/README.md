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

Per-prompt budgets are generous (10 min for `aiNode.spec.ts`, 15 min for
`aiChallenges.spec.ts`) because deep-thinking turns on `--effort max` can
genuinely run for several minutes of channel silence (the underlying API does
not surface per-token thinking deltas). Stall detection lives in the main
process: `IDLE_TIMEOUT_MS` (5 min) errors out a turn whose stream-json channel
falls silent for that long, which fails the assertion well before the per-prompt
budget. There is no test-side feedback watchdog — the prompt-failure signal is
enough.

### `aiNode.spec.ts` — quick smoke (~1 min)

Requires the `claude` CLI on `PATH` and authenticated. Set `ENSO_TEST_AI=1`.

```bash
enso> ENSO_TEST_AI=1 corepack pnpm -r --filter enso ide-integration-test tests/aiNode.spec.ts
```

### `aiChallenges.spec.ts` — AI challenge inputs (5–15 min/test)

Long-running e2e tests that drive AI nodes through analytics workflows. Two
flavors:

- **Preppin' Data tests** isolate single capability gaps with prompts that spell
  out value-dependent context.
- **App-demo tests** (Colorado COVID, FX Rates History) describe a business goal
  and verify the agent picks the right multi-step approach on its own.

These tests need the `claude` CLI **and** (for the file-driven tests) the
original inputs downloaded by hand — no fixtures are committed. Source URLs:

- Week 32 — Pokemon Card Organising:
  <https://preppindata.blogspot.com/2024/08/2024-week-32-pokemon-card-organising.html>
- Week 51 — Strictly Positive Improvements:
  <https://preppindata.blogspot.com/2024/12/2024-week-51-strictly-positive.html>
  (input is identical to Challenge 42)
- Colorado COVID — `CDPHE_COVID19_County_Status_Metrics.csv` and
  `ColoradoGeoData.db` (the SQLite holds a `ColoradoLatLong` table) live in the
  local `~/dev/project-templates/Data/` checkout; copy both into the challenge
  directory before running.

Drop the downloaded files into a single directory (flat — no subfolders):

```
$ENSO_TEST_AI_CHALLENGES_DIR/
  Gym Leader Set Cards.xlsx                         # week 32 (sheets: Trainer Cards, Pokemon Cards, Leader Order)
  Pokemon Input.xlsx                                # week 32 (only the `Pokemon` sheet is used)
  strictly_come_dancing_series_1_to_21_tables.csv   # week 51
  CDPHE_COVID19_County_Status_Metrics.csv           # Colorado COVID
  ColoradoGeoData.db                                # Colorado COVID
```

The FX Rates History test doesn't read any local file — it fetches the BoE
exchange-rate CSV over HTTPS at runtime. The env var still gates it (so the test
doesn't fire in default local runs), but the directory can be empty for this
test; the network connection to `bankofengland.co.uk` is the real prerequisite.

Then:

```bash
enso> ENSO_TEST_AI_CHALLENGES_DIR=/abs/path/to/challenge-inputs \
        corepack pnpm -r --filter enso ide-integration-test tests/aiChallenges.spec.ts
```

Per-test skips fire when only some challenges' files are present, so a developer
who has downloaded only week 51 can still run that one. If a vendor publishes
the inputs under different filenames, edit the corresponding `WEEK_32_FILES` /
`WEEK_51_FILES` / `COLORADO_FILES` constant in the spec or rename the local
copy.

#### Effectiveness metrics (optional)

Set `ENSO_AI_CHALLENGES_METRICS_DIR=/abs/path` alongside the dataset env var to
collect per-run AI-effectiveness telemetry. **Every** run (pass OR fail) appends
one CSV row to `<dir>/<sanitized-test-name>.csv`, with totals plus
semicolon-separated per-AI-node breakdowns of duration, input/output tokens, and
running context size. The row's `commit` column carries the current HEAD SHA
when the working tree is clean and the literal `WIP` otherwise, so runs from
in-progress branches are still recorded but won't be confused with clean-tree
benchmarks. Skipped tests (missing inputs) append nothing.

The `status` column reports one of `pass`, `pass (broken)`, `fail`, or
`fail (broken)`. The `(broken)` suffix flags rows where at least one AI-node
sample had its `contextTokens` value derived from the cost-side fallback rather
than the final assistant envelope — the number then overstates real
context-window occupancy. The pass/fail half of the status reflects the
Playwright assertions independently, so a failed run with broken telemetry still
surfaces as a failure for downstream analysis.

#### Comparing models or effort levels

Set `ENSO_AI_CLAUDE_EXTRA_ARGS` to extra flags forwarded verbatim to the spawned
`claude` CLI (whitespace-split, no shell quoting):

```bash
ENSO_TEST_AI_CHALLENGES_DIR=/abs/path/to/challenge-inputs \
ENSO_AI_CHALLENGES_METRICS_DIR=/abs/path/to/metrics \
ENSO_AI_CLAUDE_EXTRA_ARGS="--model claude-sonnet-4-6" \
  corepack pnpm -r --filter enso ide-integration-test \
    tests/aiChallenges.spec.ts
```

The verbatim env-var value is captured in each row's `ai_parameters` column, so
multiple runs with different flag combinations can be grouped and compared in
the same CSV. An empty `ai_parameters` cell means the default model was used.
