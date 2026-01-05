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
