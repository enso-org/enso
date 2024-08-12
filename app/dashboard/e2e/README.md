# End-to-end tests

## Running tests

Execute all commands from the parent directory.

```sh
# Run tests normally
pnpm run test:e2e
# Open UI to run tests
pnpm run test:e2e:debug
# Run tests in a specific file only
pnpm run test:e2e -- e2e/file-name-here.spec.ts
pnpm run test:e2e:debug -- e2e/file-name-here.spec.ts
# Compile the entire app before running the tests.
# DOES NOT hot reload the tests.
# Prefer not using this when you are trying to fix a test;
# prefer using this when you just want to know which tests are failing (if any).
PROD=1 pnpm run test:e2e
PROD=1 pnpm run test:e2e:debug
PROD=1 pnpm run test:e2e -- e2e/file-name-here.spec.ts
PROD=1 pnpm run test:e2e:debug -- e2e/file-name-here.spec.ts
```

## Getting started

```ts
test.test('test name here', ({ page }) =>
  actions.mockAllAndLogin({ page }).then(
    // ONLY chain methods from `pageActions`.
    // Using methods not in `pageActions` is UNDEFINED BEHAVIOR.
    // If it is absolutely necessary though, please remember to `await` the method chain.
    // Note that the `async`/`await` pair is REQUIRED, as `Actions` subclasses are `PromiseLike`s,
    // not `Promise`s, which causes Playwright to output a type error.
    async ({ pageActions }) => await pageActions.goTo.drive(),
  ),
)
```

### Perform arbitrary actions (e.g. actions on the API)

```ts
test.test('test name here', ({ page }) =>
  actions.mockAllAndLogin({ page }).then(
    async ({ pageActions, api }) =>
      await pageActions.do(() => {
        api.foo()
        api.bar()
        test.expect(api.baz()?.quux).toEqual('bar')
      }),
  ),
)
```
