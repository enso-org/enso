# End-to-end tests

## Running tests

Execute all commands from the parent directory.

```sh
# Run tests normally
npm run test:e2e
# Open UI to run tests
npm run test:e2e:debug
# Run tests in a specific file only
npm run test:e2e -- e2e/file-name-here.spec.ts
npm run test:e2e:debug -- e2e/file-name-here.spec.ts
# Compile the entire app before running the tests.
# DOES NOT hot reload the tests.
# Prefer not using this when you are trying to fix a test;
# prefer using this when you just want to know which tests are failing (if any).
PROD=1 npm run test:e2e
PROD=1 npm run test:e2e:debug
PROD=1 npm run test:e2e -- e2e/file-name-here.spec.ts
PROD=1 npm run test:e2e:debug -- e2e/file-name-here.spec.ts
```

## Getting started

```ts
test.test("test name here", ({ page }) =>
  actions.mockAllAndLogin({ page }).then(
    // ONLY chain methods from `pageActions`.
    // Using methods not in `pageActions` is UNDEFINED BEHAVIOR.
    // If it is absolutely necessary though, please remember to `await` the method chain.
    ({ pageActions }) => pageActions.goToHomePage(),
  ),
);
```

### Perform arbitrary actions (e.g. actions on the API)

```ts
test.test("test name here", ({ page }) =>
  actions.mockAllAndLogin({ page }).then(({ pageActions, api }) =>
    pageActions.do(() => {
      api.foo();
      api.bar();
      test.expect(api.baz()?.quux).toEqual("bar");
    }),
  ),
);
```
