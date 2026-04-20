# gui/integration-test

Playwright-based integration tests for the GUI. Organized by feature subtree,
mirroring `src/`. Expect the structure to evolve as the Dashboard is ported to
Vue:

- `dashboard/` — Dashboard feature flows (sign-in, project list, settings). The
  Dashboard subtree is still React, legacy.
- `project-view/` — ProjectView feature flows (create nodes, connect edges, open
  visualization).
- `actions/` — Reusable test actions (page-object-style helpers).
- `mock/` — Mocks for the backend, Cognito, feature flags. Dashboard tests run
  against these by default.
- `base.ts` — Playwright base test fixture (provides auth'd page, mocked env,
  debug logging).
- `setup.ts` — Global setup (port finders, temp dirs).

## Run

```
cd app/gui
pnpm test:integration          # headless
pnpm test-dev:integration      # UI mode
```

## Caveats

- `DASHBOARD_TESTS=true` switches the run to a mocked backend (see
  `vite.config.ts`).
- Requires `NODE_OPTIONS='--experimental-wasm-modules'` until Node 24 is default
  (the Rust parser WASM module).
- Install browsers once with `pnpm run playwright:install` (Chromium only).
- CI runs with a matrix of {dashboard, project-view} × {chromium}. Don't add
  cross-suite test IDs — they must stay independent.

## Writing tests

- Use page-object-style helpers from `actions/`, not raw selectors.
- Mock at the boundary (HTTP / Amplify / LS), not inside components.
- Avoid `page.waitForTimeout` — prefer role-based `toBeVisible` / `toHaveText`
  assertions.
