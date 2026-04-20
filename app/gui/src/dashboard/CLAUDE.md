# dashboard/

The **Dashboard feature** subtree: sign-up/sign-in, cloud project browser,
settings, billing, subscription management. Everything whose meaning is tied to
the Enso Cloud shell (before a project is opened, or alongside it) lives here.
Import via the `#/` path alias.

**Currently implemented in React, legacy.** Vue is the main GUI framework; this
subtree was built as an independent React effort and is being progressively
migrated to Vue. Prefer porting a component to Vue over extending it here. New
features default to Vue.

Once the migration completes, this directory is expected to hold only
Dashboard-specific Vue code — common UI primitives / utilities should live at
`src/` directly (see the sibling project-view structure for the same principle).

## Structure (React, legacy shape)

- `pages/` — Route-level components. Top of the component tree for each URL.
- `layouts/` — Chromes that wrap multiple pages (protected-route wrappers, split
  panels).
- `modules/` — Feature-oriented slices (`payments/` for Stripe flows, etc.). A
  module owns its state and components.
- `components/` — Reusable UI atoms/molecules. Sub-folders group related parts
  (`Button/`, `Form/`, `Dialog/`, `Menu/`). The `aria/` folder re-exports
  `react-aria-components` with project-level styling applied. Truly shared UI
  will move to `src/` proper as it's ported.
- `providers/` — React context providers (auth, text/i18n, modals, toasts,
  etc.).
- `hooks/` — Custom React hooks.
- `configurations/` — Static/derived configuration (feature flags, endpoints,
  subscription tiers).
- `data/` — Client-side data-access helpers (React Query query/mutation
  factories for the Enso Cloud API).
- `modals/` — Global-modal registry and renderer.
- `utilities/` — Pure TS helpers (no React).
- `styles.css`, `tailwind.css`, `typings.d.ts` — Tailwind plus global resets.

## Conventions (current React stack)

- **UI lib**: `react-aria-components` for accessibility primitives,
  `tailwind-variants` and `tailwind-merge` for class composition,
  `tailwindcss-react-aria-components` for matching selectors.
- **Forms**: `react-hook-form` + `zod` resolvers. Schemas live with the form,
  not in `data/`.
- **Async/data**: `@tanstack/react-query` throughout. Keys, queries, and
  mutations should go through the factories in `data/` — don't call `useQuery`
  with a literal key inside a component.
- **Routing**: `vue-router` (yes, really — the dashboard lives inside a Vue
  shell; React components consume routing via the bridge).
- **Error boundaries**: wrap new features with `ErrorBoundary` from
  `components/`; don't catch errors with try/catch for render-time failures.
- **Styling**: Tailwind + CSS nesting (enabled via `postcss-nesting`). Prefer
  class utilities over ad-hoc CSS; if you need a component class, use
  `tailwind-variants`.

## Auth / cloud

The Dashboard authenticates against AWS Cognito via `aws-amplify`. Session
tokens are stored via `accessToken.ts` in `app/common/` and mirrored into the
Project Manager / LS so the engine can reach Enso Cloud.

## Talking to ProjectView

This subtree never imports from `@/` (the ProjectView subtree). Cross-subtree
wiring goes through shared providers under `src/providers/` (the `$/providers/`
alias), and each side subscribes.

## Tests

- Unit: `vitest` + `@testing-library/react`.
- Integration: Playwright specs in `app/gui/integration-test/dashboard/`.
