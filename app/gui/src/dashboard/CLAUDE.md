# dashboard/ (React)

The React half of the IDE: sign-up/sign-in, cloud project browser, settings, billing, subscription management. Import this half via the `#/` path alias.

## Structure

- `pages/` — Route-level components. Top of the component tree for each URL.
- `layouts/` — Chromes that wrap multiple pages (protected-route wrappers, split panels).
- `modules/` — Feature-oriented slices (`payments/` for Stripe flows, etc.). A module owns its state and components.
- `components/` — Reusable UI atoms/molecules. Sub-folders group related parts (`Button/`, `Form/`, `Dialog/`, `Menu/`). The `aria/` folder re-exports `react-aria-components` with project-level styling applied.
- `providers/` — React context providers (auth, text/i18n, modals, toasts, etc.).
- `hooks/` — Custom React hooks.
- `configurations/` — Static/derived configuration (feature flags, endpoints, subscription tiers).
- `data/` — Client-side data-access helpers (React Query query/mutation factories for the Enso Cloud API).
- `modals/` — Global-modal registry and renderer.
- `utilities/` — Pure TS helpers (no React).
- `styles.css`, `tailwind.css`, `typings.d.ts` — Tailwind plus global resets.

## Conventions

- **UI lib**: `react-aria-components` for accessibility primitives, `tailwind-variants` and `tailwind-merge` for class composition, `tailwindcss-react-aria-components` for matching selectors.
- **Forms**: `react-hook-form` + `zod` resolvers. Schemas live with the form, not in `data/`.
- **Async/data**: `@tanstack/react-query` throughout. Keys, queries, and mutations should go through the factories in `data/` — don't call `useQuery` with a literal key inside a component.
- **Routing**: `vue-router` (yes, really — the dashboard lives inside a Vue shell; React components consume routing via the bridge).
- **Error boundaries**: wrap new features with `ErrorBoundary` from `components/`; don't catch errors with try/catch for render-time failures.
- **Styling**: Tailwind + CSS nesting (enabled via `postcss-nesting`). Prefer class utilities over ad-hoc CSS; if you need a component class, use `tailwind-variants`.

## Auth / cloud

The dashboard authenticates against AWS Cognito via `aws-amplify`. Session tokens are stored via `accessToken.ts` in `app/common/` and mirrored into the Project Manager / LS so the engine can reach Enso Cloud.

## Talking to the project-view

This half never imports from `@/` (the Vue half). When it needs to open a project, it calls into shared providers under `$/providers/` (the `src/providers/` folder outside either half), and the Vue half subscribes.

## Tests

- Unit: `vitest` + `@testing-library/react`.
- Integration: Playwright specs in `app/gui/integration-test/dashboard/`.
