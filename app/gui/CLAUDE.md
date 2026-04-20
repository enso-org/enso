# app/gui

The main Enso IDE GUI — a single-page web app built with Vite, served by Electron (desktop) or a static host (cloud). Published as package `enso-gui`.

## The Vue/React split (non-obvious)

`src/` contains **two separate SPAs mounted side-by-side**:

- `src/dashboard/` — **React**. Auth, cloud storage, project browser, settings, billing. Uses `react-aria`, `@tanstack/react-query`, `react-hook-form`, `zod`. TailwindCSS for styling.
- `src/project-view/` — **Vue 3**. Graph editor, component browser, code editor, visualizations, documentation editor. Uses `@vueuse/core`, `@tanstack/vue-query`, `yjs`.

They are bridged by **`veaury`**, which lets a Vue component embed a React tree (and vice versa). When editing, stay in one framework per file — don't mix unless you're at the bridge boundary.

`src/components/` at the top of `src/` holds the small set of components that are rendered *outside* either SPA (app container, command palette, loading screen). `App.vue` / `ReactRoot.tsx` / `entrypoint.ts` wire the two together.

## TypeScript path aliases

Defined in `vite.config.ts`:
- `@/…` → `src/project-view/…` (Vue side)
- `#/…` → `src/dashboard/…` (React side)
- `$/…` → `src/…` (reach either side or the shared `src/components/`)

Importing across the boundary (`@/` from `src/dashboard/`, or `#/` from `src/project-view/`) is allowed but suggests you should be using the `veaury` bridge instead.

## Entry points

- `index.html` → `src/entrypoint.ts` (browser bundle)
- `src/App.vue` — top-level Vue shell that mounts both subtrees

## Scripts (run via `corepack pnpm`)

- `dev:vite` — Vite dev server with HMR.
- `build` / `build-cloud` — production bundle (with `ENSO_IDE_CLOUD_BUILD` env toggle for cloud-specific behavior).
- `test:unit` — vitest.
- `test:integration` — Playwright (needs `NODE_OPTIONS='--experimental-wasm-modules'` until Node 24 is default).
- `playwright:install` — install the pinned browser.

## Project-view ↔ backend

The project-view talks to the Enso Language Server via Yjs documents. The Yjs client lives in `ydoc-shared`; messaging is handled via WebSocket (`y-websocket` / `modern-isomorphic-ws`). Parsing of Enso source on the client side uses the Rust parser compiled to WASM (`rust-ffi` package → `ydoc-shared/src/ast/`).

## Dashboard ↔ backend

The dashboard talks to the Enso Cloud over HTTPS (AWS Amplify + Cognito). In local/desktop mode it also talks to the local Project Manager (TS shim during dev, real Scala PM in production builds).

## Assets and icons

Static assets live in `public/`. Icons are provided by `enso-icons` (a separate package from `app/`). The GUI also ships with sample project templates under `templates/`.

## Gotcha: node memory

Vite production builds need `NODE_OPTIONS=--max-old-space-size=6144` (already set in the `build` script). If you invoke Vite directly, re-export it or the Rollup chunker OOMs when sourcemaps are on.
