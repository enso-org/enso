# project-view/ (Vue — target framework)

The Vue 3 half of the IDE: graph editor, code editor, visualizations, component browser, markdown/plain-text editors, circular menus, documentation panes. Import this half via the `@/` path alias.

**Historical / migration context**: the separate React "dashboard" under `src/dashboard/` is legacy and is being merged into a **single unified Vue GUI app** over time. Both the framework split and the conceptual dashboard/project-view split are historical. New features default to Vue; when you organize code here, put it where it will still make sense once the halves are unified rather than in a shape that assumes "project-view" stays a separate silo.

## Structure

- `components/` — Vue SFCs. Large-feature components get their own sub-folder (e.g. `GraphEditor/`, `CodeEditor/`, `ComponentBrowser/`, `DocumentationEditor/`, `MarkdownEditor/`). Small shared components sit at the top level.
- `composables/` — Vue composables (`useX()`). Prefer a composable over a component method whenever the logic doesn't directly render DOM.
- `stores/` — App-wide reactive state. These are **plain Vue composables that `provide()` a reactive shape**, not Pinia. A `provideXxx()` is paired with a `useXxx()`/`injectXxx()`.
- `providers/` — Pure `inject`/`provide` keys, no logic. Use when a single typed slot is enough and a full store is overkill.
- `assets/` — Static SVGs/images bundled by Vite.
- `util/` — Pure TS utilities (no Vue imports in most files).
- `bindings.ts` — Keyboard shortcuts for this half (see `app/gui/shortcuts.md` for the user-facing list).

## Interop with the React dashboard

Don't reach across frameworks by hand. Use the `$/` alias only for *data* types (`providers/container`, `providers/openedProjects`, i18n `text`) that are framework-agnostic. For UI, embed React into Vue via the existing `ResultComponent` / `veaury` wrappers.

## Graph editor

`GraphEditor.vue` is the heart of the IDE. It renders an SVG scene plus positioned Vue components per node, talks to Yjs (via `ydoc-shared`) for collaborative edits, and uses `y-protocols` for awareness (cursors, selections). New interactions typically become composables in `components/GraphEditor/` or `composables/`.

## Visualizations

Each visualization in `components/visualizations/` is compiled as a **custom element** (see `customElement` matcher in `app/gui/vite.config.ts`). That means they run in the Shadow DOM and must not rely on global Tailwind classes bleeding in. Tags start with `enso-`.

## Reactivity rules

Follow Vue 3 conventions: prefer `ref` over `reactive` for primitives, and use `shallowRef` when storing large external objects (Yjs docs, CodeMirror `EditorState`) that have their own reactivity or where deep proxying would be wasteful.
