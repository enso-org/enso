# project-view/

The **ProjectView feature** subtree: graph editor, code editor, visualizations,
component browser, markdown/plain-text editors, circular menus, documentation
panes. Everything that's specific to viewing and editing an open Enso project
lives here. Import via the `@/` path alias.

This is **not** "the Vue half" — Vue is the main framework of the entire GUI.
This directory is a feature subtree that happens to be Vue, the same way
`src/dashboard/` is a feature subtree that currently happens to be React.

## What belongs here (and what doesn't)

- **Belongs**: code whose meaning depends on an open project — graph rendering,
  AST editing, component browser, per-node widgets, the Yjs session glue,
  visualization hosts.
- **Doesn't belong**: general-purpose UI primitives, cross-feature utilities,
  framework-level infrastructure. Those are _commons_ and should live at `src/`
  directly (`src/components/`, `src/composables/`, `src/providers/`,
  `src/util/`, …) so both ProjectView and other features can share them.

Historically, many commons ended up inside this directory because it was the
only Vue location. The plan is to progressively **move commons out of
`project-view/` into proper places under `src/`**. When you add something here,
ask first: is this ProjectView-specific, or would another feature want it too?

## Structure

- `components/` — Vue SFCs specific to ProjectView. Large-feature components get
  their own sub-folder (e.g. `GraphEditor/`, `CodeEditor/`, `ComponentBrowser/`,
  `DocumentationEditor/`, `MarkdownEditor/`). Truly shared components should
  move to `src/components/`.
- `composables/` — Vue composables (`useX()`). ProjectView-specific composables
  only; share-worthy ones belong at `src/composables/`.
- `stores/` — App-wide reactive state for the ProjectView session. These are
  **plain Vue composables that `provide()` a reactive shape**, not Pinia. A
  `provideXxx()` is paired with a `useXxx()`/`injectXxx()`.
- `providers/` — Pure `inject`/`provide` keys, no logic. Use when a single typed
  slot is enough and a full store is overkill.
- `assets/` — Static SVGs/images used inside ProjectView.
- `util/` — Pure TS utilities (no Vue imports in most files). Move to
  `src/util/` when something stops being ProjectView-specific.
- `bindings.ts` — Keyboard shortcuts for ProjectView (see `app/gui/shortcuts.md`
  for the user-facing list).

## Interop with the Dashboard subtree

The Dashboard (currently React under `src/dashboard/`) is being migrated to Vue.
Cross-subtree wiring should go through `src/providers/` (the `$/providers/`
alias), not by reaching into the other subtree's internals. For React/Vue
interop today, use the existing `veaury` wrappers.

## Graph editor

`GraphEditor.vue` is the heart of ProjectView. It renders an SVG scene plus
positioned Vue components per node, talks to Yjs (via `ydoc-shared`) for
collaborative edits, and uses `y-protocols` for awareness (cursors, selections).
New interactions typically become composables in `components/GraphEditor/` or
`composables/`.

## Visualizations

Each visualization in `components/visualizations/` is compiled as a **custom
element** (see `customElement` matcher in `app/gui/vite.config.ts`). That means
they run in the Shadow DOM and must not rely on global Tailwind classes bleeding
in. Tags start with `enso-`.

## Reactivity rules

Follow Vue 3 conventions: prefer `ref` over `reactive` for primitives, and use
`shallowRef` when storing large external objects (Yjs docs, CodeMirror
`EditorState`) that have their own reactivity or where deep proxying would be
wasteful.
