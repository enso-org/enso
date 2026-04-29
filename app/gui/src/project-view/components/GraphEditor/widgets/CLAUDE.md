# widgets/

Per-node widget tree for the graph editor. Each `Widget*.vue` is a Vue SFC plus
a `widgetDefinition` exported from a sibling `<script lang="ts">` block. The
registry walks the node's AST and picks one widget per subexpression.

## Selection — `defineWidget`

A widget definition declares:

- A **matcher** — `WidgetInput.astMatcher(Ast.X)`, a `Symbol` on `WidgetInput`,
  or a custom type guard. Inputs that fail the matcher are never scored.
- A **priority** — **smaller wins**. The registry sorts ascending and
  short-circuits on the first `Score.Perfect`. So priority 1 beats priority 2.
- A **score** — `Score.Mismatch | Weak | Good | Perfect`. `Mismatch` rules the
  widget out even if the matcher passed. `Perfect` lets the registry stop
  scanning lower-priority candidates.

See `app/gui/src/providers/openedProjects/widgetRegistry/widgetRegistry.ts`.

When a widget renders `<NodeWidget :input="props.input" />` over its own input,
the registry is re-run with that widget added to `alreadyUsed`, so it won't
match itself recursively. That's how `WidgetIcon` wraps a child widget around
its glyph without infinite recursion.

## Icon rendering — render once, at the right layer

The node's category icon is the responsibility of **one** widget per row. Don't
draw a second icon from a downstream widget — that's the bug `WidgetAiPrompt`
was originally guilty of.

Two paths land an icon in the row:

- **Method calls** (`subject.method args …`): `WidgetSelfAccessChain` matches
  the outer `PropertyAccess`, computes the displayed icon from the call's `lhs`,
  and attaches `DisplayIcon` to that `lhs`'s widget input. The icon appears
  where the subject would have rendered.
- **Static calls** — `Main.<name> args …`, `Data.read …`, any application whose
  subject is a Type/constructor: `tree.primaryApplication.function` is `null`,
  because the AST's outer `App` function is the whole property access (the
  resolver folds the type subject into the method identity). In that case
  `ComponentWidgetTree.vue` attaches `DisplayIcon` to the **root** input, and
  `WidgetIcon` renders the glyph + recurses into the same input via
  `<NodeWidget>` for the rest of the row.

So a custom widget that occupies the function-token slot of a static call (e.g.
`WidgetAiPrompt` for AI nodes) must render only its own slot content. The icon
is already on the row, drawn by `WidgetIcon` at the root.

## File layout

- `Widget*.vue` — leaf widgets registered with `defineWidget`.
- `Widget*/` — folder beside a widget for its helper modules (no enforced
  pattern beyond colocation).
- `WidgetIcon.vue` — owns the `DisplayIcon` symbol. Other widgets attach to it
  via `input[DisplayIcon] = { icon, showContents?, noGap? }` to request an
  inline icon in front of a child widget.
