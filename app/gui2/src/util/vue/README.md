# Vue composables

This directory contains various Vue composables, mostly used to interact with the DOM:

- `animation.ts` defines:
  - `useRaf` to run a callback every frame
  - `useApproach` to smoothly interpolate a value towards a given target value
- `events.ts` defines:
  - `useEvent` to watch a JS event
  - `useEventConditional` to watch a JS event, but only when the given `WatchSource<boolean>` is `true`
- `navigator.ts` defines `useNavigator`, which handles zooming and panning based on pointer events
- `selection.ts` defines `useSelection`, which depends on a `useNavigator`, and tracks which elements are selected based on the given reactive `Map` of `Rect`s
