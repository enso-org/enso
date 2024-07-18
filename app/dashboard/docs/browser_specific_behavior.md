# Browser-specific behavior

This document details behavior that is inconsistent between browsers and needs
to be worked around.

## List of inconsistent behaviors

### Drag event missing coordinates

Firefox sets `MouseEvent.pageX` and `MouseEvent.pageY` to `0` for `drag`
events.

#### Fix

Pass the `drag` event handlers to `dragover` event as well, and wrap all `drag`
event handlers in:

````ts
if (event.pageX !== 0 || event.pageY !== 0) {
	// original body here
}
```

#### Affected files

- [`DragModal.tsx`](../src/modals/DragModal.tsx)

### Drag event propagation in text inputs

Text selection in text inputs DO NOT WORK on Firefox, when the text input is a
child of an element with `draggable="true"`.
See [Firefox bug 800050].
To solve this problem, use `useDraggable` from
[`dragAndDropHooks.ts`] on ALL elements that MAY contain a text input.

[Firefox bug 800050]: https://bugzilla.mozilla.org/show_bug.cgi?id=800050

#### Fix

Merge `useDraggable` from [`dragAndDropHooks.ts`] on ALL elements that MAY
contain a text input.

It is recommended to use `aria.mergeProps` to combine these props with existing
props.

```tsx
import * as dragAndDropHooks from "#/hooks/dragAndDropHooks.ts";

const draggableProps = dragAndDropHooks.useDraggable();

return <div {...draggableProps}></div>;
````

[`draggableHooks.ts`]: ../src/hooks/dragAndDropHooks.ts

#### Affected browsers

- Firefox (all versions)

#### Affected files

- [`EditableSpan.tsx`](../src/components/EditableSpan.tsx) - the text inputs
  that are affected
- [`AssetRow.tsx`](../src/components/dashboard/AssetRow.tsx) - fixes text
  selection in `EditableSpan.tsx`
