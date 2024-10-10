/** @file Hooks related to the HTML5 Drag and Drop API. */
import * as React from 'react'

import * as eventModule from '#/utilities/event'

/**
 * Whether an element is actually draggable. This should be used on ALL
 * elements that are parents of text inputs.
 *
 * This is required to work around a Firefox bug:
 * https://bugzilla.mozilla.org/show_bug.cgi?id=800050
 * @returns An object that should be merged into the element's props.
 */
export function useDraggable() {
  const [isDraggable, setIsDraggable] = React.useState(true)

  return {
    draggable: isDraggable,
    onFocus: (event) => {
      if (eventModule.isElementTextInput(event.target)) {
        setIsDraggable(false)
      }
    },
    onBlur: () => {
      setIsDraggable(true)
    },
  } satisfies Partial<React.HTMLAttributes<HTMLElement>>
}
