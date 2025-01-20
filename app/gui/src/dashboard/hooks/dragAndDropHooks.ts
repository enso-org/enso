/** @file Hooks related to the HTML5 Drag and Drop API. */
import * as React from 'react'

import * as eventModule from '#/utilities/event'

/**
 * Parameters for the `useDraggable` hook.
 */
export interface UseDraggableOptions {
  /**
   * Whether the drag and drop functionality should be disabled.
   */
  readonly isDisabled?: boolean
}

/**
 * Whether an element is actually draggable. This should be used on ALL
 * elements that are parents of text inputs.
 *
 * This is required to work around a Firefox bug:
 * https://bugzilla.mozilla.org/show_bug.cgi?id=800050
 * @returns An object that should be merged into the element's props.
 */
export function useDraggable(params: UseDraggableOptions = {}) {
  const { isDisabled = false } = params

  const [isDraggable, setIsDraggable] = React.useState(true)

  return {
    draggable: isDisabled ? false : isDraggable,
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
