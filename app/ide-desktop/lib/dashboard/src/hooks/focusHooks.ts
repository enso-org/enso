/** @file Hooks for moving focus. */
import * as React from 'react'

import * as aria from '#/components/aria'

// ==========================
// === useHandleFocusMove ===
// ==========================

/** The type of `react-aria` keyboard events. It must be extracted out of this type as it is not
 * exposed from the library itself. */
// eslint-disable-next-line @typescript-eslint/no-magic-numbers
type AriaKeyboardEvent = Parameters<NonNullable<aria.KeyboardEvents['onKeyUp']>>[0]

/** Handle arrow keys for moving focus. */
export function useHandleFocusMove(direction: 'horizontal' | 'vertical') {
  const focusManager = aria.useFocusManager()
  const keyPrevious = direction === 'horizontal' ? 'ArrowLeft' : 'ArrowUp'
  const keyNext = direction === 'horizontal' ? 'ArrowRight' : 'ArrowDown'

  return React.useCallback(
    (event: AriaKeyboardEvent | React.KeyboardEvent) => {
      const ariaEvent = 'continuePropagation' in event ? event : null
      const reactEvent = 'continuePropagation' in event ? null : event
      switch (event.key) {
        case keyPrevious: {
          const element = focusManager?.focusPrevious()
          if (element != null) {
            reactEvent?.stopPropagation()
            event.stopPropagation()
            event.preventDefault()
          } else {
            ariaEvent?.continuePropagation()
          }
          break
        }
        case keyNext: {
          const element = focusManager?.focusNext()
          if (element != null) {
            reactEvent?.stopPropagation()
            event.stopPropagation()
            event.preventDefault()
          } else {
            ariaEvent?.continuePropagation()
          }
          break
        }
        default: {
          ariaEvent?.continuePropagation()
          break
        }
      }
    },
    [keyPrevious, keyNext, focusManager]
  )
}
