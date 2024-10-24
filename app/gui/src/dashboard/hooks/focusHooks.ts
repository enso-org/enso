/** @file Hooks for moving focus. */
import * as React from 'react'

import * as focusClassProvider from '#/providers/FocusClassProvider'
import * as focusDirectionProvider from '#/providers/FocusDirectionProvider'

import * as aria from '#/components/aria'

// ==========================
// === useHandleFocusMove ===
// ==========================

/**
 * The type of `react-aria` keyboard events. It must be extracted out of this type as it is not
 * exposed from the library itself.
 */
type AriaKeyboardEvent = Parameters<NonNullable<aria.KeyboardEvents['onKeyUp']>>[0]

/** Handle arrow keys for moving focus. */
export function useHandleFocusMove(direction: 'horizontal' | 'vertical') {
  const { focusChildClass } = focusClassProvider.useFocusClasses()
  const focusManager = aria.useFocusManager()
  const keyPrevious = direction === 'horizontal' ? 'ArrowLeft' : 'ArrowUp'
  const keyNext = direction === 'horizontal' ? 'ArrowRight' : 'ArrowDown'

  return React.useCallback(
    (event: AriaKeyboardEvent | React.KeyboardEvent) => {
      const ariaEvent = 'continuePropagation' in event ? event : null
      const reactEvent = 'continuePropagation' in event ? null : event
      switch (event.key) {
        case keyPrevious: {
          const element = focusManager?.focusPrevious({
            accept: (other) => other.classList.contains(focusChildClass),
          })
          if (element != null) {
            reactEvent?.stopPropagation()
            event.preventDefault()
          } else {
            ariaEvent?.continuePropagation()
          }
          break
        }
        case keyNext: {
          const element = focusManager?.focusNext({
            accept: (other) => other.classList.contains(focusChildClass),
          })
          if (element != null) {
            reactEvent?.stopPropagation()
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
    [keyPrevious, keyNext, focusManager, focusChildClass],
  )
}

// =========================
// === useSoleFocusChild ===
// =========================

/**
 * Return JSX props to make a child focusable by `Navigator2D`. DOES NOT handle arrow keys,
 * because this hook assumes the child is the only focus child.
 */
export function useSoleFocusChild() {
  const { focusChildClass } = focusClassProvider.useFocusClasses()

  return {
    className: focusChildClass,
  } satisfies React.HTMLAttributes<Element>
}

// =====================
// === useFocusChild ===
// =====================

/**
 * Return JSX props to make a child focusable by `Navigator2D`, and make the child handle arrow
 * keys to navigate to siblings.
 */
export function useFocusChild() {
  const focusDirection = focusDirectionProvider.useFocusDirection()
  const handleFocusMove = useHandleFocusMove(focusDirection)
  const { focusChildClass } = focusClassProvider.useFocusClasses()

  return {
    className: focusChildClass,
    onKeyDown: handleFocusMove,
  } satisfies React.HTMLAttributes<Element>
}
