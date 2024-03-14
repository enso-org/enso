/** @file Hooks for handling keyboard events. */
import * as React from 'react'

/** The axis on which navigation happens. */
export enum Axis {
  horizontal = 'horizontal',
  vertical = 'vertical',
}

/** Options for {@link useKeyboardChildNavigation}. */
export interface KeyboardChildNavigationOptions {
  readonly axis?: Axis
  /** The length of the list of children. */
  readonly length: number
  readonly defaultIndex?: number
}

/** Handle UpArrow, DownArrow, Enter, and Escape keys, for navigating between children.. */
export function useKeyboardChildNavigation(
  rootRef: React.RefObject<HTMLElement>,
  options: KeyboardChildNavigationOptions
) {
  const { axis = Axis.vertical, length, defaultIndex } = options
  const lengthRef = React.useRef(length)
  const defaultIndexRef = React.useRef(defaultIndex ?? 0)
  const [keyboardSelectedIndex, setKeyboardSelectedIndexRaw] = React.useState<number | null>(null)
  const keyboardSelectedIndexRef = React.useRef(keyboardSelectedIndex)

  React.useEffect(() => {
    lengthRef.current = length
    defaultIndexRef.current = defaultIndex ?? 0
  }, [length, defaultIndex])

  const setKeyboardSelectedIndex = React.useCallback((index: number | null) => {
    keyboardSelectedIndexRef.current = index
    setKeyboardSelectedIndexRaw(index)
  }, [])

  React.useEffect(() => {
    const previousKey = axis === Axis.horizontal ? 'ArrowLeft' : 'ArrowUp'
    const nextKey = axis === Axis.horizontal ? 'ArrowRight' : 'ArrowDown'
    const onKeyDown = (event: KeyboardEvent) => {
      switch (event.key) {
        case previousKey: {
          const oldIndex = keyboardSelectedIndexRef.current ?? defaultIndexRef.current
          const newIndex = Math.max(0, oldIndex - 1)
          if (newIndex !== oldIndex) {
            event.stopPropagation()
            setKeyboardSelectedIndex(newIndex)
          }
          break
        }
        case nextKey: {
          const oldIndex = keyboardSelectedIndexRef.current ?? defaultIndexRef.current
          const newIndex = Math.min(lengthRef.current - 1, oldIndex + 1)
          if (newIndex !== oldIndex) {
            event.stopPropagation()
            setKeyboardSelectedIndex(newIndex)
          }
          break
        }
        case 'Enter': {
          event.stopPropagation()
          // Should already be handled by the button's `onClick`.
          break
        }
        case 'Escape': {
          if (keyboardSelectedIndexRef.current != null) {
            event.stopPropagation()
            setKeyboardSelectedIndex(null)
          }
          break
        }
      }
    }

    const onFocusOut = (event: FocusEvent) => {
      if (
        event.currentTarget instanceof HTMLElement &&
        event.relatedTarget instanceof HTMLElement &&
        !event.currentTarget.contains(event.relatedTarget)
      ) {
        setKeyboardSelectedIndex(null)
      }
    }

    const root = rootRef.current
    root?.addEventListener('keydown', onKeyDown)
    root?.addEventListener('focusout', onFocusOut)
    return () => {
      root?.removeEventListener('keydown', onKeyDown)
      root?.removeEventListener('focusout', onFocusOut)
    }
  }, [rootRef, axis, /* should never change */ setKeyboardSelectedIndex])

  return [keyboardSelectedIndex, setKeyboardSelectedIndex] as const
}
