/** @file Hooks for handling keyboard events. */
import * as React from 'react'

/** The axis on which navigation happens. */
export enum Axis {
  horizontal = 'horizontal',
  vertical = 'vertical',
}

/** Options for {@link useKeyboardChildNavigation}. */
export interface KeyboardChildNavigationOptions {
  /** If `true`, handles all arrow key presses, even ones that are a no-op. */
  readonly catchAllArrowKeys?: boolean
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
  const { catchAllArrowKeys = false, axis = Axis.vertical, length, defaultIndex } = options
  const catchAllArrowKeysRef = React.useRef(catchAllArrowKeys)
  const lengthRef = React.useRef(length)
  const defaultIndexRef = React.useRef(defaultIndex ?? 0)
  const keyboardSelectedIndexRef = React.useRef<number | null>(null)
  const [keyboardSelectedIndex, setKeyboardSelectedIndex] = React.useState<number | null>(null)
  keyboardSelectedIndexRef.current = keyboardSelectedIndex
  catchAllArrowKeysRef.current = catchAllArrowKeys
  lengthRef.current = length
  defaultIndexRef.current = defaultIndex ?? 0

  React.useEffect(() => {
    const previousKey = axis === Axis.horizontal ? 'ArrowLeft' : 'ArrowUp'
    const nextKey = axis === Axis.horizontal ? 'ArrowRight' : 'ArrowDown'
    const otherPreviousKey = axis === Axis.horizontal ? 'ArrowUp' : 'ArrowLeft'
    const otherNextKey = axis === Axis.horizontal ? 'ArrowDown' : 'ArrowRight'
    const onKeyDown = (event: KeyboardEvent) => {
      switch (event.key) {
        case otherPreviousKey:
        case otherNextKey: {
          if (catchAllArrowKeysRef.current) {
            event.stopPropagation()
          }
          break
        }
        case previousKey: {
          if (catchAllArrowKeysRef.current) {
            event.stopPropagation()
          }
          const oldIndex = keyboardSelectedIndexRef.current ?? defaultIndexRef.current
          const newIndex = Math.max(0, oldIndex - 1)
          if (newIndex !== oldIndex) {
            event.stopPropagation()
            setKeyboardSelectedIndex(newIndex)
          }
          break
        }
        case nextKey: {
          if (catchAllArrowKeysRef.current) {
            event.stopPropagation()
          }
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

    const root = rootRef.current
    root?.addEventListener('keydown', onKeyDown)
    return () => {
      root?.removeEventListener('keydown', onKeyDown)
    }
  }, [rootRef, axis])

  React.useEffect(() => {
    const onFocusOut = (event: FocusEvent) => {
      if (
        event.currentTarget instanceof HTMLElement &&
        event.relatedTarget instanceof HTMLElement &&
        !event.currentTarget.contains(event.relatedTarget)
      ) {
        if (keyboardSelectedIndexRef.current != null) {
          setKeyboardSelectedIndex(null)
        }
      }
    }

    const root = rootRef.current
    root?.addEventListener('focusout', onFocusOut)
    return () => {
      root?.removeEventListener('focusout', onFocusOut)
    }
  }, [rootRef])

  React.useEffect(() => {
    const onClick = () => {
      if (keyboardSelectedIndexRef.current != null) {
        setKeyboardSelectedIndex(null)
      }
    }

    document.addEventListener('click', onClick, { capture: true })
    return () => {
      document.removeEventListener('click', onClick, { capture: true })
    }
  }, [])

  return [keyboardSelectedIndex, setKeyboardSelectedIndex] as const
}
