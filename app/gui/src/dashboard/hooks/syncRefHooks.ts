/** @file A hook that returns a ref object whose `current` property is always in sync with the provided value. */
import { useRef, type MutableRefObject } from 'react'

/** A hook that returns a ref object whose `current` property is always in sync with the provided value. */
export function useSyncRef<T>(value: T): MutableRefObject<T> {
  const ref = useRef(value)

  /*
    Even though the react core team doesn't recommend setting ref values during the render (it might lead to deoptimizations), the reasoning behind this is:
    - We want to make useEventCallback behave the same way as const x = () => {} or useCallback but have a stable reference.
    - React components shall be idempotent by default, and we don't see violations here.

    React compiler works on the file level, so disabling the rule for this one line is fine as it won't affect other files.
   */
  if (ref.current !== value) {
    ref.current = value
  }

  return ref
}
