/** @file A hook that returns a ref object whose `current` property is always in sync with the provided value. */
import { type MutableRefObject, useRef } from 'react'

/** A hook that returns a ref object whose `current` property is always in sync with the provided value. */
export function useSyncRef<T>(value: T): Readonly<MutableRefObject<T>> {
  const ref = useRef(value)

  /*
  Even though the react core team doesn't recommend setting ref values during the render (it might lead to deoptimizations), the reasoning behind this is:
  - We want to make useEventCallback behave the same way as const x = () => {} or useCallback but have a stable reference.
  - React components shall be idempotent by default, and we don't see violations here.
   */
  ref.current = value

  return ref
}
