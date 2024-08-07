/**
 * @file useSyncRef.ts
 *
 * A hook that returns a ref object whose `current` property is always in sync with the provided value.
 */

import * as React from 'react'

/**
 * A hook that returns a ref object whose `current` property is always in sync with the provided value.
 */
export function useSyncRef<T>(value: T): Readonly<React.MutableRefObject<T>> {
  const ref = React.useRef(value)

  // Update the ref value whenever the provided value changes
  // Refs shall never change during the render phase, so we use `useEffect` here.
  ref.current = value

  return ref
}
