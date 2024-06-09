/**
 * @file useSyncRef.ts
 *
 * A hook that returns a ref object whose `current` property is always in sync with the provided value.
 */

import * as React from 'react'

/**
 * A hook that returns a ref object whose `current` property is always in sync with the provided value.
 */
export function useSyncRef<T>(value: T): React.MutableRefObject<T> {
  const ref = React.useRef(value)
  ref.current = value

  return ref
}
