/**
 * @file
 *
 * A hook that returns a memoized function that will only be called once
 */

import * as React from 'react'

const UNSET_VALUE = Symbol('unset')

/** A hook that returns a memoized function that will only be called once */
export function useLazyMemoHooks<T>(factory: T | (() => T), deps: React.DependencyList): () => T {
  return React.useMemo(() => {
    let cachedValue: T | typeof UNSET_VALUE = UNSET_VALUE

    return (): T => {
      if (cachedValue === UNSET_VALUE) {
        cachedValue = factory instanceof Function ? factory() : factory
      }

      return cachedValue
    }
    // We assume that the callback should change only when
    // the deps change.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, deps)
}
