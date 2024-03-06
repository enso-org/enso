/**
 * @file
 *
 * A hook that returns a memoized function that will only be called once
 */

import * as React from 'react'

const UNSET_VALUE = Symbol('inset')

/**
 * A hook that returns a memoized function that will only be called once
 */
export function useLazyMemo<T>(factory: T | (() => T), deps: React.DependencyList): () => T {
  return React.useMemo(() => {
    let cachedValue: T | typeof UNSET_VALUE = UNSET_VALUE

    return (): T => {
      if (cachedValue === UNSET_VALUE) {
        cachedValue = factory instanceof Function ? factory() : factory
      }

      return cachedValue
    }
  }, deps)
}
