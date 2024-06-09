/**
 * @file Merges multiple refs into a single ref callback.
 */
import type * as React from 'react'

/**
 * Merges multiple refs into a single ref callback.
 */
// eslint-disable-next-line no-restricted-syntax
export function mergeRefs<T>(...refs: Array<React.Ref<T> | undefined>): React.RefCallback<T> {
  return value => {
    refs.forEach(ref => {
      if (ref) {
        if (typeof ref === 'function') {
          ref(value)
        } else {
          // eslint-disable-next-line no-restricted-syntax
          ;(ref as React.MutableRefObject<T | null>).current = value
        }
      }
    })
  }
}
