/** @file Merge multiple refs into a single ref callback. */
import type * as React from 'react'

/** Merge multiple refs into a single ref callback. */
export function mergeRefs<T>(...refs: React.Ref<T>[]): React.RefCallback<T> {
  return value => {
    for (const ref of refs) {
      if (!ref) {
        // Ignore.
      } else if (typeof ref === 'function') {
        ref(value)
      } else {
        const mutableRef: React.MutableRefObject<T | null> = ref
        mutableRef.current = value
      }
    }
  }
}
