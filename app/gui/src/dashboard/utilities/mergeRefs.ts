/** @file Merge multiple refs into a single ref callback. */
import * as React from 'react'

/** Merge multiple refs into a single ref callback. */
export function mergeRefs<T>(
  ...refs: (React.Ref<T> | false | null | undefined)[]
): React.RefCallback<T> {
  return (value) => {
    for (const ref of refs) {
      if (ref != null && ref !== false) {
        if (typeof ref === 'function') {
          ref(value)
        } else {
          const mutableRef: React.MutableRefObject<T | null> = ref
          mutableRef.current = value
        }
      }
    }
  }
}

/**
 * Merge multiple refs into a single ref object.
 * Useful for when you need to pass a ref to a component that only accepts refs as an object.
 */
export function useMergedRef<T>(
  ...refs: (React.Ref<T> | false | null | undefined)[]
): React.RefObject<T> {
  const applyValueOnRefs = React.useMemo(() => mergeRefs(...refs), [refs])

  return React.useMemo(
    () =>
      new Proxy(
        { current: null satisfies T | null },
        {
          /** Set the value of the ref object and call all the refs. */
          set(
            target: { current: null },
            p: string | symbol,
            newValue: T | null,
            receiver: never,
          ): boolean {
            applyValueOnRefs(newValue)

            return Reflect.set(target, p, newValue, receiver)
          },
        },
      ),
    [applyValueOnRefs],
  )
}
