/** @file Utilities for React. */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useMount } from '#/hooks/mountHooks'
import { shallowEquality } from '#/utilities/equalities'
import { useMemo, useRef, type DependencyList } from 'react'

/** `useMemo` but shallowly comparing the `value`. */
export function useShallowMemo<T>(initializer: () => T, dependencies: DependencyList) {
  // This is SAFE as it is assigned in the `useMount` below.
  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  const valueRef = useRef<T>(null!)
  const stableInitializer = useEventCallback(initializer)

  useMount(() => {
    valueRef.current = stableInitializer()
  })

  useMemo(() => {
    const newValue = stableInitializer()
    if (!shallowEquality(valueRef.current, newValue)) {
      valueRef.current = newValue
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [stableInitializer, ...dependencies])

  return valueRef.current
}
