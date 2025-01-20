/**
 * @file
 *
 * This file contains the `useDebounceState` hook,
 * which is a custom hook that returns a stateful value and a function to update it that will debounce updates.
 */
import * as React from 'react'

import * as debouncedCallback from './debounceCallbackHooks'
import { useEventCallback } from './eventCallbackHooks'

/** A hook that returns a stateful value, and a function to update it that will debounce updates. */
export function useDebounceState<S>(
  initialState: S | (() => S),
  delay: number,
  maxWait = 0,
): [S, React.Dispatch<React.SetStateAction<S>>] {
  const [state, setState] = React.useState(initialState)
  const currentValueRef = React.useRef(state)

  const debouncedSetState = debouncedCallback.useDebouncedCallback<
    React.Dispatch<React.SetStateAction<S>>
  >(
    (value) => {
      React.startTransition(() => {
        setState(value)
      })
    },
    delay,
    maxWait,
  )

  const setValue = useEventCallback((next: S | ((currentValue: S) => S)) => {
    currentValueRef.current = next instanceof Function ? next(currentValueRef.current) : next

    debouncedSetState(currentValueRef.current)
  })

  return [state, setValue]
}
