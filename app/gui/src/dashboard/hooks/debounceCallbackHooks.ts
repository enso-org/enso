/** @file A hook to debounce a callback function. */
import * as React from 'react'

import { useEventCallback } from './eventCallbackHooks'
import { useUnmount } from './unmountHooks'

/** Wrap a callback into a debounced function. */
export function useDebouncedCallback<Fn extends (...args: never[]) => unknown>(
  callback: Fn,
  /** When `false`, the callback is executed immediately, before even the next microtask. */
  delayMs: number | false,
  maxWait: number | null = null,
): DebouncedFunction<Fn> {
  const stableCallback = useEventCallback(callback)

  const timeoutIdRef = React.useRef<ReturnType<typeof setTimeout>>()
  const waitTimeoutIdRef = React.useRef<ReturnType<typeof setTimeout>>()

  const lastCallRef = React.useRef<{ args: Parameters<Fn> }>()

  const clear = useEventCallback(() => {
    if (timeoutIdRef.current) {
      clearTimeout(timeoutIdRef.current)
      timeoutIdRef.current = undefined
    }

    if (waitTimeoutIdRef.current) {
      clearTimeout(waitTimeoutIdRef.current)
      waitTimeoutIdRef.current = undefined
    }
  })

  const execute = useEventCallback(() => {
    if (!lastCallRef.current) {
      return
    }

    const context = lastCallRef.current
    lastCallRef.current = undefined

    stableCallback(...context.args)

    clear()
  })

  const wrapped = useEventCallback((...args: Parameters<Fn>) => {
    if (timeoutIdRef.current) {
      clearTimeout(timeoutIdRef.current)
    }

    lastCallRef.current = { args }

    if (delayMs === false) {
      execute()
    } else {
      // plan regular execution
      timeoutIdRef.current = setTimeout(execute, delayMs)

      // plan maxWait execution if required
      if (maxWait != null && !waitTimeoutIdRef.current) {
        waitTimeoutIdRef.current = setTimeout(execute, maxWait)
      }
    }
  })

  Object.defineProperties(wrapped, {
    length: { value: stableCallback.length },
    name: { value: `${stableCallback.name || 'anonymous'}__debounced__${delayMs}` },
  })

  // cancel scheduled execution on unmount
  useUnmount(clear)

  return wrapped
}

/** Return a state that waits until `delayMs` has passed since the last update to update the derived state. */
export function useDerivedDebouncedState<T>(state: T, delayMs: number) {
  const [debouncedState, setDebouncedState] = React.useState(state)
  const handleRef = React.useRef(0)
  React.useEffect(() => {
    clearTimeout(handleRef.current)
    handleRef.current = window.setTimeout(() => {
      React.startTransition(() => {
        setDebouncedState(state)
      })
    }, delayMs)
  }, [delayMs, state])
  return debouncedState
}

/** The type of a wrapped function that has been debounced. */
export type DebouncedFunction<Fn extends (...args: never[]) => unknown> = (
  this: ThisParameterType<Fn>,
  ...args: Parameters<Fn>
) => void

/** Wrap an async callback into a throttled async function */
export function useThrottledAsyncCallback<Fn extends (...args: Parameters<Fn>) => Promise<T>, T>(
  callback: Fn,
): Fn {
  const promiseRef = React.useRef<Promise<T> | null>(null)

  const wrapped = useEventCallback((...args: Parameters<Fn>) => {
    if (promiseRef.current == null) {
      const promise = callback(...args)
      promiseRef.current = promise
      void promise.then(() => {
        promiseRef.current = null
      })
    }
    return promiseRef.current
  })

  Object.defineProperties(wrapped, {
    length: { value: callback.length },
    name: { value: `${callback.name || 'anonymous'}__throttled__` },
  })

  // eslint-disable-next-line no-restricted-syntax
  return wrapped as Fn
}
