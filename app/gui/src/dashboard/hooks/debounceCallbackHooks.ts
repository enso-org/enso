/**
 * @file
 *
 * This file contains the `useDebouncedCallback` hook which is used to debounce a callback function.
 */
import * as React from 'react'

import * as callbackHooks from './eventCallbackHooks'
import * as unmountEffect from './unmountHooks'

/** Wrap a callback into debounce function */
export function useDebouncedCallback<Fn extends (...args: never[]) => unknown>(
  callback: Fn,
  deps: React.DependencyList,
  delay: number,
  maxWait = 0,
): DebouncedFunction<Fn> {
  const stableCallback = callbackHooks.useEventCallback(callback)
  const timeoutIdRef = React.useRef<ReturnType<typeof setTimeout>>()
  const waitTimeoutIdRef = React.useRef<ReturnType<typeof setTimeout>>()
  const lastCallRef = React.useRef<{ args: Parameters<Fn> }>()

  const clear = () => {
    if (timeoutIdRef.current) {
      clearTimeout(timeoutIdRef.current)
      timeoutIdRef.current = undefined
    }

    if (waitTimeoutIdRef.current) {
      clearTimeout(waitTimeoutIdRef.current)
      waitTimeoutIdRef.current = undefined
    }
  }

  // cancel scheduled execution on unmount
  unmountEffect.useUnmount(clear)

  return React.useMemo(() => {
    const execute = () => {
      if (!lastCallRef.current) {
        return
      }

      const context = lastCallRef.current
      lastCallRef.current = undefined

      stableCallback(...context.args)

      clear()
    }

    const wrapped = (...args: Parameters<Fn>) => {
      if (timeoutIdRef.current) {
        clearTimeout(timeoutIdRef.current)
      }

      lastCallRef.current = { args }

      if (delay === 0) {
        execute()
      } else {
        // plan regular execution
        timeoutIdRef.current = setTimeout(execute, delay)

        // plan maxWait execution if required
        if (maxWait > 0 && !waitTimeoutIdRef.current) {
          waitTimeoutIdRef.current = setTimeout(execute, maxWait)
        }
      }
    }

    Object.defineProperties(wrapped, {
      length: { value: stableCallback.length },
      name: { value: `${stableCallback.name || 'anonymous'}__debounced__${delay}` },
    })

    return wrapped
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [stableCallback, delay, maxWait, ...deps])
}

/** The type of a wrapped function that has been debounced. */
export type DebouncedFunction<Fn extends (...args: never[]) => unknown> = (
  this: ThisParameterType<Fn>,
  ...args: Parameters<Fn>
) => void
