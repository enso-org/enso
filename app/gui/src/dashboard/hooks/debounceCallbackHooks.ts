/** @file A hook to debounce a callback function. */
import * as React from 'react'

import { useEventCallback } from './eventCallbackHooks'
import { useUnmount } from './unmountHooks'

/** Wrap a callback into a debounced function */
export function useDebouncedCallback<Fn extends (...args: never[]) => unknown>(
  callback: Fn,
  /** The delay in milliseconds. Set to `false` to disable debouncing. */
  delay: number | false,
  maxWait: number | null = null,
): DebouncedFunction<Fn> {
  const stableCallback = useEventCallback(callback)

  const timeoutIdRef = React.useRef<ReturnType<typeof setTimeout> | null>(null)
  const waitTimeoutIdRef = React.useRef<ReturnType<typeof setTimeout> | null>(null)

  const lastCallRef = React.useRef<{ args: Parameters<Fn> } | null>(null)

  const clear = useEventCallback(() => {
    if (timeoutIdRef.current) {
      clearTimeout(timeoutIdRef.current)
      timeoutIdRef.current = null
    }

    if (waitTimeoutIdRef.current) {
      clearTimeout(waitTimeoutIdRef.current)
      waitTimeoutIdRef.current = null
    }
  })

  const execute = useEventCallback(() => {
    if (!lastCallRef.current) {
      return
    }

    const context = lastCallRef.current
    lastCallRef.current = null

    stableCallback(...context.args)

    clear()
  })

  const wrapped = useEventCallback((...args: Parameters<Fn>) => {
    if (timeoutIdRef.current) {
      clearTimeout(timeoutIdRef.current)
    }

    lastCallRef.current = { args }

    if (delay === false) {
      execute()
    } else {
      // plan regular execution
      timeoutIdRef.current = setTimeout(execute, delay)

      // plan maxWait execution if required
      if (maxWait != null && !waitTimeoutIdRef.current) {
        waitTimeoutIdRef.current = setTimeout(execute, maxWait)
      }
    }
  })

  Object.defineProperties(wrapped, {
    length: { value: stableCallback.length },
    name: { value: `${stableCallback.name || 'anonymous'}__debounced__${delay}` },
  })

  // cancel scheduled execution on unmount
  useUnmount(clear)

  return wrapped
}

/** The type of a wrapped function that has been debounced. */
export type DebouncedFunction<Fn extends (...args: never[]) => unknown> = (
  this: ThisParameterType<Fn>,
  ...args: Parameters<Fn>
) => void
