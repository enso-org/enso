/**
 * @file
 *
 * This file contains the `useDebouncedCallback` hook which is used to debounce a callback function.
 */
import * as React from 'react'

import * as callbackHooks from './eventCallbackHooks'
import * as unmountEffect from './unmountEffectHooks'

/**
 * Wrap a callback into debounce function
 */
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function useDebouncedCallback<Fn extends (...args: any[]) => any>(
  callback: Fn,
  deps: React.DependencyList,
  delay: number,
  maxWait = 0
): DebouncedFunction<Fn> {
  const callbackEvent = callbackHooks.useEventCallback(callback)
  const timeout = React.useRef<ReturnType<typeof setTimeout>>()
  const waitTimeout = React.useRef<ReturnType<typeof setTimeout>>()
  const lastCall = React.useRef<{ args: Parameters<Fn>; this: ThisParameterType<Fn> }>()

  const clear = () => {
    if (timeout.current) {
      clearTimeout(timeout.current)
      timeout.current = undefined
    }

    if (waitTimeout.current) {
      clearTimeout(waitTimeout.current)
      waitTimeout.current = undefined
    }
  }

  // cancel scheduled execution on unmount
  unmountEffect.useUnmountEffect(clear)

  return React.useMemo(() => {
    const execute = () => {
      if (!lastCall.current) {
        // eslint-disable-next-line no-restricted-syntax
        return
      }

      const context = lastCall.current
      lastCall.current = undefined

      callbackEvent.apply(context.this, context.args)

      clear()
    }

    // eslint-disable-next-line no-restricted-syntax
    const wrapped = function (this, ...args) {
      if (timeout.current) {
        clearTimeout(timeout.current)
      }

      lastCall.current = { args, this: this }

      if (delay === 0) {
        execute()
      } else {
        // plan regular execution
        timeout.current = setTimeout(execute, delay)

        // plan maxWait execution if required
        if (maxWait > 0 && !waitTimeout.current) {
          waitTimeout.current = setTimeout(execute, maxWait)
        }
      }
    } as DebouncedFunction<Fn>

    Object.defineProperties(wrapped, {
      length: { value: callbackEvent.length },
      name: { value: `${callbackEvent.name || 'anonymous'}__debounced__${delay}` },
    })

    return wrapped
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [callbackEvent, delay, maxWait, ...deps])
}

/**
 *
 */
// eslint-disable-next-line @typescript-eslint/no-explicit-any
export interface DebouncedFunction<Fn extends (...args: any[]) => any> {
  (this: ThisParameterType<Fn>, ...args: Parameters<Fn>): void
}
