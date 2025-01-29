/**
 * @file Timeout related hooks.
 */
import { useEffect, useRef, useState, type DependencyList } from 'react'
import { useEventCallback } from './eventCallbackHooks'
import { useUnmount } from './unmountHooks'

/**
 * Options for {@link useTimeoutCallback}.
 */
export interface UseTimeoutCallbackOptions {
  /**
   * Callback to execute after the timeout.
   */
  readonly callback: () => void
  /**
   * Timeout in milliseconds.
   */
  readonly ms: number
  /**
   * Dependencies for {@link useEventCallback}.
   * Reset the timeout when the dependencies change.
   */
  readonly deps?: DependencyList
  /**
   * Whether the timeout is disabled.
   */
  readonly isDisabled?: boolean
}

const STABLE_DEPS_ARRAY: DependencyList = []

/**
 * Hook that executes a callback after a timeout.
 */
export function useTimeoutCallback(options: UseTimeoutCallbackOptions) {
  const { callback, ms, deps = STABLE_DEPS_ARRAY, isDisabled = false } = options

  const timeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null)
  const stableCallback = useEventCallback(callback)

  /**
   * Restarts the timer.
   */
  const restartTimer = useEventCallback(() => {
    stopTimer()
    startTimer()
  })

  /**
   * Starts the timer.
   */
  const startTimer = useEventCallback(() => {
    stopTimer()
    timeoutRef.current = setTimeout(stableCallback, ms)
  })

  /**
   * Stops the timer.
   */
  const stopTimer = useEventCallback(() => {
    if (timeoutRef.current != null) {
      clearTimeout(timeoutRef.current)
      timeoutRef.current = null
    }
  })

  useEffect(() => {
    if (isDisabled) {
      return
    }

    startTimer()

    return () => {
      stopTimer()
    }
    // There is no way to enable compiler, but it's not needed here
    // as everything is stable.
    // eslint-disable-next-line react-compiler/react-compiler
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [ms, isDisabled, ...deps])

  useUnmount(() => {
    stopTimer()
  })

  return [restartTimer, stopTimer, startTimer] as const
}

/**
 * Hook that returns a boolean indicating whether the timeout has expired.
 */
export function useTimeout(params: Pick<UseTimeoutCallbackOptions, 'deps' | 'ms'>) {
  const { ms, deps = STABLE_DEPS_ARRAY } = params

  /**
   * Get the default value for the timeout.
   */
  const getDefaultValue = useEventCallback(() => {
    return ms === 0
  })

  const [isTimeout, setIsTimeout] = useState(getDefaultValue)

  const [restartTimer] = useTimeoutCallback({
    callback: () => {
      setIsTimeout(true)
    },
    ms,
    deps,
    isDisabled: false,
  })

  /**
   * Resets the timeout and restarts it.
   */
  const restart = useEventCallback(() => {
    setIsTimeout(getDefaultValue)
    restartTimer()
  })

  return [isTimeout, restart] as const
}
