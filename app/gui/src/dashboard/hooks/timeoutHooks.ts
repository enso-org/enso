/**
 * @file Timeout related hooks.
 */
import { useCallback, useEffect, useRef, useState, type DependencyList } from 'react'
import { noop } from '../utilities/functions'
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
 * A custom error class for abort error.
 */
export class AbortError extends Error {
  /**
   * Create a new abort error.
   */
  constructor(message: string) {
    super(message)
    this.name = 'AbortError'
  }
}

/**
 * Hook that provides imperative API for timeouts
 * Keeps only one timeout active at a time, cancelling the previous, and cleaning up on unmount
 */
export function useTimeoutApi(
  options: Pick<UseTimeoutCallbackOptions, 'callback' | 'ms'>,
): [start: () => void, stop: () => void, restart: () => void] {
  const { callback, ms } = options

  const stableCallback = useEventCallback(callback)

  const timeoutAPI = useTimeoutAPI({ ms })

  /**
   * Restarts the timer.
   */
  const restartTimer = useEventCallback(() => {
    void timeoutAPI.restartTimer().then(stableCallback).catch(noop)
  })

  /**
   * Starts the timer.
   */
  const startTimer = useEventCallback(() => {
    void timeoutAPI.startTimer().then(stableCallback).catch(noop)
  })

  /**
   * Stops the timer.
   */
  const stopTimer = useEventCallback(() => {
    timeoutAPI.stopTimer()
  })

  useUnmount(() => {
    stopTimer()
  })

  return [startTimer, stopTimer, restartTimer]
}

/**
 * Hook that executes a callback after a timeout.
 */
export function useTimeoutCallback(
  options: UseTimeoutCallbackOptions,
): [restart: () => void, stop: () => void, start: () => void] {
  const { callback, ms, deps = STABLE_DEPS_ARRAY, isDisabled = false } = options

  const [startTimer, stopTimer, restartTimer] = useTimeoutApi({ callback, ms })

  useEffect(() => {
    if (isDisabled) return
    startTimer()
    return stopTimer
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [ms, isDisabled, ...deps])

  return [restartTimer, stopTimer, startTimer]
}

/** Hook that returns a boolean indicating whether the timeout has expired. */
export function useTimeout(params: Pick<UseTimeoutCallbackOptions, 'deps' | 'ms'>) {
  const { ms, deps = STABLE_DEPS_ARRAY } = params
  const getDefaultValue = useEventCallback(() => ms === 0)
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

/**
 * Hook that returns a promise that resolves when the timeout expires.
 */
export function useTimeoutAPI(params: Pick<UseTimeoutCallbackOptions, 'ms'>) {
  const { ms } = params

  const timeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null)
  const resolveRef = useRef<(() => void) | null>(null)
  const rejectRef = useRef<((reason?: unknown) => void) | null>(null)

  const cleanup = useCallback(() => {
    timeoutRef.current = null
    resolveRef.current = null
    rejectRef.current = null
  }, [])

  /**
   * Stops the timer.
   */
  const stopTimer = useEventCallback(() => {
    if (timeoutRef.current != null) {
      clearTimeout(timeoutRef.current)
      rejectRef.current?.(new AbortError('Timeout aborted'))
      cleanup()
    }
  })

  /**
   * Starts the timer.
   */
  const startTimer = useEventCallback(async (signal?: AbortSignal) => {
    stopTimer()

    const abortHandler = () => {
      rejectRef.current?.(new AbortError('Timeout aborted'))
    }

    const timeoutPromise = new Promise<void>((res, rej) => {
      resolveRef.current = () => {
        signal?.removeEventListener('abort', abortHandler)
        cleanup()
        res()
      }
      rejectRef.current = (...args: unknown[]) => {
        signal?.removeEventListener('abort', abortHandler)
        cleanup()
        // eslint-disable-next-line @typescript-eslint/prefer-promise-reject-errors
        rej(...args)
      }
    })

    signal?.addEventListener('abort', abortHandler, { once: true })

    timeoutRef.current = setTimeout(() => {
      resolveRef.current?.()
    }, ms)

    return timeoutPromise
  })

  /**
   * Restarts the timer.
   */
  const restartTimer = useEventCallback(async (signal?: AbortSignal) => {
    stopTimer()
    return startTimer(signal)
  })

  useUnmount(stopTimer)

  return { startTimer, stopTimer, restartTimer } as const
}
