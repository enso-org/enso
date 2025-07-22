/**
 * @file
 * Throttle hooks.
 */

import { useEventCallback } from './eventCallbackHooks'

import { useRAF } from './useRaf'

/**
 * Synchronizes calbacks with the RAF loop.
 * Cancels all callbacks before scheduling a new one.
 */
export function useRafThrottle() {
  const { cancelRAF, scheduleRAF: scheduleRAFRaw } = useRAF()

  const scheduleRAF = useEventCallback((callback: FrameRequestCallback) => {
    cancelRAF()
    scheduleRAFRaw(callback)
  })

  return { scheduleRAF, cancelRAF }
}
