/** @file Hooks for throttling events. */
import { useEventCallback } from './eventCallbackHooks'
import { useRAF } from './useRaf'

/**
 * Synchronizes calbacks with the RAF loop.
 * Cancels all callbacks before scheduling a new one.
 */
export function useRafThrottle() {
  const { cancelRaf: cancelRafRaw, scheduleRaf: scheduleRafRaw } = useRAF()

  const scheduleRAF = useEventCallback((callback: FrameRequestCallback) => {
    cancelRafRaw()
    scheduleRafRaw(callback)
  })

  return { scheduleRAF, cancelRAF: cancelRafRaw }
}
