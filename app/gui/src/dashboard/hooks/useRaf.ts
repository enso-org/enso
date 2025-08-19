/** @file A hook that synchronizes callbacks with the RAF loop. */
import * as React from 'react'
import { useEventCallback } from './eventCallbackHooks.ts'
import { useUnmount } from './unmountHooks.ts'

/** Adds a callback to the RAF loop. */
export function useRAF() {
  const callbacksRef = React.useRef<Set<FrameRequestCallback>>(new Set())
  const lastRafRef = React.useRef<ReturnType<typeof requestAnimationFrame> | null>(null)

  const cancelRaf = useEventCallback(() => {
    if (lastRafRef.current != null) {
      cancelAnimationFrame(lastRafRef.current)
      lastRafRef.current = null
      callbacksRef.current.clear()
    }
  })

  const scheduleRaf = useEventCallback((callback: FrameRequestCallback) => {
    if (lastRafRef.current == null) {
      lastRafRef.current = requestAnimationFrame((time) => {
        const lastCallbacks = [...callbacksRef.current]

        lastCallbacks.forEach((cb) => {
          cb(time)
        })
        lastRafRef.current = null
        callbacksRef.current.clear()
      })
    }

    callbacksRef.current.add(callback)

    return () => {
      callbacksRef.current.delete(callback)
    }
  })

  useUnmount(cancelRaf)

  return { scheduleRaf, cancelRaf } as const
}
