/** @file React Hooks for running a callback on every frame, and smooth interpolation. */
import * as React from 'react'

// ==============
// === useRaf ===
// ==============

/** Details for a callback used in {@link useRaf}. */
interface RafCallback {
  readonly fn: (t: number, dt: number) => void
  readonly priority: number
}

const RAF_CALLBACKS: RafCallback[] = []

/**
 * Use `requestAnimationFrame` API to perform animation. The callback will be called every frame
 * while the `active` watch source returns true value.
 *
 * For performing simple easing animations, see [`useApproach`].
 * @param active - As long as it returns true value, the `fn` callback will be called every frame.
 * @param fn - The callback to call every animation frame.
 * @param priority - When multiple callbacks are registered, the one with the lowest priority number
 * will be called first. Default priority is 0. For callbacks with the same priority, the order of
 * execution matches the order of registration.
 */
export function useRaf(active: boolean, fn: (t: number, dt: number) => void, priority = 0): void {
  const callback = React.useMemo(() => ({ fn, priority }), [fn, priority])
  React.useEffect(() => {
    if (active) {
      const idx = RAF_CALLBACKS.findIndex((cb) => cb.priority > priority)
      if (idx >= 0) {
        RAF_CALLBACKS.splice(idx, 0, callback)
      } else {
        RAF_CALLBACKS.push(callback)
        runRaf()
      }
      return () => {
        const i = RAF_CALLBACKS.indexOf(callback)
        if (i >= 0) {
          RAF_CALLBACKS.splice(i, 1)
        }
      }
    } else {
      return
    }
  }, [active, callback, priority])
}

let animTime = 0
let rafRunning = false

/** Run all callbacks registered by {@link useRaf} in a continuous loop. */
function tick(time: number) {
  const lastTime = animTime
  const delta = Math.max(0, time - lastTime)
  animTime = time
  if (RAF_CALLBACKS.length > 0) {
    requestAnimationFrame(tick)
    for (const callback of RAF_CALLBACKS) {
      callback.fn(time, delta)
    }
  } else {
    rafRunning = false
  }
}

/** Run the continuous loop using {@link tick}, if it is not already running. */
function runRaf() {
  if (!rafRunning) {
    rafRunning = true
    animTime = window.performance.now()
    requestAnimationFrame(tick)
  }
}

// ===================
// === useApproach ===
// ===================

/**
 * Animate value over time using exponential approach.
 * http://badladns.com/stories/exp-approach
 * @param target - Target value to approach.
 * @param timeHorizon - Time at which the approach will be at 63% of the target value. Effectively
 * represents a speed of the approach. Lower values means faster animation.
 * @param epsilon - The approach will stop when the difference between the current value and the
 * target value is less than `epsilon`. This is to prevent the animation from running forever.
 */
export function useApproach(
  target: number,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  timeHorizon: number = 100,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  epsilon = 0.005,
) {
  const [value, setValue] = React.useState(target)
  const targetRef = React.useRef(target)

  React.useEffect(() => {
    setValue(target)
    targetRef.current = target
  }, [target])

  useRaf(target !== value, (_, dt) => {
    const diff = value - target
    setValue(Math.abs(diff) <= epsilon ? target : target + diff / Math.exp(dt / timeHorizon))
  })

  const skip = React.useCallback(() => {
    setValue(targetRef.current)
  }, [])

  return { value, skip }
}
