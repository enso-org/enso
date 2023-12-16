/** @file Vue composables for running a callback on every frame, and smooth interpolation. */

import { watchSourceToRef } from '@/util/reactivity'
import { onScopeDispose, proxyRefs, ref, watch, type WatchSource } from 'vue'

const rafCallbacks: { fn: (t: number, dt: number) => void; priority: number }[] = []

const animTime = ref(0)

/**
 * Use `requestAnimationFrame` API to perform animation. The callback will be called every frame
 * while the `active` watch source returns true value.
 *
 * For performing simple easing animations, see [`useApproach`].
 *
 * @param active As long as it returns true value, the `fn` callback will be called every frame.
 * @param fn The callback to call every animation frame.
 * @param priority When multiple callbacks are registered, the one with the lowest priority number
 * will be called first. Default priority is 0. For callbacks with the same priority, the order of
 * execution matches the order of registration.
 */
export function useRaf(
  active: WatchSource<boolean>,
  fn: (t: number, dt: number) => void,
  priority = 0,
): void {
  const callback = { fn, priority }
  function mountRaf() {
    const idx = rafCallbacks.findIndex((cb) => cb.priority > priority)
    if (idx >= 0) {
      rafCallbacks.splice(idx, 0, callback)
    } else {
      rafCallbacks.push(callback)
      runRaf()
    }
  }
  function unmountRaf() {
    const i = rafCallbacks.indexOf(callback)
    if (i >= 0) {
      rafCallbacks.splice(i, 1)
    }
  }

  watch(active, (isActive, previous) => {
    if (isActive === previous) return
    if (isActive) {
      mountRaf()
    } else {
      unmountRaf()
    }
  })
  onScopeDispose(unmountRaf)
}

let rafRunning = false
function tick(time: number) {
  const lastTime = animTime.value
  const delta = Math.max(0, time - lastTime)
  animTime.value = time
  if (rafCallbacks.length > 0) {
    requestAnimationFrame(tick)
    rafCallbacks.forEach((cb) => cb.fn(time, delta))
  } else {
    rafRunning = false
  }
}

function runRaf() {
  if (!rafRunning) {
    rafRunning = true
    animTime.value = window.performance.now()
    requestAnimationFrame(tick)
  }
}

const defaultDiffFn = (a: number, b: number): number => b - a

/**
 * Animate value over time using exponential approach.
 * http://badladns.com/stories/exp-approach
 *
 * @param to Target value to approach.
 * @param timeHorizon Time at which the approach will be at 63% of the target value. Effectively
 * represents a speed of the approach. Lower values means faster animation.
 * @param epsilon The approach will stop when the difference between the current value and the
 * target value is less than `epsilon`. This is to prevent the animation from running forever.
 * @param diffFn Function that will be used to calculate the difference between two values.
 * By default, the difference is calculated as simple number difference `b - a`.
 * Custom `diffFn` can be used to implement e.g. angle value approach over the shortest arc.
 */
export function useApproach(
  to: WatchSource<number>,
  timeHorizon: number = 100,
  epsilon = 0.005,
  diffFn = defaultDiffFn,
) {
  const target = watchSourceToRef(to)
  const current = ref(target.value)

  useRaf(
    () => target.value != current.value,
    (_, dt) => {
      const targetVal = target.value
      const currentValue = current.value
      if (targetVal != currentValue) {
        const diff = diffFn(targetVal, currentValue)
        if (Math.abs(diff) > epsilon) {
          current.value = targetVal + diff / Math.exp(dt / timeHorizon)
        } else {
          current.value = targetVal
        }
      }
    },
  )

  function skip() {
    current.value = target.value
  }

  return proxyRefs({ value: current, skip })
}

export function useTransitioning(observedProperties?: Set<string>) {
  const hasActiveAnimations = ref(false)
  let numActiveTransitions = 0
  function onTransitionStart(e: TransitionEvent) {
    if (!observedProperties || observedProperties.has(e.propertyName)) {
      if (numActiveTransitions == 0) hasActiveAnimations.value = true
      numActiveTransitions += 1
    }
  }

  function onTransitionEnd(e: TransitionEvent) {
    if (!observedProperties || observedProperties.has(e.propertyName)) {
      numActiveTransitions -= 1
      if (numActiveTransitions == 0) hasActiveAnimations.value = false
    }
  }

  return {
    active: hasActiveAnimations,
    events: {
      transitionstart: onTransitionStart,
      transitionend: onTransitionEnd,
      transitioncancel: onTransitionEnd,
    },
  }
}
