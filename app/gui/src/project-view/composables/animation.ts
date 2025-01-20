/** @file Vue composables for running a callback on every frame, and smooth interpolation. */

import type { Vec2 } from '@/util/data/vec2'
import { watchSourceToRef } from '@/util/reactivity'
import {
  onScopeDispose,
  proxyRefs,
  readonly,
  ref,
  shallowRef,
  watch,
  type Ref,
  type WatchSource,
} from 'vue'

const rafCallbacks: { fn: (t: number, dt: number) => void; priority: number }[] = []

const animTime = ref(0)

/**
 * Use `requestAnimationFrame` API to perform animation. The callback will be called every frame
 * while the `active` watch source returns true value.
 *
 * For performing simple easing animations, see [`useApproach`].
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

  watch(
    active,
    (isActive, previous) => {
      if (isActive === previous) return
      if (isActive) {
        mountRaf()
      } else {
        unmountRaf()
      }
    },
    { immediate: true },
  )
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

/**
 * Animate value over time using exponential approach.
 * http://badladns.com/stories/exp-approach
 * @param to Target value to approach.
 * @param timeHorizon Time at which the approach will be at 63% of the target value. Effectively
 * represents a speed of the approach. Lower values means faster animation.
 * @param epsilon The approach will stop when the difference between the current value and the
 * target value is less than `epsilon`. This is to prevent the animation from running forever.
 */
export function useApproach(to: WatchSource<number>, timeHorizon: number = 100, epsilon = 0.005) {
  return useApproachBase(
    to,
    (t, c) => t == c,
    (targetVal, currentValue, dt) => {
      const diff = currentValue - targetVal
      if (Math.abs(diff) > epsilon) {
        return targetVal + diff / Math.exp(dt / timeHorizon)
      } else {
        return targetVal
      }
    },
  )
}

/**
 * Animate a vector value over time using exponential approach.
 * http://badladns.com/stories/exp-approach
 * @param to Target vector value to approach.
 * @param timeHorizon Time at which the approach will be at 63% of the target value. Effectively
 * represents a speed of the approach. Lower values means faster animation.
 * @param epsilon The approach will stop when the squared distance between the current vector and
 * the target value is less than `epsilon`. This is to prevent the animation from running forever.
 */
export function useApproachVec(to: WatchSource<Vec2>, timeHorizon: number = 100, epsilon = 0.003) {
  return useApproachBase(
    to,
    (t, c) => t.equals(c),
    (targetVal, currentValue, dt) => {
      const diff = currentValue.sub(targetVal)
      if (diff.lengthSquared() > epsilon) {
        return targetVal.add(diff.scale(1 / Math.exp(dt / timeHorizon)))
      } else {
        return targetVal
      }
    },
  )
}

function useApproachBase<T>(
  to: WatchSource<T>,
  stable: (target: T, current: T) => boolean,
  update: (target: T, current: T, dt: number) => T,
) {
  const target = watchSourceToRef(to)
  const current: Ref<T> = shallowRef(target.value)

  useRaf(
    () => !stable(target.value, current.value),
    (_, dt) => {
      current.value = update(target.value, current.value, dt)
    },
  )

  function skip() {
    current.value = target.value
  }

  return readonly(proxyRefs({ value: current, skip }))
}

/** TODO: Add docs */
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
