/** @file Functions to manually animate values over time.
 * This is useful if the values need to be known before paint.
 *
 * See MDN for information on the easing functions defined in this module:
 * https://developer.mozilla.org/en-US/docs/Web/CSS/easing-function */
import * as react from 'react'

// =================
// === Constants ===
// =================

/** The number of times the segment from 0 to 1 will be bisected to find the x-value for
 * a cubic bezier curve. */
const CUBIC_BEZIER_BISECTIONS = 10

// ==============================
// === useInterpolateOverTime ===
// ==============================

/** Accepts a parameter containing the actual progress as a fraction between 0 and 1 inclusive,
 * and returns the fraction. */
export type InterpolationFunction = (progress: number) => number

/** Interpolates between two values over time */
export function useInterpolateOverTime(
  interpolationFunction: InterpolationFunction,
  durationMs: number,
  initialValue = 0
): [value: number, setTargetValue: react.Dispatch<react.SetStateAction<number>>] {
  const [value, setValue] = react.useState(initialValue)
  const [startValue, setStartValue] = react.useState(initialValue)
  const [endValue, setEndValue] = react.useState(initialValue)

  react.useEffect(() => {
    setStartValue(value)
    const startTime = Number(new Date())
    let isRunning = true
    const onTick = () => {
      const fraction = Math.min((Number(new Date()) - startTime) / durationMs, 1)
      if (isRunning && fraction < 1) {
        setValue(startValue + (endValue - startValue) * interpolationFunction(fraction))
        requestAnimationFrame(onTick)
      } else {
        setValue(endValue)
        setStartValue(endValue)
      }
    }
    requestAnimationFrame(onTick)
    return () => {
      isRunning = false
    }
  }, [endValue])

  return [value, setEndValue]
}

/** Equivalent to the CSS easing function `linear(a, b, c, ...)`.
 *
 * `interpolationFunctionLinear()` is equivalent to `interpolationFunctionLinear(0, 1)`.
 *
 * Does not support percentages to control time spent on a specific line segment, unlike the CSS
 * `linear(0, 0.25 75%, 1)` */
export function interpolationFunctionLinear(...points: number[]): InterpolationFunction {
  if (points.length === 0) {
    return progress => progress
  } else {
    const length = points.length
    return progress => {
      const effectiveIndex = progress * length
      // The following are guaranteed to be non-null, as `progress` is guaranteed
      // to be between 0 and 1.
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      const start = points[Math.floor(effectiveIndex)]!
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      const end = points[Math.ceil(effectiveIndex)]!
      const progressThroughEffectiveIndex = effectiveIndex % 1
      return (end - start) * progressThroughEffectiveIndex
    }
  }
}

/** Defines a cubic Bézier curve with control points `(0, 0)`, `(x1, y1)`, `(x2, y2)`,
 * and `(1, 1)`.
 *
 * Equivalent to the CSS easing function `cubic-bezier(x1, y1, x2, y2)` */
export function interpolationFunctionCubicBezier(
  x1: number,
  y1: number,
  x2: number,
  y2: number
): InterpolationFunction {
  return progress => {
    let minimum = 0
    let maximum = 1
    for (let i = 0; i < CUBIC_BEZIER_BISECTIONS; ++i) {
      const t = (minimum + maximum) / 2
      const u = 1 - t
      // See here for the source of the explicit form:
      // https://en.wikipedia.org/wiki/B%C3%A9zier_curve#Cubic_B%C3%A9zier_curves
      // `x0 = 0` and `x3 = 1` have been substituted in.
      // eslint-disable-next-line @typescript-eslint/no-magic-numbers
      const estimatedProgress = 3 * u * t * (u * x1 + t * x2) + t * t * t
      if (estimatedProgress > progress) {
        maximum = t
      } else {
        minimum = t
      }
    }
    const t = (minimum + maximum) / 2
    const u = 1 - t
    // Uses the same formula as above, but calculating `y` instead of `x`.
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    return 3 * u * t * (u * y1 + t * y2) + t * t * t
  }
}

// Magic numbers are allowable as these definitions are taken straight from the spec.
/* eslint-disable @typescript-eslint/no-magic-numbers */

/** Equivalent to the CSS easing function `ease`, which is itself equivalent to
 * `cubic-bezier(0.25, 0.1, 0.25, 1.0)`. */
export const interpolationFunctionEase = interpolationFunctionCubicBezier(0.25, 0.1, 0.25, 1.0)

/** Equivalent to the CSS easing function `ease-in`, which is itself equivalent to
 * `cubic-bezier(0.42, 0.0, 1.0, 1.0)`. */
export const interpolationFunctionEaseIn = interpolationFunctionCubicBezier(0.42, 0.0, 1.0, 1.0)

/** Equivalent to the CSS easing function `ease-in-out`, which is itself equivalent to
 * `cubic-bezier(0.42, 0.0, 0.58, 1.0)`. */
export const interpolationFunctionEaseInOut = interpolationFunctionCubicBezier(0.42, 0.0, 0.58, 1.0)

/** Equivalent to the CSS easing function `ease-out`, which is itself equivalent to
 * `cubic-bezier(0.0, 0.0, 0.58, 1.0)`. */
export const interpolationFunctionEaseOut = interpolationFunctionCubicBezier(0.0, 0.0, 0.58, 1.0)

/* eslint-enable @typescript-eslint/no-magic-numbers */

/** Determines which sides should have a "jump" - a step that lasts for zero time, effectively
 * making the anmiation skip that end point. */
export enum StepJumpSides {
  start = 'jump-start',
  end = 'jump-end',
  both = 'jump-both',
  none = 'jump-none',
}

/** Equivalent to the CSS easing function `steps(stepCount, jumpSides)`. */
export function interpolationFunctionSteps(
  stepCount: number,
  jumpSides: StepJumpSides
): InterpolationFunction {
  switch (jumpSides) {
    case StepJumpSides.start: {
      return progress => Math.ceil(progress * stepCount) / stepCount
    }
    case StepJumpSides.end: {
      return progress => Math.floor(progress * stepCount) / stepCount
    }
    case StepJumpSides.both: {
      const stepCountPlusOne = stepCount + 1
      return progress => Math.ceil(progress * stepCount) / stepCountPlusOne
    }
    case StepJumpSides.none: {
      const stepCountMinusOne = stepCount - 1
      return progress => Math.min(1, Math.floor(progress * stepCount) / stepCountMinusOne)
    }
  }
}

/** Equivalent to the CSS easing function `step-start`, which is itself equivalent to
 * `steps(1, jump-start)`. */
export const interpolationFunctionStepStart = interpolationFunctionSteps(1, StepJumpSides.start)

/** Equivalent to the CSS easing function `step-end`, which is itself equivalent to
 * `steps(1, jump-end)`. */
export const interpolationFunctionStepEnd = interpolationFunctionSteps(1, StepJumpSides.end)
