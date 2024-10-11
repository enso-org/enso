/**
 * @file A spinning arc that animates using the `dasharray-<percentage>` custom Tailwind
 * classes.
 */
import * as React from 'react'

import * as tailwindMerge from '#/utilities/tailwindMerge'

// ===============
// === Spinner ===
// ===============

/** The state of the spinner. It should go from initial, to loading, to done. */
export enum SpinnerState {
  initial = 'initial',
  loadingSlow = 'loading-slow',
  loadingMedium = 'loading-medium',
  loadingFast = 'loading-fast',
  done = 'done',
}

export const SPINNER_CSS_CLASSES: Readonly<Record<SpinnerState, string>> = {
  [SpinnerState.initial]: 'dasharray-5 ease-linear',
  [SpinnerState.loadingSlow]: 'dasharray-75 duration-spinner-slow ease-linear',
  [SpinnerState.loadingMedium]: 'dasharray-75 duration-spinner-medium ease-linear',
  [SpinnerState.loadingFast]: 'dasharray-75 duration-spinner-fast ease-linear',
  [SpinnerState.done]: 'dasharray-100 duration-spinner-fast ease-in',
}

/** Props for a {@link Spinner}. */
export interface SpinnerProps {
  readonly size?: number
  readonly padding?: number
  readonly className?: string
  readonly state: SpinnerState
}

/** A spinning arc that animates using the `dasharray-<percentage>` custom Tailwind classes. */
export default function Spinner(props: SpinnerProps) {
  const { size, padding, className, state } = props
  return (
    <svg
      width={size}
      height={size}
      className={className}
      style={{ padding }}
      viewBox="0 0 24 24"
      fill="none"
      xmlns="http://www.w3.org/2000/svg"
      aria-hidden="true"
    >
      <rect
        x={1.5}
        y={1.5}
        width={21}
        height={21}
        rx={10.5}
        stroke="currentColor"
        strokeLinecap="round"
        strokeWidth={3}
        className={tailwindMerge.twMerge(
          'pointer-events-none origin-center !animate-spin-ease transition-stroke-dasharray [transition-duration:var(--spinner-slow-transition-duration)]',
          SPINNER_CSS_CLASSES[state],
        )}
      />
    </svg>
  )
}
