/**
 * @file A spinning arc that animates using the `dasharray-<percentage>` custom Tailwind
 * classes.
 */
import * as React from 'react'
import { twJoin } from 'tailwind-merge'

/** The state of the spinner. It should go from `initial`, to `loading`, to `done`. */
export type SpinnerPhase = 'done' | 'initial' | 'loading-fast' | 'loading-medium' | 'loading-slow'

/** The default size of the spinner. */
export const ROTATING_ELEMENT_SIZE = 24

const SPINNER_CSS_CLASSES: Readonly<Record<SpinnerPhase, string>> = {
  initial: 'dasharray-5 ease-linear',
  /* eslint-disable-next-line @typescript-eslint/naming-convention */
  'loading-slow': 'dasharray-75 duration-spinner-slow ease-linear',
  /* eslint-disable-next-line @typescript-eslint/naming-convention */
  'loading-medium': 'dasharray-75 duration-spinner-medium ease-linear',
  /* eslint-disable-next-line @typescript-eslint/naming-convention */
  'loading-fast': 'dasharray-75 duration-spinner-fast ease-linear',
  done: 'dasharray-100 duration-spinner-fast ease-in',
}

/** Props for a {@link Spinner}. */
export interface SpinnerProps {
  readonly size?: number
  readonly padding?: number
  readonly className?: string
  readonly phase: SpinnerPhase
  readonly thickness?: number
}

/** A spinning arc that animates using the `dasharray-<percentage>` custom Tailwind classes. */

export const Spinner = React.memo(function Spinner(props: SpinnerProps) {
  const { size, padding, className, phase, thickness = 3 } = props

  const cssClasses = twJoin('pointer-events-none', className)

  return (
    <svg
      width={size}
      height={size}
      className={cssClasses}
      style={{ padding }}
      viewBox="0 0 24 24"
      fill="none"
      xmlns="http://www.w3.org/2000/svg"
      aria-hidden="true"
      data-testid="spinner"
    >
      <rect
        x={thickness / 2}
        y={thickness / 2}
        width={ROTATING_ELEMENT_SIZE - thickness}
        height={ROTATING_ELEMENT_SIZE - thickness}
        rx={ROTATING_ELEMENT_SIZE / 2 - thickness / 2}
        stroke="currentColor"
        strokeLinecap="round"
        strokeWidth={thickness}
        className={twJoin(
          'pointer-events-none origin-center !animate-spin-ease transition-stroke-dasharray',
          SPINNER_CSS_CLASSES[phase],
        )}
      />
    </svg>
  )
})
