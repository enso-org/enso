/**
 * @file A spinning arc that animates using the `dasharray-<percentage>` custom Tailwind
 * classes.
 */
import * as React from 'react'
import { twJoin } from 'tailwind-merge'

/** The state of the spinner. It should go from `initial`, to `loading`, to `done`. */
export type SpinnerState = 'done' | 'initial' | 'loading-fast' | 'loading-medium' | 'loading-slow'

export const SPINNER_CSS_CLASSES: Readonly<Record<SpinnerState, string>> = {
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
  readonly state: SpinnerState
}

/** A spinning arc that animates using the `dasharray-<percentage>` custom Tailwind classes. */
// eslint-disable-next-line no-restricted-syntax
export const Spinner = React.memo(function Spinner(props: SpinnerProps) {
  const { size, padding, className, state } = props

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
        x={1.5}
        y={1.5}
        width={21}
        height={21}
        rx={10.5}
        stroke="currentColor"
        strokeLinecap="round"
        strokeWidth={3}
        className={twJoin(
          'pointer-events-none origin-center !animate-spin-ease transition-stroke-dasharray [transition-duration:var(--spinner-slow-transition-duration)]',
          SPINNER_CSS_CLASSES[state],
        )}
      />
    </svg>
  )
})

/**
 * Props for a {@link IndefiniteSpinner}.
 */
export interface IndefiniteSpinnerProps extends Omit<SpinnerProps, 'state'> {}

/**
 * A spinning arc that animates indefinitely.
 */
export function IndefiniteSpinner(props: IndefiniteSpinnerProps) {
  const { size, padding, className } = props

  const cssClasses = twJoin(
    'pointer-events-none flex-none contain-strict h-10 w-10 animate-spin ease-in-out rounded-full border-4 border-primary/10 border-l-primary',
    className,
  )

  return <div className={cssClasses} style={{ padding, width: size, height: size }} />
}
