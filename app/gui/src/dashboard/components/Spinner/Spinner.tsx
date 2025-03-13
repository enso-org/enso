/**
 * @file A spinning arc that animates using the `dasharray-<percentage>` custom Tailwind
 * classes.
 */
import * as React from 'react'
import { twJoin } from 'tailwind-merge'
import { SPINNER_CSS_CLASSES, type SpinnerState } from './constants'

/** Props for a {@link Spinner}. */
export interface SpinnerProps {
  readonly size?: number
  readonly padding?: number
  readonly className?: string
  readonly state: SpinnerState
}

/** A spinning arc that animates using the `dasharray-<percentage>` custom Tailwind classes. */
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

/** Props for a {@link IndefiniteSpinner}. */
export interface IndefiniteSpinnerProps extends Omit<SpinnerProps, 'state'> {}

/** A spinning arc that animates indefinitely. */
export function IndefiniteSpinner(props: IndefiniteSpinnerProps) {
  const { size, padding, className } = props

  const cssClasses = twJoin(
    'pointer-events-none flex-none contain-strict h-10 w-10 animate-spin ease-in-out rounded-full border-4 border-primary/10 border-l-primary',
    className,
  )

  return <div className={cssClasses} style={{ padding, width: size, height: size }} />
}
