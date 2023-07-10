/** @file A spinning arc that animates using the `dasharray-<percentage>` custom Tailwind
 * classes. */
import * as React from 'react'

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

export const SPINNER_CSS_CLASSES: Record<SpinnerState, string> = {
    [SpinnerState.initial]: 'dasharray-5 ease-linear',
    [SpinnerState.loadingSlow]: 'dasharray-75 duration-90000 ease-linear',
    [SpinnerState.loadingMedium]: 'dasharray-75 duration-5000 ease-linear',
    [SpinnerState.loadingFast]: 'dasharray-75 duration-1000 ease-linear',
    [SpinnerState.done]: 'dasharray-100 duration-1000 ease-in',
} as const

/** Props for a {@link Spinner}. */
export interface SpinnerProps {
    size: number
    state: SpinnerState
}

/** A spinning arc that animates using the `dasharray-<percentage>` custom Tailwind classes. */
function Spinner(props: SpinnerProps) {
    const { size, state } = props
    return (
        <svg
            width={size}
            height={size}
            viewBox="0 0 24 24"
            fill="none"
            xmlns="http://www.w3.org/2000/svg"
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
                className={
                    'animate-spin-ease origin-center transition-stroke-dasharray ' +
                    SPINNER_CSS_CLASSES[state]
                }
            />
        </svg>
    )
}

export default Spinner
