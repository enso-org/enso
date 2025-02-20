/** @file A spinner that does not expose its {@link SpinnerState}. */
import { startTransition, useEffect, useState } from 'react'

import type { SpinnerProps, SpinnerState } from '#/components/Spinner'
import { Spinner } from '#/components/Spinner'
export type { SpinnerState } from '#/components/Spinner'

/** Props for a {@link StatelessSpinner}. */
export type StatelessSpinnerProps = SpinnerProps

/**
 * A spinner that does not expose its {@link SpinnerState}. Instead, it begins at
 * `initial` and immediately changes to the given state.
 */
export function StatelessSpinner(props: StatelessSpinnerProps) {
  const { state: rawState, ...spinnerProps } = props

  const [state, setState] = useState<SpinnerState>('initial')

  useEffect(() => {
    const id = requestAnimationFrame(() => {
      // consider this as a low-priority update
      startTransition(() => {
        setState(rawState)
      })
    })

    return () => {
      cancelAnimationFrame(id)
    }
  }, [rawState])

  return <Spinner state={state} {...spinnerProps} />
}
