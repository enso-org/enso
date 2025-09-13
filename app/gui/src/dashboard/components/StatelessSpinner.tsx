/** @file A spinner that does not expose its {@link SpinnerPhase}. */
import { startTransition, useEffect, useState } from 'react'

import type { SpinnerPhase, SpinnerProps } from '#/components/Spinner'
import { Spinner } from '#/components/Spinner'
export type { SpinnerPhase as SpinnerState } from '#/components/Spinner'

/** Props for a {@link StatelessSpinner}. */
export type StatelessSpinnerProps = SpinnerProps

/**
 * A spinner that does not expose its {@link SpinnerPhase}. Instead, it begins at
 * `initial` and immediately changes to the given state.
 */
export function StatelessSpinner(props: StatelessSpinnerProps) {
  const { phase: rawPhase, ...spinnerProps } = props

  const [phase, setPhase] = useState<SpinnerPhase>('initial')

  useEffect(() => {
    const id = requestAnimationFrame(() => {
      // consider this as a low-priority update
      startTransition(() => {
        setPhase(rawPhase)
      })
    })

    return () => {
      cancelAnimationFrame(id)
    }
  }, [rawPhase])

  return <Spinner phase={phase} {...spinnerProps} />
}
