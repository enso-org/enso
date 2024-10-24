/** @file A spinner that does not expose its {@link spinner.SpinnerState}. */
import * as React from 'react'

import Spinner, * as spinner from '#/components/Spinner'

// ========================
// === StatelessSpinner ===
// ========================

export { SpinnerState } from './Spinner'

/** Props for a {@link StatelessSpinner}. */
export type StatelessSpinnerProps = spinner.SpinnerProps

/**
 * A spinner that does not expose its {@link spinner.SpinnerState}. Instead, it begins at
 * {@link spinner.SpinnerState.initial} and immediately changes to the given state.
 */
export default function StatelessSpinner(props: StatelessSpinnerProps) {
  const { size, state: rawState, ...spinnerProps } = props
  const [, startTransition] = React.useTransition()
  const [state, setState] = React.useState(spinner.SpinnerState.initial)

  React.useLayoutEffect(() => {
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

  return <Spinner state={state} {...(size != null ? { size } : {})} {...spinnerProps} />
}
