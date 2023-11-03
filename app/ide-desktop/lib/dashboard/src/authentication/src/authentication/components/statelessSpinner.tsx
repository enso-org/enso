/** @file A spinner that does not expose its {@link spinner.SpinnerState}. */
import * as React from 'react'

import Spinner, * as spinner from './spinner'

// ========================
// === StatelessSpinner ===
// ========================

// This is a re-export, so that the API of this module mirrors that of the `spinner` module.
// eslint-disable-next-line no-restricted-syntax
export { SpinnerState } from './spinner'

/** Props for a {@link StatelessSpinner}. */
export interface StatelessSpinnerProps extends spinner.SpinnerProps {}

/** A spinner that does not expose its {@link spinner.SpinnerState}. Instead, it begins at
 * {@link spinner.SpinnerState.initial} and immediately changes to the given state. */
export default function StatelessSpinner(props: StatelessSpinnerProps) {
    const { size, state: rawState } = props
    const [state, setState] = React.useState(spinner.SpinnerState.initial)

    React.useEffect(() => {
        window.setTimeout(() => {
            setState(rawState)
        })
    }, [/* should never change */ rawState])

    return <Spinner size={size} state={state} />
}
