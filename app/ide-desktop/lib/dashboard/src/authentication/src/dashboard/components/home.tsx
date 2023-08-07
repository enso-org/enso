/** @file Home screen. */
import * as React from 'react'

import * as spinner from './spinner'
import Templates from './templates'

// ============
// === Home ===
// ============

/** Props for a {@link Home}. */
export interface HomeProps {
    visible: boolean
    onTemplateClick: (
        name: string | null,
        onSpinnerStateChange: (state: spinner.SpinnerState | null) => void
    ) => void
}

/** Home screen. */
export default function Home(props: HomeProps) {
    const { visible, onTemplateClick } = props
    return (
        <div className={`${visible ? '' : 'hidden'}`}>
            <Templates onTemplateClick={onTemplateClick} />
        </div>
    )
}
