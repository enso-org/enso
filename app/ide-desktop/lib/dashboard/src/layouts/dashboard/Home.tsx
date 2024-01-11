/** @file Home screen. */
import * as React from 'react'

import Samples from '#/layouts/dashboard/Samples'
import WhatsNew from '#/layouts/dashboard/WhatsNew'

import type * as spinner from '#/components/Spinner'

// ============
// === Home ===
// ============

/** Props for a {@link Home}. */
export interface HomeProps {
    hidden: boolean
    onTemplateClick: (
        name: string | null,
        onSpinnerStateChange: (state: spinner.SpinnerState | null) => void
    ) => void
}

/** Home screen. */
export default function Home(props: HomeProps) {
    const { hidden, onTemplateClick } = props
    return (
        <div
            className={`flex flex-col flex-1 overflow-auto scroll-hidden gap-12 ${
                hidden ? 'hidden' : ''
            }`}
        >
            {/* For spacing */}
            <div />
            {/* Header */}
            <div className="flex flex-col gap-2 px-9.75 py-2.25">
                <h1 className="self-center text-center leading-144.5 text-4xl py-0.5">
                    Welcome to Enso Community
                </h1>
                <h2 className="self-center text-center font-normal leading-144.5 text-xl py-0.5">
                    Explore templates, plugins, and data sources to kickstart your next big idea.
                </h2>
            </div>
            <WhatsNew />
            <Samples onTemplateClick={onTemplateClick} />
        </div>
    )
}
