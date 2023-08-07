/** @file Home screen. */
import * as React from 'react'

import * as spinner from './spinner'
import Samples from './samples'
import WhatsNew from './whatsNew'

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
        <>
            {/* Header */}
            <div className={`flex flex-col gap-2 px-9.75 py-2.25 ${visible ? '' : 'hidden'}`}>
                <h1 className="self-center leading-144.5 text-4xl py-0.5">
                    Welcome to Enso Community
                </h1>
                <h2 className="self-center leading-144.5 text-xl py-0.5">
                    Explore templates, plugins, and data sources to kickstart your next big idea.
                </h2>
            </div>
            <WhatsNew visible={visible} />
            <Samples visible={visible} onTemplateClick={onTemplateClick} />
        </>
    )
}
