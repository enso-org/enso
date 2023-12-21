/** @file A component that renders the modal instance from the modal React Context. */
import * as React from 'react'

import * as providers from '#/providers'

// ================
// === TheModal ===
// ================

/** Renders the modal instance from the modal React Context (if any). */
export default function TheModal() {
    const { modal } = providers.useModal()

    return <>{modal}</>
}
