/** @file A component that renders the modal instance from the
 * {@link modalProvider.ModalContext}. */
import * as React from 'react'

import * as modalProvider from '../../providers/modal'

// ================
// === TheModal ===
// ================

/** Renders the modal instance from the {@link modalProvider.ModalContext} (if any). */
function TheModal() {
    const { modal } = modalProvider.useModal()

    return <>{modal}</>
}

export default TheModal
