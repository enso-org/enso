/** @file Base modal component that provides the full-screen element that blocks mouse events. */
import * as react from 'react'

import * as modalProvider from '../../providers/modal'

// =================
// === Component ===
// =================

export interface ModalProps extends react.PropsWithChildren {
    centered?: boolean
    className?: string
}

function Modal(props: ModalProps) {
    const { children, centered, className } = props
    const { unsetModal } = modalProvider.useSetModal()

    return (
        <div
            className={`inset-0 bg-primary ${
                centered ? 'fixed w-screen h-screen grid place-items-center ' : ''
            }${className ?? ''}`}
            onClick={event => {
                if (event.currentTarget === event.target) {
                    unsetModal()
                }
            }}
        >
            {children}
        </div>
    )
}

export default Modal
