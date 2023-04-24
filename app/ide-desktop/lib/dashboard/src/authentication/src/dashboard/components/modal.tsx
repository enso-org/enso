/** @file Base modal component that provides the full-screen element that blocks mouse events. */
import * as react from 'react'

import * as modalProvider from '../../providers/modal'

// =================
// === Component ===
// =================

export interface ModalProps extends react.PropsWithChildren {
    className?: string
}

/** A fullscreen modal with content at the center.
 * The background is fully opaque by default;
 * background transparency can be enabled with Tailwind's `bg-opacity` classes,
 * like `className="bg-opacity-50"` */
function Modal(props: ModalProps) {
    const { children } = props
    const { unsetModal } = modalProvider.useSetModal()
    return (
        <div
            className={`fixed w-screen h-screen inset-0 bg-primary grid place-items-center ${
                props.className ?? ''
            }`}
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
