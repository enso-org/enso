/** @file Base modal component that provides the full-screen element that blocks mouse events. */
import * as react from 'react'

import * as modalProvider from '../../providers/modal'

// =================
// === Component ===
// =================

/** Props for a {@link Modal}. */
export interface ModalProps extends react.PropsWithChildren {
    centered?: boolean
    className?: string
}

/** A fullscreen modal with content at the center.
 * The background is fully opaque by default;
 * background transparency can be enabled with Tailwind's `bg-opacity` classes,
 * like `className="bg-opacity-50"` */
function Modal(props: ModalProps) {
    const { children, centered, className } = props
    const { unsetModal } = modalProvider.useSetModal()

    return (
        <div
            className={`inset-0 bg-primary ${
                centered ? 'fixed w-screen h-screen grid place-items-center ' : ''
            }${className ?? ''}`}
            onClick={event => {
                if (event.currentTarget === event.target && getSelection()?.type !== 'Range') {
                    event.stopPropagation()
                    unsetModal()
                }
            }}
        >
            {children}
        </div>
    )
}

export default Modal
