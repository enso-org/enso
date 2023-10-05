/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import * as modalProvider from '../../providers/modal'

import Modal from './modal'

// =================
// === Constants ===
// =================

/** The default offset (up and to the right) of the drag element. */
const DEFAULT_OFFSET_PX = 16

// =================
// === DragModal ===
// =================

/** Props for a {@link DragModal}. */
export interface DragModalProps extends React.PropsWithChildren<JSX.IntrinsicElements['div']> {
    event: React.DragEvent
    offsetPx?: number
    offsetXPx?: number
    offsetYPx?: number
}

/** A modal for confirming the deletion of an asset. */
export default function DragModal(props: DragModalProps) {
    const {
        event,
        offsetPx,
        offsetXPx = DEFAULT_OFFSET_PX,
        offsetYPx = DEFAULT_OFFSET_PX,
        children,
        style,
        className,
        ...passthrough
    } = props
    const { unsetModal } = modalProvider.useSetModal()
    const [left, setLeft] = React.useState(event.pageX - (offsetPx ?? offsetXPx))
    const [top, setTop] = React.useState(event.pageY - (offsetPx ?? offsetYPx))

    React.useEffect(() => {
        const onMouseMove = (moveEvent: MouseEvent) => {
            setLeft(oldLeft => oldLeft + moveEvent.movementX)
            setTop(oldTop => oldTop + moveEvent.movementY)
        }
        const onMouseUp = () => {
            unsetModal()
        }
        document.addEventListener('mousemove', onMouseMove)
        document.addEventListener('mouseup', onMouseUp)
        return () => {
            document.removeEventListener('mousemove', onMouseMove)
            document.removeEventListener('mouseup', onMouseUp)
        }
    }, [/* should never change */ unsetModal])

    return (
        <Modal className="absolute overflow-hidden pointer-events-none w-full h-full">
            <div
                {...passthrough}
                style={{ left, top, ...style }}
                className={`sticky w-min ${className ?? ''}`}
            >
                {children}
            </div>
        </Modal>
    )
}
