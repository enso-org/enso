/** @file A context menu. */
import * as React from 'react'

import Modal from './modal'

// =================
// === Constants ===
// =================

/** The margin around the context menu, so that it is not at the edge of the screen. */
const SCROLL_MARGIN = 12

// ===================
// === ContextMenu ===
// ===================

/** Props for a {@link ContextMenus}. */
export interface ContextMenusProps extends React.PropsWithChildren {
    key: string
    // `left: number` and `top: number` may be more correct,
    // however passing an event eliminates the chance
    // of passing the wrong coordinates from the event.
    event: React.MouseEvent
}

/** A context menu that opens at the current mouse position. */
export default function ContextMenus(props: ContextMenusProps) {
    const { children, event } = props
    const contextMenuRef = React.useRef<HTMLDivElement>(null)
    const [left, setLeft] = React.useState(event.pageX)
    const [top, setTop] = React.useState(event.pageY)
    // This must be the original height before the returned element affects the `scrollHeight`.
    const [bodyHeight] = React.useState(document.body.scrollHeight)

    React.useEffect(() => {
        if (contextMenuRef.current != null) {
            setTop(Math.min(top, bodyHeight - contextMenuRef.current.clientHeight))
            const boundingBox = contextMenuRef.current.getBoundingClientRect()
            setLeft(event.pageX - boundingBox.width / 2)
            const scrollBy = boundingBox.bottom - innerHeight + SCROLL_MARGIN
            if (scrollBy > 0) {
                scroll(scrollX, scrollY + scrollBy)
            }
        }
    }, [bodyHeight, children, top, event.pageX])

    return (
        <Modal className="absolute overflow-hidden bg-dim w-full h-full top-0 left-0 z-10">
            <div
                ref={contextMenuRef}
                // The location must be offset by -0.5rem to balance out the `m-2`.
                style={{ left: `calc(${left}px - 0.5rem)`, top: `calc(${top}px - 0.5rem)` }}
                className="sticky bg-white rounded-lg shadow-soft flex flex-col flex-nowrap m-2"
                onClick={clickEvent => {
                    clickEvent.stopPropagation()
                }}
            >
                {children}
            </div>
        </Modal>
    )
}
