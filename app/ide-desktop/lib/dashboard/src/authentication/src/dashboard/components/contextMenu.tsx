/** @file A context menu. */

import * as react from 'react'

// =================
// === Constants ===
// =================

/** The margin around the context menu, so that it is not at the edge of the screen. */
const SCROLL_MARGIN = 12

// ===================
// === ContextMenu ===
// ===================

/** Props for a {@link ContextMenu}. */
export interface ContextMenuProps {
    // `left: number` and `top: number` may be more correct,
    // however passing an event eliminates the chance
    // of passing the wrong coordinates from the event.
    event: react.MouseEvent
}

/** A context menu that opens at the current mouse position. */
function ContextMenu(props: react.PropsWithChildren<ContextMenuProps>) {
    const { children, event } = props
    const contextMenuRef = react.useRef<HTMLDivElement>(null)

    react.useEffect(() => {
        if (contextMenuRef.current != null) {
            const boundingBox = contextMenuRef.current.getBoundingClientRect()
            const scrollBy = boundingBox.bottom - innerHeight + SCROLL_MARGIN
            if (scrollBy > 0) {
                scroll(scrollX, scrollY + scrollBy)
            }
        }
    }, [children])

    return (
        <div
            ref={contextMenuRef}
            style={{ left: event.pageX, top: event.pageY }}
            className="absolute bg-white rounded-lg shadow-soft flex flex-col flex-nowrap m-2"
        >
            {children}
        </div>
    )
}

export default ContextMenu
