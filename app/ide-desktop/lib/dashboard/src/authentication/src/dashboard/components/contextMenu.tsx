/** @file A context menu. */

import * as React from 'react'

// =================
// === Component ===
// =================

/** Props for a {@link ContextMenu}. */
export interface ContextMenuProps {
    // `left: number` and `top: number` may be more correct,
    // however passing an event eliminates the chance
    // of passing the wrong coordinates from the event.
    event: React.MouseEvent
}

/** A context menu that opens at the current mouse position. */
function ContextMenu(props: React.PropsWithChildren<ContextMenuProps>) {
    const { children, event } = props
    return (
        <div
            style={{ left: event.pageX, top: event.pageY }}
            className="absolute bg-white rounded-lg shadow-soft flex flex-col flex-nowrap"
        >
            {children}
        </div>
    )
}

export default ContextMenu
