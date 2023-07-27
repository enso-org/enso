/** @file A context menu. */
import * as React from 'react'

// ===================
// === ContextMenu ===
// ===================

/** Props for a {@link ContextMenu}. */
export interface ContextMenuProps extends React.PropsWithChildren {}

/** A context menu that opens at the current mouse position. */
export default function ContextMenu(props: ContextMenuProps) {
    const { children } = props

    return (
        <div
            onClick={clickEvent => {
                clickEvent.stopPropagation()
            }}
        >
            {children}
        </div>
    )
}
