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
        <div className="relative rounded-2xl pointer-events-auto">
            <div className="absolute rounded-2xl bg-frame-selected backdrop-blur-3xl w-full h-full -z-10" />
            <div
                className="flex flex-col rounded-2xl w-57.5 px-4 py-2"
                onClick={clickEvent => {
                    clickEvent.stopPropagation()
                }}
            >
                {children}
            </div>
        </div>
    )
}
