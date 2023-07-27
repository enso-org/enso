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
        <div className="relative">
            <div className="absolute rounded-2xl bg-frame-selected backdrop-blur-3xl w-full h-full -z-10" />
            <div
                className="flex flex-col w-57.5 px-4 py-2"
                onClick={clickEvent => {
                    clickEvent.stopPropagation()
                }}
            >
                {children}
            </div>
        </div>
    )
}
