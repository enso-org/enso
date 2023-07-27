/** @file An entry in a context menu. */
import * as React from 'react'

import * as shortcuts from '../shortcuts'

// ========================
// === ContextMenuEntry ===
// ========================

/** Props for a {@link ContextMenuEntry}. */
export interface ContextMenuEntryProps {
    /** The URL to the icon representing the action. */
    icon: string
    shortcut: shortcuts.KeyboardShortcut
    /** The label of the {@link ContextMenuEntry}. This should usually be a string. */
    children: React.ReactNode
    disabled?: boolean
    title?: string
    onClick: (event: React.MouseEvent<HTMLButtonElement>) => void
}

/** An item in a `ContextMenu`. */
export default function ContextMenuEntry(props: ContextMenuEntryProps) {
    const { icon, shortcut, children, disabled = false, title, onClick } = props
    return (
        <button
            disabled={disabled}
            title={title}
            className="hover:bg-black-a10 first:rounded-t-2xl last:rounded-b-2xl text-left disabled:opacity-50 p-1"
            onClick={event => {
                event.stopPropagation()
                onClick(event)
            }}
        >
            <img src={icon} /> {children} {shortcut}
        </button>
    )
}
