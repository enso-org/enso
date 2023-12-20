/** @file A horizontal line dividing two sections in the context menu. */
import * as React from 'react'

// ============================
// === ContextMenuSeparator ===
// ============================

/** Props for a {@link ContextMenuSeparator}. */
export interface ContextMenuSeparatorProps {
    hidden?: boolean
}

/** A horizontal line dividing two sections in the context menu. */
export default function ContextMenuSeparator(props: ContextMenuSeparatorProps) {
    const { hidden = false } = props
    return hidden ? null : (
        <div className="py-0.5">
            <div className="border-t-0.5 border-black/[0.16]" />
        </div>
    )
}
