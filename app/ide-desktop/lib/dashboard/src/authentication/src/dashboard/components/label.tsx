/** @file An label that can be applied to an asset. */
import * as React from 'react'

import * as backend from '../backend'

// =================
// === Constants ===
// =================

export const DEFAULT_LABEL_COLOR = backend.Color('rgba(255, 255, 255, 0.70)')

// =============
// === Label ===
// =============

/** Props for a {@link Label}. */
interface InternalLabelProps extends React.PropsWithChildren {
    /** When true, the button is not faded out even when not hovered. */
    active?: boolean
    color: backend.Color
    className?: string
    onClick: React.MouseEventHandler<HTMLDivElement>
}

/** An label that can be applied to an asset. */
export default function Label(props: InternalLabelProps) {
    const { active = false, color, className = 'text-tag-text', onClick, children } = props
    return (
        <div
            className={`cursor-pointer flex items-center rounded-full gap-1.5 h-6 px-2.25 hover:opacity-100 ${className} ${
                active ? '' : 'text-not-selected opacity-50'
            }`}
            style={{ backgroundColor: color }}
            onClick={onClick}
        >
            {children}
        </div>
    )
}
