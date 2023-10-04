/** @file An label that can be applied to an asset. */
import * as React from 'react'

// =============
// === Label ===
// =============

/** Props for a {@link Label}. */
interface InternalLabelProps extends React.PropsWithChildren {
    /** When true, the button is not faded out even when not hovered. */
    active?: boolean
    className?: string
    onClick: React.MouseEventHandler<HTMLDivElement>
}

/** An label that can be applied to an asset. */
export default function Label(props: InternalLabelProps) {
    const {
        active = false,
        className = 'bg-frame-selected text-primary',
        onClick,
        children,
    } = props
    return (
        <div
            className={`cursor-pointer flex items-center rounded-full gap-1.5 h-6 px-2.25 hover:opacity-100 ${className} ${
                active ? 'bg-frame-selected' : 'text-not-selected opacity-50'
            }`}
            onClick={onClick}
        >
            {children}
        </div>
    )
}
