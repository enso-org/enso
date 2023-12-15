/** @file An label that can be applied to an asset. */
import * as React from 'react'

import * as backend from '../backend'

// =================
// === Constants ===
// =================

/** The default color for labels (Light blue). */
export const DEFAULT_LABEL_COLOR: backend.LChColor = {
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    lightness: 100,
    chroma: 0,
    hue: 0,
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    alpha: 70,
}

// =============
// === Label ===
// =============

/** Props for a {@link Label}. */
interface InternalLabelProps
    extends React.PropsWithChildren,
        Omit<JSX.IntrinsicElements['button'], 'color' | 'onClick'>,
        Required<Pick<JSX.IntrinsicElements['button'], 'onClick'>> {
    /** When true, the button is not faded out even when not hovered. */
    active?: boolean
    /** When true, the button has a red border signifying that it will be deleted,
     * or that it is excluded from search. */
    negated?: boolean
    /** When true, the button cannot be clicked. */
    disabled?: boolean
    color: backend.LChColor
    className?: string
}

/** An label that can be applied to an asset. */
export default function Label(props: InternalLabelProps) {
    const {
        active = false,
        disabled = false,
        color,
        negated = false,
        className = 'text-tag-text',
        children,
        ...passthrough
    } = props
    const textColorClassName = /\btext-/.test(className)
        ? '' // eslint-disable-next-line @typescript-eslint/no-magic-numbers
        : color.lightness <= 50
        ? 'text-tag-text'
        : active
        ? 'text-primary'
        : 'text-not-selected'
    return (
        <button
            disabled={disabled}
            className={`flex items-center rounded-full gap-1.5 h-6 px-2.25 ${className} ${
                negated
                    ? 'relative before:absolute before:rounded-full before:border-2 before:border-delete before:inset-0 before:w-full before:h-full'
                    : ''
            } ${active ? '' : 'opacity-50'} ${
                disabled ? '' : 'group-hover:opacity-100'
            } ${textColorClassName}`}
            style={{ backgroundColor: backend.lChColorToCssColor(color) }}
            {...passthrough}
        >
            {children}
        </button>
    )
}
