/** @file A styled input that includes an icon. */
import * as React from 'react'

import type * as controlledInput from './controlledInput'
import ControlledInput from './controlledInput'
import SvgIcon from './svgIcon'

// =============
// === Input ===
// =============

/** Props for a {@link Input}. */
export interface InputProps extends controlledInput.ControlledInputProps {
    label: string | null
    icon: string
    footer?: React.ReactNode
}

/** A styled input that includes an icon. */
export default function Input(props: InputProps) {
    const { label, icon, footer, ...passthrough } = props
    const input = (
        <div className="relative">
            <SvgIcon src={icon} />
            <ControlledInput {...passthrough} />
        </div>
    )
    return label != null || footer != null ? (
        <label className="flex flex-col gap-1">
            {label}
            {input}
            {footer}
        </label>
    ) : (
        input
    )
}
