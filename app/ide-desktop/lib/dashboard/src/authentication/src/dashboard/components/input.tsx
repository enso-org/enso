/** @file Input element with default event handlers. */
import * as React from 'react'

// =============
// === Input ===
// =============

/** Props for an `<input>` HTML element/ */
type InputAttributes = JSX.IntrinsicElements['input']

/** Props for an {@link Input}. */
export interface InputProps extends InputAttributes {
    setValue: (value: string) => void
}

/** A component for authentication from inputs, with preset styles. */
export default function Input(props: InputProps) {
    const { setValue, onChange, ...passthroughProps } = props
    return (
        <input
            {...passthroughProps}
            onChange={event => {
                onChange?.(event)
                setValue(event.target.value)
            }}
            onBlur={() => {
                getSelection()?.empty()
            }}
        />
    )
}
