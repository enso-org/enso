/** @file Input element with default event handlers. */
import * as react from 'react'

// =============
// === Input ===
// =============

/** Props for an {@link Input}. */
export interface InputProps extends react.InputHTMLAttributes<HTMLInputElement> {
    setValue: (value: string) => void
}

/** A component for authentication from inputs, with preset styles. */
function Input(props: InputProps) {
    const { setValue, ...passThrough } = props
    return (
        <input
            {...passThrough}
            onChange={event => {
                setValue(event.target.value)
            }}
            onBlur={() => {
                getSelection()?.empty()
            }}
        />
    )
}

export default Input
