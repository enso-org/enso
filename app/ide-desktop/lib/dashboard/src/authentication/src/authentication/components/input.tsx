/** @file Styled input element. */
import * as React from 'react'

// =============
// === Input ===
// =============

/** Props for an {@link Input}. */
export interface InputProps extends React.InputHTMLAttributes<HTMLInputElement> {
    value: string
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
            className="bg-label placeholder-gray-500 pl-10 pr-4 rounded-2xl w-full py-2"
        />
    )
}

export default Input
