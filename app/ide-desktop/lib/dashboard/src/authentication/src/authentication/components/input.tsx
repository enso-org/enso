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
export default function Input(props: InputProps) {
    const { setValue, ...passThrough } = props
    return (
        <input
            {...passThrough}
            onChange={event => {
                setValue(event.target.value)
            }}
            className="text-sm sm:text-base placeholder-gray-500 pl-10 pr-4 rounded-lg border border-gray-400 w-full py-2 focus:outline-none focus:border-blue-400"
        />
    )
}
