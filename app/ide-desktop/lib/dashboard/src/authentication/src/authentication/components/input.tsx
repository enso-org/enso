/** @file Styled input element. */
import * as react from 'react'

// =============
// === Input ===
// =============

export interface InputProps extends react.InputHTMLAttributes<HTMLInputElement> {
    value: string
    setValue: (value: string) => void
}

function Input(props: InputProps) {
    const { setValue, ...passThrough } = props
    return (
        <input
            {...passThrough}
            onChange={event => {
                setValue(event.target.value)
            }}
            className={
                'text-sm sm:text-base placeholder-gray-500 pl-10 pr-4 rounded-lg border border-gray-400 ' +
                'w-full py-2 focus:outline-none focus:border-blue-400'
            }
        />
    )
}

export default Input
