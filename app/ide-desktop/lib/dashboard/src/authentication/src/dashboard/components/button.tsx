/** @file A styled button. */
import * as React from 'react'

/** Props for a {@link Button}. */
export interface ButtonProps {
    active?: boolean
    disabled?: boolean
    image: string
    /** A title that is only shown when `disabled` is true. */
    error?: string | null
    className?: string
    onClick: (event: React.MouseEvent) => void
}

/** A styled button. */
export default function Button(props: ButtonProps) {
    const { active = false, disabled = false, image, error, className, onClick } = props

    return (
        <button
            disabled={disabled}
            {...(disabled && error != null ? { title: error } : {})}
            className={`cursor-pointer disabled:cursor-default disabled:opacity-50 disabled:cursor-not-allowed hover:opacity-100 ${
                active ? '' : 'opacity-50'
            } ${className ?? ''}`}
            onClick={onClick}
        >
            <img src={image} />
        </button>
    )
}
