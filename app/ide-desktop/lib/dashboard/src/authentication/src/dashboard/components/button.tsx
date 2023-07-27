/** @file A styled button. */
import * as React from 'react'

/** Props for a {@link Button}. */
export interface ButtonProps {
    active?: boolean
    disabled?: boolean
    image: string
    onClick: (event: React.MouseEvent) => void
}

/** A styled button. */
export default function Button(props: ButtonProps) {
    const { active = false, disabled = false, image, onClick } = props

    return (
        <button
            disabled={disabled}
            className={`cursor-pointer disabled:cursor-default disabled:opacity-50 hover:opacity-100 ${
                active ? '' : 'opacity-50'
            }`}
            onClick={onClick}
        >
            <img src={image} />
        </button>
    )
}
