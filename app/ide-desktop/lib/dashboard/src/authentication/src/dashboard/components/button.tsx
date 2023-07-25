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
        <img
            className={`${active ? '' : 'opacity-50'} ${
                disabled ? '' : 'hover:opacity-100 cursor-pointer'
            }`}
            src={image}
            onClick={onClick}
        />
    )
}
