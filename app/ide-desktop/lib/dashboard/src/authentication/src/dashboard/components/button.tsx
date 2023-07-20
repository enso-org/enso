/** @file A styled button. */
import * as React from 'react'

/** Props for a {@link Button}. */
export interface ButtonProps {
    active: boolean
    disabled?: boolean
    image: string
    onClick: (event: React.MouseEvent) => void
}

/** A styled button. */
export default function Button(props: ButtonProps) {
    const { active, disabled = false, image, onClick } = props

    return (
        <img
            className={`hover:opacity-100 ${active ? '' : 'opacity-30'} ${disabled ? '' : 'cursor-pointer'}`}
            src={image}
            onClick={onClick}
        />
    )
}
