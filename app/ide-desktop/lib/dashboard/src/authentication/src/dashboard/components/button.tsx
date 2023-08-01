/** @file A styled button. */
import * as React from 'react'
import SvgMask from '../../authentication/components/svgMask'

/** Props for a {@link Button}. */
export interface ButtonProps {
    active?: boolean
    disabled?: boolean
    image: string
    /** A title that is only shown when `disabled` is true. */
    error?: string | null
    onClick: (event: React.MouseEvent) => void
}

/** A styled button. */
export default function Button(props: ButtonProps) {
    const { active = false, disabled = false, image, error, onClick } = props

    return (
        <SvgMask
            {...(error != null ? { title: error } : {})}
            src={image}
            className={`${active && !disabled ? '' : 'opacity-50'} ${
                disabled ? 'cursor-not-allowed' : 'cursor-pointer hover:opacity-100 cursor-pointer'
            }`}
            onClick={onClick}
        />
    )
}
