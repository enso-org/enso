/** @file Styled wrapper around SVG images. */
import * as React from 'react'

import SvgMask from './svgMask'

// ===============
// === SvgIcon ===
// ===============

/** Props for a {@link SvgIcon}. */
export interface SvgIconProps {
    src: string
    className?: string
    positionClassName?: string
    onClick?: React.MouseEventHandler<HTMLDivElement>
}

/** A fixed-size container for a SVG image. */
export default function SvgIcon(props: SvgIconProps) {
    const { src, className = '', positionClassName = 'left-0 top-0', onClick } = props
    return (
        <div
            className={`inline-flex items-center justify-center absolute ${positionClassName} h-full w-10 text-gray-400 ${className}`}
            onClick={onClick}
        >
            <SvgMask src={src} />
        </div>
    )
}
