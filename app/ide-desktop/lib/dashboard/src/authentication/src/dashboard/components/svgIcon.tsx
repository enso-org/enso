/** @file Styled wrapper around SVG images. */
import * as React from 'react'

// ===============
// === SvgIcon ===
// ===============

/** Props for a {@link SvgIcon}. */
export interface SvgIconProps extends React.PropsWithChildren {}

/** A fixed-size container for a SVG image. */
export default function SvgIcon(props: SvgIconProps) {
    const { children } = props

    return (
        <div className="inline-flex items-center justify-center absolute left-0 top-0 h-full w-10 text-gray-400">
            <span>{children}</span>
        </div>
    )
}
