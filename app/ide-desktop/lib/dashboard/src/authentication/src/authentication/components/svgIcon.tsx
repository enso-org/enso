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
        <div className="absolute left-0 top-0 inline-flex h-full w-10 items-center justify-center text-gray-400">
            {children}
        </div>
    )
}
