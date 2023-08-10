/** @file File containing SVG icon definitions. */
import * as React from 'react'

// ===============
// === SvgMask ===
// ===============

/** Props for a {@link SvgMask}. */
export interface SvgMaskProps {
    /** The URL of the SVG to use as the mask. */
    src: string
}

/** Use an SVG as a mask. This lets the SVG use the text color (`currentColor`). */
export default function SvgMask(props: SvgMaskProps) {
    const { src } = props
    const urlSrc = `url(${JSON.stringify(src)})`

    return (
        <div
            style={{
                backgroundColor: 'currentcolor',
                mask: urlSrc,
                // The names come from a third-party API and cannot be changed.
                // eslint-disable-next-line @typescript-eslint/naming-convention
                WebkitMask: urlSrc,
            }}
        >
            {/* This is required for this component to have the right size. */}
            <img src={src} className="opacity-0" />
        </div>
    )
}
