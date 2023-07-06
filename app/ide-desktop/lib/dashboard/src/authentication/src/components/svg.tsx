/** @file File containing SVG icon definitions. */
import * as React from 'react'

/** TODO [NP]: https://github.com/enso-org/cloud-v2/issues/342
 * These should all be regular `.svg` files rather than React components, but React doesn't include
 * the `svg` files when building for Electron. Once the build scripts have been adapted to allow for
 * for this, the contents of this file should be moved back to standalone SVG files. */

// ===================================
// === SVGs with custom formatting ===
// ===================================

/** Props for a {@link Spinner}. */
export interface SpinnerProps {
    size: number
    className: string
}

/** A spinning arc that animates using the `dasharray-<percentage>` custom Tailwind classes. */
export function Spinner(props: SpinnerProps) {
    const { size, className } = props
    return (
        <svg
            width={size}
            height={size}
            viewBox="0 0 24 24"
            fill="none"
            xmlns="http://www.w3.org/2000/svg"
        >
            <rect
                x={1.5}
                y={1.5}
                width={21}
                height={21}
                rx={10.5}
                stroke="currentColor"
                strokeLinecap="round"
                strokeWidth={3}
                className={
                    'animate-spin-ease origin-center transition-stroke-dasharray ' + className
                }
            />
        </svg>
    )
}

/** Props for a {@link StopIcon}. */
export interface StopIconProps {
    className?: string
}

/** Icon displayed when a project is ready to stop. */
export function StopIcon(props: StopIconProps) {
    const { className } = props
    return (
        <svg
            width={24}
            height={24}
            viewBox="0 0 24 24"
            fill="none"
            xmlns="http://www.w3.org/2000/svg"
        >
            <path
                d="m9 8L15 8a1 1 0 0 1 1 1L16 15a1 1 0 0 1 -1 1L9 16a1 1 0 0 1 -1 -1L8 9a1 1 0 0 1 1 -1"
                fill="currentColor"
            />
            <rect
                x={1.5}
                y={1.5}
                width={21}
                height={21}
                rx={10.5}
                stroke="currentColor"
                strokeOpacity={0.1}
                strokeWidth={3}
            />
            <rect
                x={1.5}
                y={1.5}
                width={21}
                height={21}
                rx={10.5}
                stroke="currentColor"
                strokeLinecap="round"
                strokeWidth={3}
                className={`animate-spin-ease origin-center transition-stroke-dasharray ${
                    className ?? ''
                }`}
            />
        </svg>
    )
}

// ===============
// === SvgMask ===
// ===============

/** Props for a {@link SvgMask}. */
export interface SvgMaskProps {
    /** The URL of the SVG to use as the mask. */
    src: string
}

/** Use an SVG as a mask. This lets the SVG use the text color (`currentColor`). */
export function SvgMask(props: SvgMaskProps) {
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
