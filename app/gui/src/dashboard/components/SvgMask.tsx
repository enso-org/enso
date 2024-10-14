/** @file File containing SVG icon definitions. */
import * as React from 'react'

import * as tailwindMerge from '#/utilities/tailwindMerge'

// ===============
// === SvgMask ===
// ===============

/** Props for a {@link SvgMask}. */
export interface SvgMaskProps {
  readonly invert?: boolean
  readonly alt?: string
  /** The URL of the SVG to use as the mask. */
  readonly src: string
  readonly title?: string
  readonly style?: React.CSSProperties
  readonly color?: string | undefined
  readonly className?: string | undefined
}

/** Use an SVG as a mask. This lets the SVG use the text color (`currentColor`). */
export default function SvgMask(props: SvgMaskProps) {
  const { invert = false, alt = '', src, style, color, className } = props
  const urlSrc = `url(${JSON.stringify(src)})`
  const mask = invert ? `${urlSrc}, linear-gradient(white 0 0)` : urlSrc

  return (
    <div
      style={{
        ...(style ?? {}),
        backgroundColor: color ?? 'currentcolor',
        maskImage: mask,
        maskPosition: 'center',
        maskRepeat: 'no-repeat',
        maskSize: 'contain',
        ...(invert ? { maskComposite: 'exclude, exclude' } : {}),
        // The names come from a third-party API and cannot be changed.
        /* eslint-disable @typescript-eslint/naming-convention */
        WebkitMaskImage: mask,
        WebkitMaskPosition: 'center',
        WebkitMaskRepeat: 'no-repeat',
        WebkitMaskSize: 'contain',
        ...(invert ? { WebkitMaskComposite: 'exclude, exclude' } : {}),
        /* eslint-enable @typescript-eslint/naming-convention */
      }}
      className={tailwindMerge.twMerge('inline-block h-max w-max', className)}
    >
      {/* This is required for this component to have the right size. */}
      <img alt={alt} src={src} className="pointer-events-none opacity-0" draggable={false} />
    </div>
  )
}
