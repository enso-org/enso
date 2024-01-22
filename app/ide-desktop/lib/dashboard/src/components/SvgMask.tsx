/** @file File containing SVG icon definitions. */
import * as React from 'react'

// ===============
// === SvgMask ===
// ===============

/** Props for a {@link SvgMask}. */
export interface SvgMaskProps {
  alt?: string
  /** The URL of the SVG to use as the mask. */
  src: string
  title?: string
  style?: React.CSSProperties
  // Allowing `undefined` is fine here as this prop is being transparently passed through to the
  // underlying `div`.
  // eslint-disable-next-line no-restricted-syntax
  className?: string | undefined
  onClick?: (event: React.MouseEvent) => void
}

/** Use an SVG as a mask. This lets the SVG use the text color (`currentColor`). */
export default function SvgMask(props: SvgMaskProps) {
  const { alt, src, title, style, className, onClick } = props
  const urlSrc = `url(${JSON.stringify(src)})`

  return (
    <div
      title={title}
      style={{
        ...(style ?? {}),
        backgroundColor: 'currentcolor',
        mask: urlSrc,
        // The names come from a third-party API and cannot be changed.
        // eslint-disable-next-line @typescript-eslint/naming-convention
        WebkitMask: urlSrc,
      }}
      className={`inline-block w-max h-max ${className ?? ''}`}
      onClick={onClick}
      onDragStart={event => {
        event.preventDefault()
      }}
    >
      {/* This is required for this component to have the right size. */}
      <img alt={alt} src={src} className="opacity-0" />
    </div>
  )
}
