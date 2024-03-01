/** @file Styled wrapper around SVG images. */
import * as React from 'react'

import SvgMask from '#/components/SvgMask'

// ===============
// === SvgIcon ===
// ===============

/** Props for a {@link SvgIcon}. */
export interface SvgIconProps {
  readonly src: string
  readonly className?: string
  readonly positionClassName?: string
  readonly onClick?: React.MouseEventHandler<HTMLDivElement>
}

/** A fixed-size container for a SVG image. */
export default function SvgIcon(props: SvgIconProps) {
  const { src, className = '', positionClassName = 'top left', onClick } = props
  return (
    <div
      className={`inline-flex items-center justify-center absolute h-full w-auth-icon-container text-gray-400 ${className} ${positionClassName}`}
      onClick={onClick}
    >
      <SvgMask src={src} />
    </div>
  )
}
