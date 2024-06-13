/** @file Styled wrapper around SVG images. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import SvgMask from '#/components/SvgMask'

// ===============
// === SvgIcon ===
// ===============

/** Props for a {@link SvgIcon}. */
export interface SvgIconProps {
  readonly src: string
  readonly className?: string
  readonly onClick?: React.MouseEventHandler<HTMLDivElement>
}

/** A fixed-size container for a SVG image. */
export default function SvgIcon(props: SvgIconProps) {
  const { src, className, onClick } = props
  return (
    <div
      className={tailwindMerge.twMerge(
        'absolute left top inline-flex h-full w-auth-icon-container items-center justify-center text-gray-400',
        className
      )}
      onClick={onClick}
    >
      <SvgMask src={src} />
    </div>
  )
}
