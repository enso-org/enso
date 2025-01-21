/**
 * @file SvgIcon component
 */

import type { SVGProps } from 'react'

/**
 * Props for the {@link SvgIcon} component
 */
export interface SvgIconProps extends SVGProps<SVGSVGElement> {
  readonly icon: string
  readonly showOutline?: boolean
}

/**
 * Renders an SVG icon
 */
export function SvgIcon({ icon, ...props }: SvgIconProps) {
  return (
    <svg {...props}>
      <use href={`/icons.svg#${icon}`} />
    </svg>
  )
}
