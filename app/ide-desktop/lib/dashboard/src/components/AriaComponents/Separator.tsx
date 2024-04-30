/**
 * @file
 *
 * The separator component.
 */
import * as React from 'react'

import * as tw from 'tailwind-merge'

import * as aria from '#/components/aria'

/**
 * The props for the separator component.
 */
export interface SeparatorProps extends aria.SeparatorProps {
  readonly className?: string
  readonly variant?: SeparatorVariant
  readonly orientation?: SeparatorOrientation
}

/**
 *
 */
export type SeparatorOrientation = 'horizontal' | 'vertical'
/**
 *
 */
export type SeparatorVariant = 'primary' | 'secondary'

const VARIANT_MAP: Record<SeparatorVariant, string> = {
  primary: 'border-primary',
  secondary: 'border-gray-500',
}

/**
 * A separator component.
 */
export function Separator(props: SeparatorProps) {
  const { orientation = 'horizontal', variant = 'primary', className, ...rest } = props

  return (
    <aria.Separator
      {...rest}
      orientation={orientation}
      className={tw.twMerge('isolate rounded-full', VARIANT_MAP[variant], className)}
    />
  )
}
