/**
 * @file
 *
 * The separator component.
 */
import * as React from 'react'

import * as aria from '#/components/aria'

import * as twv from '#/utilities/tailwindVariants'

/**
 * The props for {@link Separator} component.
 */
export interface SeparatorProps
  extends aria.SeparatorProps,
    twv.VariantProps<typeof SEPARATOR_STYLES> {
  readonly className?: string
}

/**
 * The styles for the {@link Separator} component.
 */
export const SEPARATOR_STYLES = twv.tv({
  base: 'isolate rounded-full',
  variants: {
    orientation: {
      horizontal: 'border-t',
      vertical: 'border-l',
    },
    variant: {
      primary: 'border-primary/30',
      inverted: 'border-white/30',
    },
  },
})

/**
 * A separator component.
 */
export function Separator(props: SeparatorProps) {
  const { orientation = 'horizontal', variant = 'primary', className, ...rest } = props

  return (
    <aria.Separator
      orientation={orientation}
      className={SEPARATOR_STYLES({ orientation, variant, className })}
      {...rest}
    />
  )
}
