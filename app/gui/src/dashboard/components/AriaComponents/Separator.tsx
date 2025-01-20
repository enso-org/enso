/**
 * @file
 *
 * The separator component.
 */
import * as React from 'react'

import * as aria from '#/components/aria'

import * as twv from '#/utilities/tailwindVariants'

/** The props for {@link Separator} component. */
export interface SeparatorProps
  extends aria.SeparatorProps,
    twv.VariantProps<typeof SEPARATOR_STYLES> {
  readonly className?: string
}

/** The styles for the {@link Separator} component. */
export const SEPARATOR_STYLES = twv.tv({
  base: 'rounded-full border-none',
  variants: {
    size: {
      thin: '',
      medium: '',
      thick: '',
    },
    orientation: {
      horizontal: 'w-full',
      vertical: 'h-full',
    },
    variant: {
      current: 'bg-current',
      primary: 'bg-primary/30',
      inverted: 'bg-white/30',
    },
  },
  defaultVariants: {
    // `size: 'thin'` causes the separator to disappear on Firefox.
    size: 'medium',
    orientation: 'horizontal',
    variant: 'primary',
  },
  compoundVariants: [
    {
      size: 'thin',
      orientation: 'horizontal',
      class: 'h-[0.5px]',
    },
    {
      size: 'thin',
      orientation: 'vertical',
      class: 'w-[0.5px]',
    },
    {
      size: 'medium',
      orientation: 'horizontal',
      class: 'h-[1px]',
    },
    {
      size: 'medium',
      orientation: 'vertical',
      class: 'w-[1px]',
    },
    {
      size: 'thick',
      orientation: 'horizontal',
      class: 'h-1',
    },
    {
      size: 'thick',
      orientation: 'vertical',
      class: 'w-1',
    },
  ],
})

/** A separator component. */
export function Separator(props: SeparatorProps) {
  const { orientation = 'horizontal', variant, className, size, ...rest } = props

  return (
    <aria.Separator
      orientation={orientation}
      className={SEPARATOR_STYLES({ orientation, variant, size, className })}
      {...rest}
    />
  )
}
