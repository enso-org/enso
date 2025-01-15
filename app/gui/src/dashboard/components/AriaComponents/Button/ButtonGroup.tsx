/** @file A group of buttons. */
import * as React from 'react'

import * as twv from '#/utilities/tailwindVariants'

// =================
// === Constants ===
// =================

const STYLES = twv.tv({
  base: 'flex flex-1 shrink-0 max-h-max',
  variants: {
    wrap: { true: 'flex-wrap' },
    direction: { column: 'flex-col', row: 'flex-row' },
    width: {
      auto: 'w-auto',
      full: 'w-full',
      min: 'w-min',
      max: 'w-max',
    },
    gap: {
      custom: '',
      large: 'gap-3.5',
      medium: 'gap-2',
      small: 'gap-1.5',
      xsmall: 'gap-1',
      xxsmall: 'gap-0.5',
      none: 'gap-0',
    },
    align: {
      start: 'justify-start',
      center: 'justify-center',
      end: 'justify-end',
      between: 'justify-between',
      around: 'justify-around',
      evenly: 'justify-evenly',
    },
    verticalAlign: {
      start: 'items-start',
      center: 'items-center',
      end: 'items-end',
    },
  },
  compoundVariants: [
    { direction: 'column', align: 'start', class: 'items-start' },
    { direction: 'column', align: 'center', class: 'items-center' },
    { direction: 'column', align: 'end', class: 'items-end' },
    { direction: 'column', verticalAlign: 'start', class: 'justify-start' },
    { direction: 'column', verticalAlign: 'center', class: 'justify-center' },
    { direction: 'column', verticalAlign: 'end', class: 'justify-end' },
  ],
})

// ===================
// === ButtonGroup ===
// ===================

/** Props for a {@link ButtonGroup}. */
interface ButtonGroupProps extends React.PropsWithChildren, twv.VariantProps<typeof STYLES> {
  readonly className?: string | undefined
}

/** A group of buttons. */
export const ButtonGroup = React.forwardRef(function ButtonGroup(
  props: ButtonGroupProps,
  ref: React.ForwardedRef<HTMLDivElement>,
) {
  const {
    children,
    className,
    gap = 'medium',
    wrap = false,
    direction = 'row',
    width = 'full',
    align,
    variants = STYLES,
    verticalAlign,
    ...passthrough
  } = props

  return (
    <div
      ref={ref}
      className={variants({
        gap,
        wrap,
        direction,
        align,
        verticalAlign,
        width,
        className,
      })}
      {...passthrough}
    >
      {children}
    </div>
  )
})
