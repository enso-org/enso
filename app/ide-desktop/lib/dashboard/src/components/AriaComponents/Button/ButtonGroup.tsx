/** @file A group of buttons. */
import * as React from 'react'

import * as twv from '#/utilities/tailwindVariants'

// =================
// === Constants ===
// =================

const STYLES = twv.tv({
  base: 'flex w-full flex-1 shrink-0',
  variants: {
    wrap: { true: 'flex-wrap' },
    direction: { column: 'flex-col', row: 'flex-row' },
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
  },
  compoundVariants: [
    { direction: 'column', align: 'start', class: 'items-start' },
    { direction: 'column', align: 'center', class: 'items-center' },
    { direction: 'column', align: 'end', class: 'items-end' },
  ],
})

// ===================
// === ButtonGroup ===
// ===================

/** Props for a {@link ButtonGroup}. */
interface ButtonGroupProps extends React.PropsWithChildren, twv.VariantProps<typeof STYLES> {
  readonly className?: string
}

/** A group of buttons. */
export function ButtonGroup(props: ButtonGroupProps) {
  const {
    children,
    className,
    gap = 'medium',
    wrap = false,
    direction = 'row',
    align,
    ...passthrough
  } = props

  return (
    <div
      className={STYLES({
        gap,
        wrap,
        direction,
        align,
        className,
      })}
      {...passthrough}
    >
      {children}
    </div>
  )
}
