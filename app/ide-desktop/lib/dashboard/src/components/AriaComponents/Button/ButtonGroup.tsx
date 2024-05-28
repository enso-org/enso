/** @file A group of buttons. */
import * as React from 'react'

import * as twv from 'tailwind-variants'

// =================
// === Constants ===
// =================

const STYLES = twv.tv({
  base: 'flex w-full flex-auto',
  variants: {
    wrap: { true: 'flex-wrap' },
    direction: { column: 'flex-col justify-center', row: 'flex-row items-center' },
    gap: {
      custom: '',
      large: 'gap-3.5',
      medium: 'gap-2',
      small: 'gap-1.5',
      none: 'gap-0',
    },
    align: { start: 'justify-start', center: 'justify-center', end: 'justify-end' },
  },
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
