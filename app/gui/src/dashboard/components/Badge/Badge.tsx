/** @file Highlight an item's status for quick recognition. */
import type { VariantProps } from '#/utilities/tailwindVariants'
import type { ReactNode } from 'react'
import { BADGE_STYLES } from './variants'

/** Props for the {@link Badge} component. */
export interface BadgeProps extends VariantProps<typeof BADGE_STYLES> {
  readonly children?: ReactNode
  readonly className?: string
}

/** Badges are used to highlight an item's status for quick recognition. */
export function Badge(props: BadgeProps) {
  const { children, color, rounded, className, variant } = props

  const classes = BADGE_STYLES({ color, rounded, variant })

  return (
    <div className={classes.base({ class: className })}>
      <div className={classes.content()}>{children}</div>
    </div>
  )
}
