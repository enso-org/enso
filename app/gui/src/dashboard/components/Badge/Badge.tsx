/** @file Highlight an item's status for quick recognition. */
import type { IconPropSvgUse } from '#/components/AriaComponents'
import { Icon } from '#/components/Icon'
import type { VariantProps } from '#/utilities/tailwindVariants'
import type { ReactNode } from 'react'
import { BADGE_STYLES } from './variants'

/** Props for the {@link Badge} component. */
export interface BadgeProps extends VariantProps<typeof BADGE_STYLES> {
  readonly children?: ReactNode
  readonly className?: string
  readonly icon?: IconPropSvgUse<never> | undefined
}

/** Badges are used to highlight an item's status for quick recognition. */
export function Badge(props: BadgeProps) {
  const {
    children,
    color,
    rounded,
    className,
    variant,
    size,
    variants = BADGE_STYLES,
    icon,
  } = props

  const classes = variants({ color, rounded, variant, size })

  return (
    <div className={classes.base({ class: className })}>
      {icon != null && <Icon icon={icon} className={classes.icon()} />}
      <div className={classes.content()}>{children}</div>
    </div>
  )
}
