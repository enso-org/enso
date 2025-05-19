/** @file A status badge to notify the user of the state of an item. */
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import type { PropsWithChildren } from 'react'

const STATUS_BADGE_STYLES = tv({
  base: 'relative flex',
  variants: {
    color: {
      custom: { badge: '' },
      primary: { badge: 'after:bg-primary' },
      danger: { badge: 'after:bg-danger' },
      success: { badge: 'after:bg-accent-dark' },
      accent: { badge: 'after:bg-accent-dark' },
      muted: { badge: 'after:bg-primary/40' },
      disabled: { badge: 'after:bg-disabled' },
      invert: { badge: 'after:bg-invert' },
      inherit: { badge: 'after:bg-inherit' },
      current: { badge: 'after:bg-current' },
    },
    hidden: { true: { badge: 'invisible' } },
  },
  slots: {
    badge:
      'absolute bg-dashboard/80 top-0 right-0 rounded-full size-1.5 after:absolute after:rounded-full after:size-1 after:top-px after:right-px',
  },
})

/** Props for a {@link StatusBadge}. */
export interface StatusBadgeProps
  extends Readonly<PropsWithChildren>,
    VariantProps<typeof STATUS_BADGE_STYLES> {}

/** A status badge to notify the user of the state of an item. */
export function StatusBadge(props: StatusBadgeProps) {
  const { variants = STATUS_BADGE_STYLES, color, hidden, children } = props

  const styles = variants({ color, hidden })

  return (
    <div className={styles.base()}>
      {children}
      <div className={styles.badge()} />
    </div>
  )
}
