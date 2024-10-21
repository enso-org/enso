/**
 * @file
 *
 * Badges are used to highlight an item's status for quick recognition.
 */
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import type { ReactNode } from 'react'
import { TEXT_STYLE } from '../AriaComponents'

/** Props for the {@link Badge} component. */
export interface BadgeProps extends VariantProps<typeof BADGE_STYLES> {
  readonly children?: ReactNode
  readonly className?: string
}

export const BADGE_STYLES = tv({
  base: 'flex items-center justify-center px-[5px] border-[0.5px]',
  variants: {
    variant: {
      solid: 'border-transparent bg-[var(--badge-bg-color)] text-[var(--badge-text-color)]',
      outline: 'border-[var(--badge-border-color)] bg-transparent text-[var(--badge-text-color)]',
    },
    color: {
      primary:
        '[--badge-border-color:var(--color-primary)] [--badge-bg-color:var(--color-primary)] [--badge-text-color:var(--color-invert)]',
      accent:
        '[--badge-border-color:var(--color-accent)] [--badge-bg-color:var(--color-accent)] [--badge-text-color:var(--color-invert)]',
      error:
        '[--badge-border-color:var(--color-danger)] [--badge-bg-color:var(--color-danger)] [--badge-text-color:var(--color-invert)]',
    },
    rounded: {
      true: 'rounded-full',
      false: 'rounded-none',
      none: 'rounded-none',
      small: 'rounded-sm',
      medium: 'rounded-md',
      large: 'rounded-lg',
      xlarge: 'rounded-xl',
      xxlarge: 'rounded-2xl',
      xxxlarge: 'rounded-3xl',
      xxxxlarge: 'rounded-4xl',
      full: 'rounded-full',
    },
  },
  slots: {
    icon: 'flex items-center justify-center',
    content: TEXT_STYLE({
      variant: 'body-sm',
      color: 'current',
      className: '',
    }),
  },
  defaultVariants: {
    variant: 'solid',
    color: 'primary',
    rounded: 'xxxxlarge',
    iconPosition: 'start',
  },
})

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
