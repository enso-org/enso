/**
 * @file
 *
 * Badges are used to highlight an item's status for quick recognition.
 */
import { TEXT_STYLE } from '#/components/Text'
import type { IconPropSvgUse } from '#/components/types'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import type { ReactNode } from 'react'
import { Icon } from '../Icon'

/** Props for the {@link Badge} component. */
export interface BadgeProps extends VariantProps<typeof BADGE_STYLES> {
  readonly children?: ReactNode
  readonly className?: string
  readonly icon?: IconPropSvgUse<never> | undefined
}

// eslint-disable-next-line react-refresh/only-export-components
export const BADGE_STYLES = tv({
  base: 'flex items-center justify-center border-[0.5px] min-w-6 h-fit flex-none',
  variants: {
    variant: {
      solid:
        'border-transparent bg-[var(--badge-bg-color)] opacity-[var(--badge-opacity)] text-[var(--badge-text-color)]',
      outline: 'border-[var(--badge-border-color)] bg-transparent text-primary',
    },
    color: {
      primary:
        '[--badge-border-color:var(--color-primary)] [--badge-bg-color:var(--color-primary)] [--badge-text-color:var(--color-invert)]',
      accent:
        '[--badge-border-color:var(--color-accent)] [--badge-bg-color:var(--color-accent)] [--badge-text-color:var(--color-invert)]',
      error:
        '[--badge-border-color:var(--color-danger)] [--badge-bg-color:var(--color-danger)] [--badge-text-color:var(--color-invert)]',
      danger:
        '[--badge-border-color:var(--color-danger)] [--badge-bg-color:var(--color-danger)] [--badge-text-color:var(--color-invert)]',
      success:
        '[--badge-border-color:var(--color-accent-dark)] [--badge-bg-color:var(--color-accent-dark)] [--badge-text-color:var(--color-invert)]',
      muted:
        '[--badge-border-color:var(--color-primary)] [--badge-bg-color:var(--color-primary)] [--badge-text-color:var(--color-invert)] [--badge-opacity:0.5]',
      disabled:
        '[--badge-border-color:var(--color-disabled)] [--badge-bg-color:var(--color-disabled)] [--badge-text-color:var(--color-invert)]',
      invert:
        '[--badge-border-color:var(--color-invert)] [--badge-bg-color:var(--color-invert)] [--badge-text-color:var(--color-invert)]',
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
    size: {
      small: 'px-[5px]',
      medium: 'px-2',
      large: 'px-[15px]',
      xlarge: 'px-[20px]',
      xxlarge: 'px-[25px]',
      xxxlarge: 'px-[30px]',
      xxxxlarge: 'px-[35px]',
    },
  },
  slots: {
    icon: 'flex items-center justify-center',
    content: TEXT_STYLE({ variant: 'body-sm', color: 'current', truncate: true }),
  },
  defaultVariants: {
    variant: 'solid',
    color: 'primary',
    rounded: 'xxxxlarge',
    iconPosition: 'start',
    size: 'medium',
  },
})

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
