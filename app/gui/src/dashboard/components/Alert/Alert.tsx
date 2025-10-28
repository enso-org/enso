/** @file Alert component. */
import { forwardRef, type ForwardedRef, type HTMLAttributes, type PropsWithChildren } from 'react'

import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { Icon } from '../Icon'
import type { IconProp } from '../types'
// eslint-disable-next-line react-refresh/only-export-components
export const ALERT_STYLES = tv({
  base: 'flex items-stretch gap-2',
  variants: {
    fullWidth: { true: 'w-full' },
    variant: {
      custom: '',
      outline: 'border border-0.5 bg-transparent border-primary/20 text-primary',
      neutral: 'border border-0.5 bg-gray-100 border-gray-800 text-primary',
      error: 'border border-0.5 bg-red-100 border-danger text-primary',
      info: 'border border-0.5 bg-blue-100 border-blue-800 text-blue-800',
      success: 'border border-0.5 bg-green-100 border-green-800 text-green-800',
      warning: 'border border-0.5 bg-yellow-100 border-yellow-800 text-yellow-800',
    },
    rounded: {
      none: 'rounded-none',
      small: 'rounded-sm',
      medium: 'rounded-md',
      large: 'rounded-lg',
      xlarge: 'rounded-xl',
      xxlarge: 'rounded-2xl',
      xxxlarge: 'rounded-3xl',
    },
    size: {
      custom: '',
      small: 'px-1.5 pt-1 pb-1',
      medium: 'px-4 py-2',
      large: 'px-4 pt-2 pb-2',
    },
  },
  slots: {
    iconContainer: 'my-auto pt-1',
    children: 'flex flex-col items-stretch',
  },
  defaultVariants: {
    fullWidth: true,
    variant: 'error',
    size: 'medium',
    rounded: 'xlarge',
  },
})

/** Props for an {@link Alert}. */
export interface AlertProps<IconType extends string = string>
  extends PropsWithChildren,
    VariantProps<typeof ALERT_STYLES>,
    HTMLAttributes<HTMLDivElement> {
  /** The icon to display in the Alert */
  readonly icon?: IconProp<IconType> | null | undefined
}

/** Alert component. */
export const Alert = forwardRef(function Alert<IconType extends string = string>(
  props: AlertProps<IconType>,
  ref: ForwardedRef<HTMLDivElement>,
) {
  const {
    children,
    className,
    variant,
    size,
    rounded,
    fullWidth,
    icon,
    variants = ALERT_STYLES,
    tabIndex: rawTabIndex,
    role: rawRole,
    ...containerProps
  } = props

  const tabIndex = variant === 'error' ? -1 : rawTabIndex
  const role = variant === 'error' ? 'alert' : rawRole

  const classes = variants({
    variant,
    size,
    rounded,
    fullWidth,
  })

  return (
    <div
      className={classes.base({ className })}
      ref={ref}
      tabIndex={tabIndex}
      role={role}
      {...containerProps}
    >
      {icon != null && <Icon icon={icon} size="medium" className={classes.iconContainer()} />}

      <div className={classes.children()}>{children}</div>
    </div>
  )
})
