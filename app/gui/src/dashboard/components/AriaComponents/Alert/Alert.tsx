/** @file Alert component. */
import { type ForwardedRef, type HTMLAttributes, type PropsWithChildren } from 'react'

import SvgMask from '#/components/SvgMask'

import { forwardRef } from '#/utilities/react'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'

// =================
// === Constants ===
// =================

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
      medium: 'px-3 pt-1 pb-1',
      large: 'px-4 pt-2 pb-2',
    },
  },
  slots: {
    iconContainer: 'flex items-center justify-center w-6 h-6',
    children: 'flex flex-col items-stretch',
    icon: 'flex items-center justify-center w-6 h-6 mr-2',
  },
  defaultVariants: {
    fullWidth: true,
    variant: 'error',
    size: 'medium',
    rounded: 'xlarge',
  },
})

// =============
// === Alert ===
// =============

/** Props for an {@link Alert}. */
export interface AlertProps
  extends PropsWithChildren,
    VariantProps<typeof ALERT_STYLES>,
    HTMLAttributes<HTMLDivElement> {
  /** The icon to display in the Alert */
  readonly icon?: React.ReactElement | string | null | undefined
}

/** Alert component. */
export const Alert = forwardRef(function Alert(
  props: AlertProps,
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
      {icon != null &&
        (() => {
          if (typeof icon === 'string') {
            return (
              <div className={classes.iconContainer()}>
                <SvgMask src={icon} />
              </div>
            )
          }
          return <div className={classes.iconContainer()}>{icon}</div>
        })()}

      <div className={classes.children()}>{children}</div>
    </div>
  )
})
