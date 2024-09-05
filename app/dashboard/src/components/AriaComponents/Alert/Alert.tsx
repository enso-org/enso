/** @file Alert component. */
import { type ForwardedRef, type HTMLAttributes, type PropsWithChildren } from 'react'

import { forwardRef } from '#/utilities/react'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'

// =================
// === Constants ===
// =================

export const ALERT_STYLES = tv({
  base: 'flex flex-col items-stretch',
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
  defaultVariants: {
    fullWidth: true,
    variant: 'error',
    size: 'medium',
    rounded: 'large',
  },
})

// =============
// === Alert ===
// =============

/** Props for an {@link Alert}. */
export interface AlertProps
  extends PropsWithChildren,
    VariantProps<typeof ALERT_STYLES>,
    HTMLAttributes<HTMLDivElement> {}

/** Alert component. */
// eslint-disable-next-line no-restricted-syntax
export const Alert = forwardRef(function Alert(
  props: AlertProps,
  ref: ForwardedRef<HTMLDivElement>,
) {
  const { children, className, variant, size, rounded, fullWidth, ...containerProps } = props

  if (variant === 'error') {
    containerProps.tabIndex = -1
    containerProps.role = 'alert'
  }

  return (
    <div
      className={ALERT_STYLES({ variant, size, className, rounded, fullWidth })}
      ref={ref}
      {...containerProps}
    >
      {children}
    </div>
  )
})
