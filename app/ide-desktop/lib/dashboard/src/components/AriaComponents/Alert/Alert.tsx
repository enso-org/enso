/** @file Alert component. */
import * as React from 'react'

import * as mergeRefs from '#/utilities/mergeRefs'
import * as twv from '#/utilities/tailwindVariants'

// =================
// === Constants ===
// =================

export const ALERT_STYLES = twv.tv({
  base: 'flex flex-col items-stretch',
  variants: {
    fullWidth: { true: 'w-full' },
    variant: {
      custom: '',
      outline: 'bg-transparent border-primary/30 text-primary',
      neutral: 'bg-gray-100 border-gray-800 text-primary',
      error: 'bg-red-100 border-danger text-primary',
      info: 'bg-blue-100 border-blue-800 text-blue-800',
      success: 'bg-green-100 border-green-800 text-green-800',
      warning: 'bg-yellow-100 border-yellow-800 text-yellow-800',
    },
    border: {
      custom: '',
      none: '',
      small: 'border',
      medium: 'border-2',
      large: 'border-[3px]',
      xlarge: 'border-4',
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
    border: 'medium',
  },
})

// =============
// === Alert ===
// =============

/** Props for an {@link Alert}. */
export interface AlertProps
  extends React.PropsWithChildren,
    twv.VariantProps<typeof ALERT_STYLES>,
    React.HTMLAttributes<HTMLDivElement> {}

/** Alert component. */
export const Alert = React.forwardRef(function Alert(
  props: AlertProps,
  ref: React.ForwardedRef<HTMLDivElement>
) {
  const { children, className, variant, size, rounded, fullWidth, border, ...containerProps } =
    props

  if (variant === 'error') {
    containerProps.tabIndex = -1
    containerProps.role = 'alert'
  }

  return (
    <div
      className={ALERT_STYLES({ variant, size, className, rounded, fullWidth, border })}
      ref={mergeRefs.mergeRefs(ref, e => {
        if (variant === 'error') {
          e?.focus()
        }
      })}
      {...containerProps}
    >
      {children}
    </div>
  )
})
