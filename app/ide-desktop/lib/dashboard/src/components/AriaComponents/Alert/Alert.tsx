/**
 * @file Alert component.
 */
import * as React from 'react'

import * as twv from 'tailwind-variants'

import * as mergeRefs from '#/utilities/mergeRefs'

import * as text from '../Text'

/**
 * Props for the Alert component.
 */
export interface AlertProps
  extends React.PropsWithChildren,
    twv.VariantProps<typeof ALERT_STYLES>,
    React.HTMLAttributes<HTMLDivElement> {}

export const ALERT_STYLES = twv.tv({
  base: 'flex flex-col items-stretch',
  variants: {
    fullWidth: { true: 'w-full' },
    variant: {
      custom: '',
      outline: 'border bg-transparent border-primary/30 text-primary',
      neutral: 'border bg-gray-200 border-gray-800 text-primary',
      error: 'border bg-red-200 border-red-800 text-danger',
      info: 'border bg-blue-200 border-blue-800 text-blue-800',
      success: 'border bg-green-200 border-green-800 text-green-800',
      warning: 'border bg-yellow-200 border-yellow-800 text-yellow-800',
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
      small: text.TEXT_STYLE({
        color: 'custom',
        variant: 'body',
        class: 'px-1.5 pt-1 pb-1',
      }),
      medium: text.TEXT_STYLE({
        color: 'custom',
        variant: 'body',
        class: 'px-3 pt-1 pb-1',
      }),
      large: text.TEXT_STYLE({
        color: 'custom',
        variant: 'subtitle',
        class: 'px-4 pt-2 pb-2',
      }),
    },
  },
  defaultVariants: {
    fullWidth: true,
    variant: 'error',
    size: 'medium',
    rounded: 'large',
  },
})

/**
 * Alert component.
 */
export const Alert = React.forwardRef(function Alert(
  props: AlertProps,
  ref: React.ForwardedRef<HTMLDivElement>
) {
  const { children, className, variant, size, rounded, fullWidth, ...containerProps } = props

  if (variant === 'error') {
    containerProps.tabIndex = -1
    containerProps.role = 'alert'
  }

  return (
    <div
      className={ALERT_STYLES({ variant, size, className, rounded, fullWidth })}
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
