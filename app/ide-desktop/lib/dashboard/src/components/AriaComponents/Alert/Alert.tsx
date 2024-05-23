/**
 * @file Alert component.
 */
import * as React from 'react'

import * as twv from 'tailwind-variants'

import * as mergeRefs from '#/utilities/mergeRefs'

/**
 * Props for the Alert component.
 */
export interface AlertProps extends React.PropsWithChildren, twv.VariantProps<typeof ALERT_STYLES> {
  readonly className?: string
}

export const ALERT_STYLES = twv.tv({
  base: 'w-full rounded-md border',
  variants: {
    variant: {
      custom: '',
      error: 'bg-red-200 border-red-800 text-red-800',
      info: 'bg-blue-200 border-blue-800 text-blue-800',
      success: 'bg-green-200 border-green-800 text-green-800',
      warning: 'bg-yellow-200 border-yellow-800 text-yellow-800',
    },
    size: {
      custom: '',
      small: 'p-1.5 text-xs',
      medium: 'p-2.5 text-sm',
      large: 'p-4.5 text-lg',
    },
  },
  defaultVariants: {
    variant: 'error',
    size: 'medium',
  },
})

/**
 * Alert component.
 */
export const Alert = React.forwardRef(function Alert(
  props: AlertProps,
  ref: React.ForwardedRef<HTMLDivElement>
) {
  const { children, className, variant, size } = props

  return (
    <div
      role="alert"
      className={ALERT_STYLES({ variant, size, className })}
      tabIndex={-1}
      ref={mergeRefs.mergeRefs(ref, e => e?.focus())}
    >
      {children}
    </div>
  )
})
