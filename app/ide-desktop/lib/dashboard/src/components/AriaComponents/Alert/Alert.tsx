/**
 * @file Alert component.
 */
import * as React from 'react'

import * as tw from 'tailwind-merge'

import * as mergeRefs from '#/utilities/mergeRefs'

/**
 * Props for the Alert component.
 */
export interface AlertProps extends React.PropsWithChildren {
  readonly variant?: AlertVariant
  readonly className?: string
  readonly size?: AlertSize
}

/**
 * Variants for the Alert component.
 */
export type AlertVariant = 'custom' | 'error' | 'info' | 'success' | 'warning'

/**
 * Sizes for the Alert component.
 */
export type AlertSize = 'custom' | 'large' | 'medium' | 'small'

/**
 * Alert component.
 */
export const Alert = React.forwardRef(
  (props: AlertProps, ref: React.ForwardedRef<HTMLDivElement>) => {
    const { children, className, variant = 'error', size = 'medium' } = props

    return (
      <div
        role="alert"
        className={tw.twMerge(
          'w-full rounded-md border',
          VARIANT_CLASSES[variant],
          SIZE_CLASSES[size],
          className
        )}
        tabIndex={-1}
        ref={mergeRefs.mergeRefs(ref, e => e?.focus())}
      >
        {children}
      </div>
    )
  }
)

const VARIANT_CLASSES: Record<AlertVariant, string> = {
  custom: '',
  error: 'bg-red-200 border-red-800 text-red-800',
  info: 'bg-blue-200 border-blue-800 text-blue-800',
  success: 'bg-green-200 border-green-800 text-green-800',
  warning: 'bg-yellow-200 border-yellow-800 text-yellow-800',
}

const SIZE_CLASSES: Record<AlertSize, string> = {
  custom: '',
  small: 'p-1.5 text-xs',
  medium: 'p-2.5 text-sm',
  large: 'p-4.5 text-lg',
}
