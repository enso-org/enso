/** @file Constants for `Alert`. */
import { tv } from '#/utilities/tailwindVariants'

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
