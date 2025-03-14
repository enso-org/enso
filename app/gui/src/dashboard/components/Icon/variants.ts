/** @file Variants for `Icon`. */
import { tv } from '#/utilities/tailwindVariants'

export const ICON_STYLES = tv({
  base: 'flex-none aspect-square [&>svg]:stroke-current [&>svg]:w-full [&>svg]:h-full',
  variants: {
    color: {
      custom: '',
      primary: 'text-primary',
      danger: 'text-danger',
      success: 'text-accent-dark',
      accent: 'text-accent-dark',
      muted: 'text-primary/40',
      disabled: 'text-disabled',
      invert: 'text-invert',
      inherit: 'text-inherit',
      current: 'text-current',
    },
    size: {
      xsmall: 'h-2 w-2',
      small: 'h-3 w-3',
      medium: 'h-4 w-4',
      large: 'h-5 w-5',
      xlarge: 'h-6 w-6',
      xxlarge: 'h-7 w-7',
      xxxlarge: 'h-8 w-8',
      xxxxlarge: 'h-9 w-9',
      full: 'h-full w-full',
    },
  },
  defaultVariants: {
    color: 'current',
    size: 'medium',
  },
})
