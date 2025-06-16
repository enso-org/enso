/** @file Variants for the ProfilePicture component. */
import { tv } from '#/utilities/tailwindVariants'

/** Variants for the ProfilePicture component. */
export const PROFILE_PICTURE_STYLES = tv({
  base: 'aspect-square flex-none object-cover',
  variants: {
    size: {
      auto: 'w-auto h-auto',
      full: 'w-full h-full',
      xxsmall: 'w-3 h-3',
      xsmall: 'w-4 h-4',
      small: 'w-6 h-6',
      medium: 'w-8 h-8',
      large: 'w-10 h-10',
      xlarge: 'w-12 h-12',
      xxlarge: 'w-14 h-14',
      xxxlarge: 'w-16 h-16',
    },
    rounded: {
      full: 'rounded-full',
      none: 'rounded-none',
    },
    default: { true: 'opacity-60' },
  },
  defaultVariants: {
    size: 'medium',
    rounded: 'full',
    default: true,
  },
})
