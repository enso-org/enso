/**
 * @file Patterns are set of reusable styles for certain elements.
 */

import { tv } from '#/utilities/tailwindVariants'

export const TEXT_WITH_ICON = tv({
  base: 'flex',
  slots: {
    icon: 'flex-none',
    text: '',
  },
  variants: {
    verticalAlign: {
      top: 'items-start',
      center: 'items-center',
      bottom: 'items-end',
    },
    gap: {
      custom: '',
      none: 'gap-0',
      small: 'gap-1',
      medium: 'gap-2',
      large: 'gap-3',
      xlarge: 'gap-4',
      xxlarge: 'gap-5',
      xxxlarge: 'gap-6',
    },
  },
  defaultVariants: { gap: 'medium', verticalAlign: 'center' },
})
