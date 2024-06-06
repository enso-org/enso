/**
 * @file
 *
 * Variants for the Dialog component.
 */
import * as twv from 'tailwind-variants'

export const DIALOG_BACKGROUND = twv.tv({
  base: 'bg-clip-padding relative before:absolute before:inset before:h-full before:w-full before:bg-selected-frame before:backdrop-blur-default [:where(&>*)]:relative',
})

export const DIALOG_STYLES = twv.tv({
  extend: DIALOG_BACKGROUND,
  base: 'flex flex-col overflow-hidden text-left align-middle shadow-sm border border-primary/10',
  variants: {
    rounded: {
      none: '',
      small: 'rounded-sm before:rounded-sm',
      medium: 'rounded-md before:rounded-md',
      large: 'rounded-lg before:rounded-lg',
      xlarge: 'rounded-xl before:rounded-xl',
      xxlarge: 'rounded-2xl before:rounded-2xl',
      xxxlarge: 'rounded-3xl before:rounded-3xl',
    },
  },
  defaultVariants: {
    rounded: 'xxlarge',
  },
})
