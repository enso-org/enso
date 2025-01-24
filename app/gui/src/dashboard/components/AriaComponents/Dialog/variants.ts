/**
 * @file
 *
 * Variants for the Dialog component.
 */
import * as twv from '#/utilities/tailwindVariants'

export const DIALOG_BACKGROUND = twv.tv({
  base: 'backdrop-blur-md',
  variants: { variant: { light: 'bg-background/75', dark: 'bg-primary/70 text-invert' } },
  defaultVariants: { variant: 'light' },
})

export const DIALOG_STYLES = twv.tv({
  extend: DIALOG_BACKGROUND,
  base: 'flex flex-col text-left align-middle shadow-xl',
})
