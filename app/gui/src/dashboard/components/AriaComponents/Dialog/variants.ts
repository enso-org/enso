/** @file Variants for the Dialog component. */
import { tv } from '#/utilities/tailwindVariants'

export const DIALOG_BACKGROUND = tv({
  base: 'backdrop-blur-md',
  variants: { variant: { light: 'bg-background/75', dark: 'bg-primary/70 text-invert' } },
  defaultVariants: { variant: 'light' },
})

export const DIALOG_STYLES = tv({
  extend: DIALOG_BACKGROUND,
  base: 'flex flex-col text-left align-middle shadow-xl',
})

export const POPOVER_STYLES = tv({
  base: 'shadow-xl w-full overflow-clip',
  variants: {
    variant: {
      custom: { dialog: '' },
      light: { base: DIALOG_BACKGROUND({ variant: 'light' }) },
      dark: { base: DIALOG_BACKGROUND({ variant: 'dark' }) },
    },
    isEntering: {
      true: 'animate-in fade-in placement-bottom:slide-in-from-top-1 placement-top:slide-in-from-bottom-1 placement-left:slide-in-from-right-1 placement-right:slide-in-from-left-1 ease-out duration-200',
    },
    isExiting: {
      true: 'animate-out fade-out placement-bottom:slide-out-to-top-1 placement-top:slide-out-to-bottom-1 placement-left:slide-out-to-right-1 placement-right:slide-out-to-left-1 ease-in duration-150',
    },
    size: {
      custom: { base: '', dialog: '' },
      auto: { base: 'w-[unset]', dialog: 'p-2.5' },
      xxsmall: { base: 'max-w-[206px]', dialog: 'p-1.5' },
      xsmall: { base: 'max-w-xs', dialog: 'p-3' },
      small: { base: 'max-w-sm', dialog: 'px-4 p-3' },
      medium: { base: 'max-w-md', dialog: 'px-5 p-3.5' },
      large: { base: 'max-w-lg', dialog: 'p-4' },
      xlarge: { base: 'max-w-xl', dialog: 'p-6' },
      xxlarge: { base: 'max-w-2xl', dialog: 'px-8 py-7' },
      xxxlarge: { base: 'max-w-3xl', dialog: 'px-10 py-9' },
    },
    rounded: {
      none: { base: 'rounded-none', dialog: 'rounded-none' },
      small: { base: 'rounded-sm', dialog: 'rounded-sm scroll-offset-edge-md' },
      medium: { base: 'rounded-md', dialog: 'rounded-md scroll-offset-edge-xl' },
      large: { base: 'rounded-lg', dialog: 'rounded-lg scroll-offset-edge-xl' },
      xlarge: { base: 'rounded-xl', dialog: 'rounded-xl scroll-offset-edge-xl' },
      xxlarge: { base: 'rounded-2xl', dialog: 'rounded-2xl scroll-offset-edge-2xl' },
      xxxlarge: { base: 'rounded-3xl', dialog: 'rounded-3xl scroll-offset-edge-3xl' },
      xxxxlarge: { base: 'rounded-4xl', dialog: 'rounded-4xl scroll-offset-edge-4xl' },
    },
  },
  slots: {
    dialog: 'flex-auto overflow-y-auto max-h-[inherit]',
  },
  defaultVariants: { rounded: 'xxlarge', size: 'small', variant: 'light' },
})
