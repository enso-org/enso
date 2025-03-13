/** @file Constants for `Check`. */
import { tv } from '#/utilities/tailwindVariants'
import type { Variants } from 'framer-motion'

/** Variants for the {@link Check} component. */
/* eslint-disable @typescript-eslint/no-magic-numbers */
export const CHECK_VARIANTS: Variants = {
  checked: {
    pathLength: 1,
    opacity: 1,
    transition: { type: 'tween', duration: 0.2, easings: 'circIn' },
  },
  // eslint-disable-next-line @typescript-eslint/naming-convention
  'pressed-checked': {
    pathLength: 0.8,
    opacity: 1,
    transition: { type: 'tween', duration: 0.2, easings: 'circIn' },
  },
  // eslint-disable-next-line @typescript-eslint/naming-convention
  'pressed-unchecked': {
    pathLength: 0.2,
    opacity: 0.5,
    transition: { type: 'tween', duration: 0.2, easings: 'circIn' },
  },
  unchecked: {
    pathLength: 0,
    opacity: 0,
    transition: { type: 'tween', duration: 0.2, easings: 'circOut' },
  },
}
/* eslint-enable @typescript-eslint/no-magic-numbers */

export const CHECK_STYLES = tv({
  base: ['flex-none aspect-square', 'transition-[outline-offset,border-width] duration-200'],
  variants: {
    isSelected: {
      true: { base: 'border-transparent' },
    },
    isPressed: {
      true: { base: '' },
    },
    // Defined in compoundVariants
    color: {
      custom: { base: '' },
      primary: { base: 'border-primary' },
      accent: { base: 'border-accent' },
      error: { base: 'border-danger' },
    },
    variant: {
      custom: { base: '' },
      outline: { base: 'border-[0.5px]' },
    },
    rounded: {
      custom: { base: '' },
      none: { base: 'rounded-none' },
      full: { base: 'rounded-full' },
      large: { base: 'rounded-lg' },
      medium: { base: 'rounded-md' },
      small: { base: 'rounded-sm' },
      xlarge: { base: 'rounded-xl' },
      xxlarge: { base: 'rounded-2xl' },
      xxxlarge: { base: 'rounded-3xl' },
    },
    size: {
      small: { base: 'w-3 h-3' },
      medium: { base: 'w-4 h-4' },
      large: { base: 'w-5 h-5' },
    },
  },
  slots: { path: '' },
  defaultVariants: {
    size: 'medium',
    rounded: 'medium',
    color: 'primary',
    isPressed: false,
    isSelected: false,
    isIndeterminate: false,
    variant: 'outline',
  },
  compoundVariants: [
    {
      isSelected: true,
      color: 'primary',
      class: { base: 'bg-primary text-white' },
    },
    {
      isSelected: true,
      color: 'accent',
      class: { base: 'bg-accent text-white' },
    },
    {
      isSelected: true,
      color: 'error',
      class: { base: 'bg-danger text-white' },
    },
  ],
})
