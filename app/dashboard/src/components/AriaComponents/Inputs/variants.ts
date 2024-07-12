/**
 * @file
 *
 * Variants for the ResizableInput component.
 */

import * as twv from '#/utilities/tailwindVariants'

import * as text from '../Text'

export const INPUT_STYLES = twv.tv({
  base: 'block w-full overflow-hidden bg-transparent transition-[border-color,outline] duration-200',
  variants: {
    disabled: {
      true: { base: 'cursor-default opacity-50', textArea: 'cursor-default' },
      false: { base: 'cursor-text', textArea: 'cursor-text' },
    },
    invalid: {
      // Specified in compoundVariants. Real classes depend on Variants
      true: '',
    },
    readOnly: {
      true: 'cursor-default',
      false: 'cursor-text',
    },
    size: {
      medium: { base: 'px-[11px] pb-1.5 pt-2' },
      small: { base: 'px-[11px] pb-0.5 pt-1' },
    },
    rounded: {
      none: 'rounded-none',
      small: 'rounded-sm',
      medium: 'rounded-md',
      large: 'rounded-lg',
      xlarge: 'rounded-xl',
      xxlarge: 'rounded-2xl',
      xxxlarge: 'rounded-3xl',
      full: 'rounded-full',
    },
    variant: {
      outline: {
        base: 'border-[0.5px] outline-offset-2 border-primary/20 focus-within:outline focus-within:outline-2 focus-within:outline-offset-[-1px] focus-within:outline-primary focus-within:border-primary/50',
        textArea: 'border-transparent focus-within:border-transparent',
      },
    },
  },
  slots: {
    addonStart: '',
    addonEnd: '',
    inputContainer: text.TEXT_STYLE({
      className: 'flex w-full items-center max-h-32 min-h-6 relative overflow-auto',
      variant: 'body',
    }),
    description: 'block select-none pointer-events-none opacity-80',
    textArea: 'block h-auto w-full max-h-full resize-none bg-transparent',
    resizableSpan: text.TEXT_STYLE({
      className:
        'pointer-events-none invisible absolute block max-h-32 min-h-10 overflow-y-auto break-all',
      variant: 'body',
    }),
  },
  compoundVariants: [
    {
      invalid: true,
      variant: 'outline',
      class: { base: 'border-danger focus-within:border-danger focus-within:outline-danger' },
    },
    {
      readOnly: true,
      class: { base: 'focus-within:outline-transparent' },
    },
  ],
  defaultVariants: {
    size: 'medium',
    rounded: 'xlarge',
    variant: 'outline',
  },
})
