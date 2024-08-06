/**
 * @file
 *
 * Variants for the ResizableInput component.
 */

import { tv } from '#/utilities/tailwindVariants'
import { TEXT_STYLE } from '../Text'

export const INPUT_STYLES = tv({
  base: 'block w-full overflow-hidden bg-transparent transition-[border-color,outline] duration-200',
  variants: {
    hasIcon: { true: '' },
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
      medium: { base: 'px-[11px] pb-[6.5px] pt-[8.5px]', icon: 'size-4' },
      small: { base: 'px-[11px] pb-0.5 pt-1', icon: 'size-3' },
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
      settings: {
        base: 'p-0',
        textArea: 'rounded-2xl border-0.5 border-primary/20 px-1',
      },
    },
  },
  slots: {
    label: '',
    icon: 'absolute top-1/2 -translate-y-1/2 left-0',
    addonStart: 'flex items-center mt-[-1px]',
    addonEnd: 'flex items-center mt-[-1px]',
    content: 'flex items-center gap-2',
    inputContainer: TEXT_STYLE({
      className: 'flex w-full items-center max-h-32 min-h-6 relative overflow-auto',
      variant: 'body',
    }),
    selectorContainer: 'flex',
    description: 'block select-none pointer-events-none opacity-80',
    textArea: 'block h-auto w-full max-h-full resize-none bg-transparent',
    resizableSpan: TEXT_STYLE({
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
    {
      hasIcon: true,
      size: 'medium',
      class: { textArea: 'pl-6' },
    },
    {
      hasIcon: true,
      size: 'small',
      class: { textArea: 'pl-5' },
    },
  ],
  defaultVariants: {
    size: 'medium',
    rounded: 'xlarge',
    variant: 'outline',
  },
})
