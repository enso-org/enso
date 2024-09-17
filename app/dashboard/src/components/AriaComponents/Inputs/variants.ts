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
      custom: {},
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
      custom: {},
      outline: {
        base: 'border-[0.5px] border-primary/20 outline-offset-2 focus-within:border-primary/50 focus-within:outline focus-within:outline-2 focus-within:outline-offset-0 focus-within:outline-primary',
        textArea: 'border-transparent focus-within:border-transparent',
      },
    },
  },
  slots: {
    icon: 'flex-none',
    addonStart: 'mt-[-1px] flex flex-none items-center gap-1',
    addonEnd: 'mt-[-1px] flex flex-none items-center gap-1',
    content: 'flex items-center gap-2',
    inputContainer: TEXT_STYLE({
      className: 'relative flex max-h-32 min-h-6 w-full items-center overflow-clip',
      variant: 'body',
    }),
    selectorContainer: 'flex',
    description: 'pointer-events-none block select-none opacity-80',
    textArea: 'block h-auto max-h-full w-full resize-none bg-transparent',
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
  ],
  defaultVariants: {
    size: 'medium',
    rounded: 'xlarge',
    variant: 'outline',
  },
})
