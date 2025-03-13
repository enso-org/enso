/** @file Variants for `MultiSelector`. */
import { TEXT_STYLE } from '#/components/AriaComponents/Text'
import { tv } from '#/utilities/tailwindVariants'

export const MULTI_SELECTOR_STYLES = tv({
  base: 'block w-full bg-transparent transition-[border-color,outline] duration-200',
  variants: {
    disabled: {
      true: { base: 'cursor-default opacity-50', textArea: 'cursor-default' },
      false: { base: 'cursor-text', textArea: 'cursor-text' },
    },
    readOnly: { true: 'cursor-default' },
    size: {
      medium: '',
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
      outline: 'border-[0.5px] border-primary/20',
      // eslint-disable-next-line @typescript-eslint/naming-convention
      'separate-outline': { listBox: 'gap-2' },
    },
  },
  defaultVariants: {
    size: 'medium',
    rounded: 'xxlarge',
    variant: 'outline',
  },
  slots: {
    listBox: 'grid',
  },
})

export const MULTI_SELECTOR_OPTION_STYLES = tv({
  base: TEXT_STYLE({
    className:
      'flex flex-1 items-center justify-center min-h-8 relative overflow-clip cursor-pointer transition-[background-color,color,outline-offset] duration-200',
    variant: 'body',
  }),
  variants: {
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
    size: {
      medium: { base: 'px-[11px] pb-1.5 pt-2' },
      small: { base: 'px-[11px] pb-0.5 pt-1' },
    },
    color: {
      primary:
        'selected:bg-primary selected:text-white hover:bg-primary/5 pressed:bg-primary/10 outline outline-2 outline-transparent outline-offset-[-2px] focus-visible:outline-primary focus-visible:outline-offset-0',
    },
    variant: {
      default: '',
      outline: 'border-[0.5px] border-primary/20',
    },
  },
  defaultVariants: {
    size: 'medium',
    rounded: 'xxxlarge',
    color: 'primary',
    variant: 'default',
  },
})
