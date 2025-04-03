/** @file Variants for `CopyBlock`. */
import { TEXT_STYLE } from '#/components/AriaComponents/Text'
import { tv } from '#/utilities/tailwindVariants'

export const COPY_BLOCK_STYLES = tv({
  base: TEXT_STYLE({
    class: 'max-w-full bg-primary/5 border-primary/10',
  }),
  variants: {
    size: {
      small: 'py-[1.5px] px-[5.5px]',
      medium: 'py-[3.5px] px-[7.5px]',
      large: 'py-[5.5px] px-[11.5px]',
    },
    rounded: {
      custom: '',
      small: 'rounded-sm',
      medium: 'rounded-md',
      large: 'rounded-lg',
      full: 'rounded-full',
    },
  },
  slots: { copyTextBlock: 'flex-auto text-nowrap overflow-x-auto scroll-hidden w-full' },
  defaultVariants: { size: 'medium', rounded: 'full' },
})
