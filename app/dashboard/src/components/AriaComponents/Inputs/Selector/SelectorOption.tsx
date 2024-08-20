/** @file An option in a selector. */
import { Radio, type RadioProps } from '#/components/aria'
import { forwardRef } from '#/utilities/react'
import { tv } from '#/utilities/tailwindVariants'
import * as React from 'react'
import type { VariantProps } from 'tailwind-variants'
import { TEXT_STYLE } from '../../Text'

/** Props for a {@link SelectorOption}. */
export interface SelectorOptionProps
  extends RadioProps,
    VariantProps<typeof SELECTOR_OPTION_STYLES> {
  readonly label: string
}

export const SELECTOR_OPTION_STYLES = tv({
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
    variant: {
      primary:
        'selected:bg-primary selected:text-white hover:bg-primary/5 pressed:bg-primary/10 outline outline-2 outline-transparent outline-offset-[-2px] focus-visible:outline-primary focus-visible:outline-offset-0',
    },
  },
  defaultVariants: {
    size: 'medium',
    rounded: 'xxxlarge',
    variant: 'primary',
  },
})

export const SelectorOption = forwardRef(function SelectorOption(
  props: SelectorOptionProps,
  ref: React.ForwardedRef<HTMLLabelElement>,
) {
  const { label, ...radioProps } = props
  const { className } = props

  return (
    <Radio
      ref={ref}
      {...radioProps}
      className={(renderProps) =>
        SELECTOR_OPTION_STYLES({
          className: typeof className === 'function' ? className(renderProps) : className,
        })
      }
    >
      {label}
    </Radio>
  )
})
