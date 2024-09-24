/** @file An option in a selector. */
import { AnimatedBackground } from '#/components/AnimatedBackground'
import { Radio, type RadioProps } from '#/components/aria'
import { forwardRef } from '#/utilities/react'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import * as React from 'react'
import { TEXT_STYLE } from '../../Text'

/** Props for a {@link SelectorOption}. */
export interface SelectorOptionProps
  extends RadioProps,
    VariantProps<typeof SELECTOR_OPTION_STYLES> {
  readonly label: string
}

export const SELECTOR_OPTION_STYLES = tv({
  base: 'flex flex-1 w-full min-h-8 cursor-pointer',
  variants: {
    rounded: {
      // specified in compoundSlots
      none: '',
      small: '',
      medium: '',
      large: '',
      xlarge: '',
      xxlarge: '',
      xxxlarge: '',
      full: '',
    },
    size: {
      medium: { radio: 'px-[9px] py-[3.5px]' },
      small: { radio: 'px-[7px] py-[1.5px]' },
    },
    variant: {
      primary: {
        radio:
          'overflow-clip outline outline-2 outline-transparent outline-offset-[-2px] [&:not(:selected)]:bg-primary/5 selected:text-white pressed:bg-primary/10 focus-visible:outline-primary focus-visible:outline-offset-0',
      },
    },
  },
  slots: {
    animation: 'bg-primary',
    radio: TEXT_STYLE({
      className: 'flex flex-1 w-full items-center justify-center transition-colors duration-200',
      variant: 'body',
    }),
  },
  compoundSlots: [
    {
      slots: ['radio', 'animation', 'base'],
      rounded: 'none',
      class: 'rounded-none',
    },
    {
      slots: ['radio', 'animation', 'base'],
      rounded: 'small',
      class: 'rounded-sm',
    },
    {
      slots: ['radio', 'animation', 'base'],
      rounded: 'medium',
      class: 'rounded-md',
    },
    {
      slots: ['radio', 'animation', 'base'],
      rounded: 'large',
      class: 'rounded-lg',
    },
    {
      slots: ['radio', 'animation', 'base'],
      rounded: 'xlarge',
      class: 'rounded-xl',
    },
    {
      slots: ['radio', 'animation', 'base'],
      rounded: 'xxlarge',
      class: 'rounded-2xl',
    },
    {
      slots: ['radio', 'animation', 'base'],
      rounded: 'xxxlarge',
      class: 'rounded-3xl',
    },
    {
      slots: ['radio', 'animation', 'base'],
      rounded: 'full',
      class: 'rounded-full',
    },
  ],
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
  const {
    label,
    value,
    size,
    rounded,
    variant,
    className,
    variants = SELECTOR_OPTION_STYLES,
    ...radioProps
  } = props

  const styles = variants({ size, rounded, variant })

  return (
    <AnimatedBackground.Item
      value={value}
      className={styles.base()}
      animationClassName={styles.animation()}
    >
      <Radio
        ref={ref}
        {...radioProps}
        value={value}
        className={(renderProps) =>
          styles.radio({
            className: typeof className === 'function' ? className(renderProps) : className,
          })
        }
      >
        {label}
      </Radio>
    </AnimatedBackground.Item>
  )
})
