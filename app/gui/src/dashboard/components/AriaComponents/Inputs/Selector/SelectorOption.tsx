/** @file An option in a selector. */
import { AnimatedBackground } from '#/components/AnimatedBackground'
import { Radio, type RadioProps } from '#/components/aria'
import { forwardRef } from '#/utilities/react'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import * as React from 'react'
import { memo } from 'react'
import { TEXT_STYLE } from '../../Text'

/** Props for a {@link SelectorOption}. */
export interface SelectorOptionProps
  extends RadioProps,
    VariantProps<typeof SELECTOR_OPTION_STYLES> {
  readonly label: string
}

export const SELECTOR_OPTION_STYLES = tv({
  base: 'flex flex-1 w-full cursor-pointer',
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
      medium: { base: 'min-h-[31px]', radio: 'px-[9px] py-[3.5px]' },
      small: { base: 'min-h-6', radio: 'px-[7px] py-[1.5px]' },
    },
    isHovered: {
      true: { radio: '' },
      false: { radio: '' },
    },
    isSelected: {
      // specified in compoundVariants
      true: { radio: '' },
      false: { radio: '' },
    },
    isFocusVisible: {
      // specified in compoundVariants
      true: {
        radio:
          'outline outline-2 outline-transparent outline-offset-[-6px] focus-visible:outline-primary focus-visible:outline-offset-[2px] transition-[outline-offset] duration-200',
      },
      false: { radio: '' },
    },

    isPressed: {
      // specified in compoundVariants
      true: { radio: '' },
      false: { radio: '' },
    },

    variant: {
      // specified in compoundVariants
      outline: {
        base: '',
      },
    },
  },
  slots: {
    animation: 'bg-primary',
    radio: TEXT_STYLE({
      className:
        'relative flex flex-1 w-full items-center justify-center transition-colors duration-200',
      variant: 'body',
    }),
    hover:
      'absolute inset-x-0 inset-y-0 transition-[background-color,transform] duration-200 isolate',
  },
  compoundSlots: [
    {
      slots: ['radio', 'animation', 'base', 'hover'],
      rounded: 'none',
      class: 'rounded-none',
    },
    {
      slots: ['radio', 'animation', 'base', 'hover'],
      rounded: 'small',
      class: 'rounded-sm',
    },
    {
      slots: ['radio', 'animation', 'base', 'hover'],
      rounded: 'medium',
      class: 'rounded-md',
    },
    {
      slots: ['radio', 'animation', 'base', 'hover'],
      rounded: 'large',
      class: 'rounded-lg',
    },
    {
      slots: ['radio', 'animation', 'base', 'hover'],
      rounded: 'xlarge',
      class: 'rounded-xl',
    },
    {
      slots: ['radio', 'animation', 'base', 'hover'],
      rounded: 'xxlarge',
      class: 'rounded-2xl',
    },
    {
      slots: ['radio', 'animation', 'base', 'hover'],
      rounded: 'xxxlarge',
      class: 'rounded-3xl',
    },
    {
      slots: ['radio', 'animation', 'base', 'hover'],
      rounded: 'full',
      class: 'rounded-full',
    },
  ],
  compoundVariants: [
    {
      variant: 'outline',
      isSelected: true,
      class: { radio: TEXT_STYLE({ variant: 'body', color: 'invert' }) },
    },
    {
      variant: 'outline',
      isHovered: true,
      isSelected: false,
      class: { hover: 'bg-invert/50' },
    },
    {
      variant: 'outline',
      isPressed: true,
      class: { hover: 'bg-invert scale-x-[0.95] scale-y-[0.85]' },
    },
    {
      variant: 'outline',
      isSelected: false,
      class: { radio: TEXT_STYLE({ variant: 'body', color: 'primary' }) },
    },
    {
      size: 'small',
      class: { hover: 'inset-[2px]' },
    },
    {
      size: 'medium',
      class: { hover: 'inset-[3px]' },
    },
  ],
  defaultVariants: {
    size: 'medium',
    rounded: 'xxxlarge',
    variant: 'outline',
  },
})

export const SelectorOption = memo(
  forwardRef(function SelectorOption(
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
          className={(renderProps) => {
            return styles.radio({
              className: typeof className === 'function' ? className(renderProps) : className,
              ...renderProps,
            })
          }}
        >
          {({ isHovered, isSelected, isPressed }) => (
            <>
              <div className={styles.hover({ isHovered, isSelected, isPressed })} />
              <span className="isolate">{label}</span>
            </>
          )}
        </Radio>
      </AnimatedBackground.Item>
    )
  }),
)
