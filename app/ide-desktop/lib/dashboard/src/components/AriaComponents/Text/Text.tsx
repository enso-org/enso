/**
 * @file Text component
 */
import * as React from 'react'

import * as twv from 'tailwind-variants'

import * as aria from '#/components/aria'

import * as mergeRefs from '#/utilities/mergeRefs'

import * as visualTooltip from './useVisualTooltip'

/**
 * Props for the Text component
 */
export interface TextProps
  extends Omit<aria.TextProps, 'color'>,
    twv.VariantProps<typeof TEXT_STYLE> {
  readonly lineClamp?: number
}

export const TEXT_STYLE = twv.tv({
  base: 'inline-block',
  variants: {
    color: {
      primary: 'text-primary/60',
      danger: 'text-danger',
      success: 'text-share',
      disabled: 'text-primary/30',
      invert: 'text-white',
    },
    // we use custom padding for the text variants to make sure the text is aligned with the grid
    // leading is also adjusted to make sure the text is aligned with the grid
    // leading should always be after the text size to make sure it is not stripped by twMerge
    variant: {
      custom: '',
      body: 'pt-[1px] pb-[3px] text-xs leading-[20px]',
      h1: 'pt-0.5 pb-[7px] text-xl leading-[29px]',
      subtitle: 'pt-0.5 pb-[3px] text-[13px] leading-[19px]',
    },
    weight: {
      custom: '',
      bold: 'font-bold',
      semibold: 'font-semibold',
      extraBold: 'font-extrabold',
      normal: 'font-normal',
      thin: 'font-thin',
    },
    balance: {
      true: 'text-balance',
    },
    transform: {
      none: '',
      capitalize: 'text-capitalize',
      lowercase: 'text-lowercase',
      uppercase: 'text-uppercase',
    },
    truncate: {
      /* eslint-disable @typescript-eslint/naming-convention */
      '1': 'truncate ellipsis w-full',
      '2': 'line-clamp-2 ellipsis w-full',
      '3': 'line-clamp-3 ellipsis w-full',
      '4': 'line-clamp-4 ellipsis w-full',
      '5': 'line-clamp-5 ellipsis w-full',
      '6': 'line-clamp-6 ellipsis w-full',
      '7': 'line-clamp-7 ellipsis w-full',
      '8': 'line-clamp-8 ellipsis w-full',
      '9': 'line-clamp-9 ellipsis w-full',
      custom: 'line-clamp-[var(--line-clamp)] ellipsis w-full',
      /* eslint-enable @typescript-eslint/naming-convention */
    },
    monospace: { true: 'font-mono' },
    italic: { true: 'italic' },
    nowrap: { true: 'whitespace-nowrap' },
  },
  defaultVariants: {
    variant: 'body',
    weight: 'normal',
    transform: 'none',
    color: 'primary',
    italic: false,
    nowrap: false,
    monospace: false,
  },
  compoundVariants: [{ variant: 'h1', class: 'font-bold text-balance' }],
})

/**
 * Text component that supports truncation and show a tooltip on hover when text is truncated
 */
// eslint-disable-next-line no-restricted-syntax
export const Text = React.forwardRef(function Text(
  props: TextProps,
  ref: React.Ref<HTMLSpanElement>
) {
  const {
    className,
    variant = 'body',
    italic = false,
    weight = 'normal',
    nowrap = false,
    monospace = false,
    transform = 'none',
    truncate,
    lineClamp = 1,
    children,
    color = 'primary',
    balance,
    elementType: ElementType = 'span',
    ...ariaProps
  } = props

  const textElementRef = React.useRef<HTMLElement>(null)

  const textClasses = TEXT_STYLE({
    variant,
    weight,
    transform,
    monospace,
    italic,
    nowrap,
    truncate,
    color,
    className,
    balance,
  })

  const { tooltip, targetProps } = visualTooltip.useVisualTooltip({
    isDisabled: !truncate,
    targetRef: textElementRef,
    display: 'whenOverflowing',
    children,
  })

  return (
    <>
      {/* @ts-expect-error We suppose that elementType is a valid HTML element */}
      <ElementType
        ref={mergeRefs.mergeRefs(ref, textElementRef)}
        className={textClasses}
        {...aria.mergeProps<React.HTMLAttributes<HTMLElement>>()(
          ariaProps,
          targetProps,
          truncate === 'custom'
            ? // eslint-disable-next-line @typescript-eslint/naming-convention,no-restricted-syntax
              ({ style: { '--line-clamp': `${lineClamp}` } } as React.HTMLAttributes<HTMLElement>)
            : {}
        )}
      >
        {children}
      </ElementType>
      {tooltip}
    </>
  )
  // eslint-disable-next-line no-restricted-syntax
}) as unknown as React.FC<React.RefAttributes<HTMLSpanElement> & TextProps> & {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Heading: React.FC<HeadingProps>
}

/**
 * Heading props
 */
export interface HeadingProps extends Omit<TextProps, 'elementType'> {
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  readonly level?: '1' | '2' | '3' | '4' | '5' | '6' | 1 | 2 | 3 | 4 | 5 | 6
}

/**
 * Heading component
 */
Text.Heading = React.forwardRef(function Heading(
  props: HeadingProps,
  ref: React.Ref<HTMLHeadingElement>
) {
  const { level = 1, ...textProps } = props
  return <Text ref={ref} elementType={`h${level}`} variant="h1" balance {...textProps} />
})
