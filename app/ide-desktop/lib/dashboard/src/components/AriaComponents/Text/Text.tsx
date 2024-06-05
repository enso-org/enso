/**
 * @file Text component
 */
import * as React from 'react'

import * as twv from 'tailwind-variants'

import * as aria from '#/components/aria'

import * as mergeRefs from '#/utilities/mergeRefs'

import * as textProvider from './TextProvider'
import * as visualTooltip from './useVisualTooltip'

/**
 * Props for the Text component
 */
export interface TextProps
  extends Omit<aria.TextProps, 'color'>,
    twv.VariantProps<typeof TEXT_STYLE> {
  readonly lineClamp?: number
  readonly tooltip?: React.ReactElement | string | false | null
  readonly tooltipDisplay?: visualTooltip.VisualTooltipProps['display']
}

export const TEXT_STYLE = twv.tv({
  base: 'inline-block',
  variants: {
    color: {
      custom: '',
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
      body: 'text-xs leading-[20px] pt-[1px] pb-[3px]',
      h1: 'text-xl leading-[29px] pt-[2px] pb-[5px]',
      subtitle: 'text-[13.5px] leading-[20px] pt-[1px] pb-[3px]',
    },
    weight: {
      custom: '',
      bold: 'font-bold',
      semibold: 'font-semibold',
      extraBold: 'font-extrabold',
      medium: 'font-medium',
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
    textSelection: {
      auto: '',
      none: 'select-none',
      word: 'select-text',
      all: 'select-all',
    },
    disableLineHeightCompensation: { true: '' },
  },
  defaultVariants: {
    variant: 'body',
    weight: 'medium',
    transform: 'none',
    color: 'primary',
    italic: false,
    nowrap: false,
    monospace: false,
    disableLineHeightCompensation: false,
    textSelection: 'auto',
  },
  compoundVariants: [
    { variant: 'h1', class: 'font-bold' },
    {
      variant: 'h1',
      disableLineHeightCompensation: true,
      class: 'pt-[unset] pb-[unset]',
    },
    {
      variant: 'body',
      disableLineHeightCompensation: true,
      class: 'pt-[unset] pb-[unset]',
    },
    {
      variant: 'subtitle',
      disableLineHeightCompensation: true,
      class: 'pt-[unset] pb-[unset]',
    },
  ],
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
    weight = 'medium',
    nowrap = false,
    monospace = false,
    transform = 'none',
    truncate,
    lineClamp = 1,
    children,
    color = 'primary',
    balance,
    elementType: ElementType = 'span',
    tooltip: tooltipElement = children,
    tooltipDisplay = 'whenOverflowing',
    textSelection,
    disableLineHeightCompensation = false,
    ...ariaProps
  } = props

  const textElementRef = React.useRef<HTMLElement>(null)
  const textContext = textProvider.useTextContext()

  const textClasses = TEXT_STYLE({
    variant,
    weight,
    transform,
    monospace,
    italic,
    nowrap,
    truncate,
    color,
    balance,
    textSelection,
    disableLineHeightCompensation:
      disableLineHeightCompensation || textContext.isInsideTextComponent,
    className,
  })

  const isToolipDisabled = () => {
    if (tooltipDisplay === 'whenOverflowing') {
      return !truncate
    } else if (tooltipDisplay === 'always') {
      return tooltipElement === false || tooltipElement == null
    } else {
      return false
    }
  }

  const { tooltip, targetProps } = visualTooltip.useVisualTooltip({
    isDisabled: isToolipDisabled(),
    targetRef: textElementRef,
    display: tooltipDisplay,
    children: tooltipElement,
  })

  return (
    <textProvider.TextProvider value={{ isInsideTextComponent: true }}>
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
    </textProvider.TextProvider>
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
