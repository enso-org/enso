/** @file Text component */
import * as aria from '#/components/aria'
import type { TooltipElementType } from '#/components/VisualTooltip'
import * as visualTooltip from '#/components/VisualTooltip'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { mergeRefs } from '#/utilities/mergeRefs'
import type { VariantProps } from '#/utilities/tailwindVariants'
import * as React from 'react'
import { memo } from 'react'
import type { TestIdProps } from '../types'
import * as textProvider from './TextProvider'
import { TEXT_STYLE } from './variants'

/** Props for the Text component */
export interface TextProps
  extends Omit<aria.TextProps, 'color'>,
    VariantProps<typeof TEXT_STYLE>,
    TestIdProps {
  readonly elementType?: keyof HTMLElementTagNameMap
  readonly lineClamp?: number
  readonly tooltip?: TooltipElementType
  readonly tooltipTriggerRef?: React.RefObject<HTMLElement>
  readonly tooltipDisplay?: visualTooltip.VisualTooltipOptions['display'] | 'never'
  readonly tooltipPlacement?: aria.Placement
  readonly tooltipOffset?: number
  readonly tooltipCrossOffset?: number
}

/** Heading props */
export interface HeadingProps extends Omit<TextProps, 'elementType'> {
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  readonly level?: '1' | '2' | '3' | '4' | '5' | '6' | 1 | 2 | 3 | 4 | 5 | 6
}

/** Heading component */
const Heading = memo(
  React.forwardRef(function Heading(props: HeadingProps, ref: React.Ref<HTMLHeadingElement>) {
    const { level = 1, ...textProps } = props
    return <Text ref={ref} elementType={`h${level}`} variant="h1" balance {...textProps} />
  }),
)

/** Body props */
export interface BodyProps extends Omit<TextProps, 'elementType'> {}

/** Body component */
const Body = memo(
  React.forwardRef(function Body(props: BodyProps, ref: React.Ref<HTMLParagraphElement>) {
    return <Text ref={ref} variant="body" balance {...props} />
  }),
)

/** Text group component. It's used to visually group text elements together */
function TextGroup(props: React.PropsWithChildren) {
  return (
    <textProvider.TextProvider value={{ isInsideTextComponent: true }}>
      {props.children}
    </textProvider.TextProvider>
  )
}

/** Text component that supports truncation and show a tooltip on hover when text is truncated */
export const Text = Object.assign(
  memo(
    React.forwardRef(function Text(props: TextProps, ref: React.Ref<HTMLSpanElement>) {
      const {
        className,
        variant,
        font,
        italic,
        weight,
        nowrap,
        monospace,
        transform,
        truncate,
        lineClamp = 1,
        children,
        color,
        balance,
        testId,
        elementType: ElementType = 'span',
        tooltip: tooltipElement = children,
        tooltipDisplay = 'whenOverflowing',
        tooltipPlacement,
        tooltipOffset,
        tooltipCrossOffset,
        textSelection,
        disableLineHeightCompensation = false,
        align,
        ...ariaProps
      } = props

      const textElementRef = React.useRef<HTMLElement>(null)
      const textContext = textProvider.useTextContext()

      const textClasses = TEXT_STYLE({
        variant,
        font,
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
          disableLineHeightCompensation === false ?
            textContext.isInsideTextComponent
          : disableLineHeightCompensation,
        className,
        align,
      })

      const isTooltipDisabled = useEventCallback(() => {
        if (tooltipDisplay === 'whenOverflowing') {
          return truncate == null
        }
        if (tooltipDisplay === 'always') {
          return tooltipElement === false || tooltipElement == null
        }

        return tooltipDisplay === 'never'
      })

      const { tooltip, targetProps } = visualTooltip.useVisualTooltip({
        isDisabled: isTooltipDisabled(),
        targetRef: textElementRef,
        display: tooltipDisplay === 'never' ? () => false : tooltipDisplay,
        children: tooltipElement,
        ...(tooltipPlacement || tooltipOffset != null || tooltipCrossOffset != null ?
          {
            overlayPositionProps: {
              ...(tooltipPlacement && { placement: tooltipPlacement }),
              ...(tooltipOffset != null && { offset: tooltipOffset }),
              ...(tooltipCrossOffset != null && { crossOffset: tooltipCrossOffset }),
            },
          }
        : {}),
      })

      return (
        <textProvider.TextProvider value={{ isInsideTextComponent: true }}>
          <ElementType
            // @ts-expect-error This is caused by the type-safe `elementType` type.
            ref={(el) => {
              // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
              mergeRefs(ref, textElementRef)(el)
            }}
            data-testid={testId}
            className={textClasses}
            {...aria.mergeProps<React.HTMLAttributes<HTMLElement>>()(
              ariaProps,
              targetProps,
              truncate === 'custom' ?
                // eslint-disable-next-line @typescript-eslint/naming-convention,no-restricted-syntax
                ({ style: { '--line-clamp': `${lineClamp}` } } as React.HTMLAttributes<HTMLElement>)
              : {},
            )}
          >
            {children}
          </ElementType>

          {tooltip}
        </textProvider.TextProvider>
      )
    }),
  ),
  /* eslint-disable @typescript-eslint/naming-convention */
  {
    Heading,
    Body,
    Group: TextGroup,
  },
  /* eslint-enable @typescript-eslint/naming-convention */
)
