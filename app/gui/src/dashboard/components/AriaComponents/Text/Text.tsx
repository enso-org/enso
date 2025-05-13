/** @file Text component */
import { mergeProps, type TextProps as AriaTextProps, type Placement } from '#/components/aria'
import type { TooltipElementType } from '#/components/AriaComponents/VisualTooltip'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { mergeRefs } from '#/utilities/mergeRefs'
import { forwardRef } from '#/utilities/react'
import type { VariantProps } from '#/utilities/tailwindVariants'
import {
  memo,
  useRef,
  type FC,
  type HTMLAttributes,
  type PropsWithChildren,
  type Ref,
  type RefAttributes,
  type RefObject,
} from 'react'
import type { TestIdProps } from '../types'
import { useVisualTooltip, type VisualTooltipOptions } from '../VisualTooltip/useVisualTooltip'
import { useTextContext } from './hooks'
import { TextProvider } from './TextProvider'
import { TEXT_STYLE } from './variants'

/** Props for the Text component */
export interface TextProps
  extends Omit<AriaTextProps, 'color'>,
    VariantProps<typeof TEXT_STYLE>,
    TestIdProps {
  readonly elementType?: keyof HTMLElementTagNameMap
  readonly lineClamp?: number
  readonly tooltip?: TooltipElementType
  readonly tooltipTriggerRef?: RefObject<HTMLElement>
  readonly tooltipDisplay?: VisualTooltipOptions['display'] | 'never'
  readonly tooltipPlacement?: Placement
  readonly tooltipOffset?: number
  readonly tooltipCrossOffset?: number
}

/** Text component that supports truncation and show a tooltip on hover when text is truncated */
// eslint-disable-next-line no-restricted-syntax
export const Text = memo(
  forwardRef(function Text(props: TextProps, ref: Ref<HTMLSpanElement>) {
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
      variants = TEXT_STYLE,
      ...ariaProps
    } = props

    const textElementRef = useRef<HTMLElement>(null)
    const textContext = useTextContext()

    const textClasses = variants({
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

    const { tooltip, targetProps } = useVisualTooltip({
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
      <TextProvider value={{ isInsideTextComponent: true }}>
        <ElementType
          // @ts-expect-error This is caused by the type-safe `elementType` type.
          ref={(el) => {
            // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
            mergeRefs(ref, textElementRef)(el)
          }}
          data-testid={testId}
          className={textClasses}
          {...mergeProps<HTMLAttributes<HTMLElement>>()(
            ariaProps,
            targetProps,
            truncate === 'custom' ?
              // eslint-disable-next-line @typescript-eslint/naming-convention,no-restricted-syntax
              ({ style: { '--line-clamp': `${lineClamp}` } } as HTMLAttributes<HTMLElement>)
            : {},
          )}
        >
          {children}
        </ElementType>

        {tooltip}
      </TextProvider>
    )
  }),
) as unknown as FC<RefAttributes<HTMLSpanElement> & TextProps> & {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Heading: typeof Heading
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Body: typeof Body
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Group: FC<React.PropsWithChildren>
}

/** Heading props */
export interface HeadingProps extends Omit<TextProps, 'elementType'> {
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  readonly level?: '1' | '2' | '3' | '4' | '5' | '6' | 1 | 2 | 3 | 4 | 5 | 6
}

/** Heading component */
const Heading = memo(
  forwardRef(function Heading(props: HeadingProps, ref: Ref<HTMLHeadingElement>) {
    const { level = 1, ...textProps } = props
    return <Text ref={ref} elementType={`h${level}`} variant="h1" balance {...textProps} />
  }),
)

/** Body props */
export interface BodyProps extends Omit<TextProps, 'elementType'> {}

/** Body component */
const Body = memo(
  forwardRef(function Body(props: BodyProps, ref: React.Ref<HTMLParagraphElement>) {
    return <Text ref={ref} variant="body" balance {...props} />
  }),
)

/** Text group component. It's used to visually group text elements together */
function TextGroup(props: PropsWithChildren) {
  return <TextProvider value={{ isInsideTextComponent: true }}>{props.children}</TextProvider>
}

Text.Heading = Heading
Text.Body = Body
Text.Group = TextGroup
