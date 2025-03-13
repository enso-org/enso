/** @file A hook for creating a visual tooltip that appears when the target element is hovered over. */
import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import { VisualTooltipInner } from '#/components/AriaComponents/Text/VisualTooltipInner'
import * as eventCallback from '#/hooks/eventCallbackHooks'
import * as React from 'react'

/** Props for {@link useVisualTooltip}. */
export interface VisualTooltipProps
  extends Pick<ariaComponents.TooltipProps, 'maxWidth' | 'rounded' | 'size' | 'variant'> {
  readonly children: React.ReactNode
  readonly className?: string
  readonly targetRef: React.RefObject<HTMLElement>
  readonly triggerRef?: React.RefObject<HTMLElement> | undefined
  readonly isDisabled?: boolean
  readonly overlayPositionProps?: Pick<
    aria.AriaPositionProps,
    'containerPadding' | 'crossOffset' | 'offset' | 'placement'
  >
  /**
   * Determines when the tooltip should be displayed.
   * - 'always': Tooltip is always displayed when the target element is hovered over.
   * - 'whenOverflowing': Tooltip is displayed only when the target element is overflowing.
   * - A function that returns a boolean. The function is called with the target element as an argument.
   */
  readonly display?: DisplayStrategy | ((target: HTMLElement) => boolean)
  readonly testId?: string
}

/** The return value of the {@link useVisualTooltip} hook. */
export interface VisualTooltipReturn {
  readonly targetProps: aria.DOMAttributes<aria.FocusableElement> & { readonly id: string }
  readonly tooltip: JSX.Element | null
}

/** The display strategy for the tooltip. */
type DisplayStrategy = 'always' | 'whenOverflowing'

const DEFAULT_DELAY = 250

/**
 * Creates a tooltip that appears when the target element is hovered over.
 * Works with any element that has a ref.
 * doesn't have a11y support. It's a visual tooltip.
 * Common use case is to show a tooltip when the content of an element is overflowing,
 * Or show a description of the element when hovered over.
 */
export function useVisualTooltip(props: VisualTooltipProps): VisualTooltipReturn {
  const {
    children,
    targetRef,
    triggerRef = targetRef,
    className,
    isDisabled = false,
    overlayPositionProps = {},
    display = 'always',
    testId = 'visual-tooltip',
    rounded,
    variant,
    size,
    maxWidth,
  } = props

  const [isTooltipDisabled, setIsTooltipDisabled] = React.useState(true)

  const id = React.useId()

  const disabled = isDisabled || isTooltipDisabled

  const state = aria.useTooltipTriggerState({
    closeDelay: DEFAULT_DELAY,
    delay: DEFAULT_DELAY,
    isDisabled: disabled,
  })

  const handleHoverChange = eventCallback.useEventCallback((isHovered: boolean) => {
    const shouldDisplay = () => {
      if (isHovered && targetRef.current != null) {
        return typeof display === 'function' ?
            display(targetRef.current)
          : DISPLAY_STRATEGIES[display](targetRef.current)
      } else {
        return false
      }
    }

    React.startTransition(() => {
      setIsTooltipDisabled(!shouldDisplay())

      if (shouldDisplay()) {
        state.open()
      } else {
        state.close()
      }
    })
  })

  const { hoverProps: targetHoverProps } = aria.useHover({
    isDisabled,
    onHoverChange: handleHoverChange,
  })

  return {
    targetProps: aria.mergeProps<React.HTMLAttributes<HTMLElement>>()(targetHoverProps, { id }),
    tooltip:
      state.isOpen ?
        React.createElement(VisualTooltipInner, {
          id,
          overlayPositionProps,
          className,
          variant,
          rounded,
          size,
          maxWidth,
          children,
          testId,
          state,
          targetRef,
          triggerRef,
          disabled,
          handleHoverChange,
        })
      : null,
  } as const
}

const DISPLAY_STRATEGIES: Record<DisplayStrategy, (target: HTMLElement) => boolean> = {
  always: () => true,
  whenOverflowing: (target) =>
    target.scrollWidth > target.clientWidth || target.scrollHeight > target.clientHeight,
}
