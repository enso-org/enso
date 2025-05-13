/** @file A hook for creating a visual tooltip that appears when the target element is hovered over. */
import {
  useHover,
  useTooltipTriggerState,
  type AriaPositionProps,
  type DOMAttributes,
  type FocusableElement,
} from '#/components/aria'
import { VisualTooltipInner } from '#/components/AriaComponents/Text/VisualTooltipInner'
import type { TooltipProps } from '#/components/AriaComponents/Tooltip'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { unsafeWriteValue } from '#/utilities/write'
import {
  createElement,
  startTransition,
  useId,
  useState,
  type ReactNode,
  type RefObject,
} from 'react'

/** Props for {@link useVisualTooltip}. */
export interface VisualTooltipOptions
  extends Pick<TooltipProps, 'maxWidth' | 'rounded' | 'size' | 'variant'> {
  readonly children: ReactNode
  readonly className?: string
  readonly targetRef: RefObject<HTMLElement>
  readonly triggerRef?: RefObject<HTMLElement> | undefined
  readonly isDisabled?: boolean
  readonly overlayPositionProps?: Pick<
    AriaPositionProps,
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
  readonly targetProps: DOMAttributes<FocusableElement> & { readonly id: string }
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
export function useVisualTooltip(props: VisualTooltipOptions): VisualTooltipReturn {
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

  const [isTooltipDisabled, setIsTooltipDisabled] = useState(true)

  const id = useId()

  const disabled = isDisabled || isTooltipDisabled

  const state = useTooltipTriggerState({
    closeDelay: DEFAULT_DELAY,
    delay: DEFAULT_DELAY,
    isDisabled: disabled,
  })

  const handleHoverChange = useEventCallback((isHovered: boolean) => {
    const shouldDisplay = () => {
      if (isHovered && targetRef.current != null) {
        return typeof display === 'function' ?
            display(targetRef.current)
          : DISPLAY_STRATEGIES[display](targetRef.current)
      } else {
        return false
      }
    }

    startTransition(() => {
      setIsTooltipDisabled(!shouldDisplay())

      if (shouldDisplay()) {
        state.open()
      } else {
        state.close()
      }
    })
  })

  const { hoverProps: targetHoverProps } = useHover({
    isDisabled,
    onHoverChange: handleHoverChange,
  })

  unsafeWriteValue(targetHoverProps, 'id', id)

  return {
    // This is SAFE because we are writing the value to the targetHoverProps object
    // above.
    // eslint-disable-next-line no-restricted-syntax
    targetProps: targetHoverProps as VisualTooltipReturn['targetProps'],
    tooltip:
      state.isOpen ?
        createElement(VisualTooltipInner, {
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
