/** @file Hooks related to hovering and hover state. */
import { useHover, type DOMAttributes, type FocusableElement } from '#/components/aria'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { unsafeWriteValue } from '#/utilities/write'
import { startTransition, useId, useState, type RefObject } from 'react'

/** Props for {@link useVisualTooltip}. */
export interface OnHoverOptions {
  readonly targetRef: RefObject<HTMLElement>
  readonly isDisabled?: boolean
  /**
   * Determines when the hover state should be enabled.
   * - 'always': Tooltip is always displayed when the target element is hovered over.
   * - 'whenOverflowing': Tooltip is displayed only when the target element is overflowing.
   * - A function that returns a boolean. The function is called with the target element as an argument.
   */
  readonly display?: DisplayStrategy | ((target: HTMLElement) => boolean)
}

/** The return value of the {@link useVisualTooltip} hook. */
export interface OnHoverReturn {
  readonly targetProps: DOMAttributes<FocusableElement> & { readonly id: string }
  readonly shouldDisplay: boolean
}

/** The display strategy for the hover content. */
type DisplayStrategy = 'always' | 'whenOverflowing'

/**
 * Returns hover state.
 * Works with any element that has a ref toa HTML or HTML-like element.
 */
export function useOnHover(props: OnHoverOptions): OnHoverReturn {
  const { targetRef, isDisabled = false, display = 'always' } = props

  const [shouldDisplay, setShouldDisplay] = useState(false)

  const id = useId()

  const onHoverChange = useEventCallback((isHovered: boolean) => {
    const calculateShouldDisplay = () => {
      if (isHovered && targetRef.current != null) {
        return typeof display === 'function' ?
            display(targetRef.current)
          : DISPLAY_STRATEGIES[display](targetRef.current)
      } else {
        return false
      }
    }

    const newShouldDisplay = calculateShouldDisplay()
    if (newShouldDisplay !== shouldDisplay) {
      startTransition(() => {
        setShouldDisplay(newShouldDisplay)
      })
    }
  })

  const { hoverProps } = useHover({
    isDisabled,
    onHoverChange,
  })

  unsafeWriteValue(hoverProps, 'id', id)

  return {
    // This is SAFE because we are writing the value to the `targetHoverProps` object above.
    // eslint-disable-next-line no-restricted-syntax
    targetProps: hoverProps as OnHoverReturn['targetProps'],
    shouldDisplay,
  }
}

const DISPLAY_STRATEGIES: Record<DisplayStrategy, (target: HTMLElement) => boolean> = {
  always: () => true,
  whenOverflowing: (target) =>
    target.scrollWidth > target.clientWidth || target.scrollHeight > target.clientHeight,
}
