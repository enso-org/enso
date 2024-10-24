/**
 * @file
 *
 * A hook for creating a visual tooltip that appears when the target element is hovered over.
 */

import * as React from 'react'

import * as eventCallback from '#/hooks/eventCallbackHooks'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import Portal from '#/components/Portal'

/** Props for {@link useVisualTooltip}. */
export interface VisualTooltipProps
  extends Pick<ariaComponents.TooltipProps, 'maxWidth' | 'rounded' | 'size' | 'variant'> {
  readonly children: React.ReactNode
  readonly className?: string
  readonly targetRef: React.RefObject<HTMLElement>
  readonly isDisabled?: boolean
  readonly overlayPositionProps?: Pick<
    aria.AriaPositionProps,
    'containerPadding' | 'offset' | 'placement'
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

const DEFAULT_OFFSET = 6
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

  const {
    containerPadding = 0,
    offset = DEFAULT_OFFSET,
    placement = 'bottom',
  } = overlayPositionProps

  const popoverRef = React.useRef<HTMLDivElement>(null)
  const id = React.useId()

  const state = aria.useTooltipTriggerState({
    closeDelay: DEFAULT_DELAY,
    delay: DEFAULT_DELAY,
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

    if (shouldDisplay()) {
      state.open()
    } else {
      state.close()
    }
  })

  const { hoverProps: targetHoverProps } = aria.useHover({
    isDisabled,
    onHoverChange: handleHoverChange,
  })

  const { hoverProps: tooltipHoverProps } = aria.useHover({
    isDisabled,
    onHoverChange: handleHoverChange,
  })

  const { tooltipProps } = aria.useTooltipTrigger({}, state, targetRef)

  // eslint-disable-next-line @typescript-eslint/unbound-method
  const { overlayProps, updatePosition } = aria.useOverlayPosition({
    isOpen: state.isOpen,
    overlayRef: popoverRef,
    targetRef,
    offset,
    placement,
    containerPadding,
  })

  const createTooltipElement = () => (
    <Portal onMount={updatePosition}>
      <span
        ref={popoverRef}
        {...aria.mergeProps<React.HTMLAttributes<HTMLDivElement>>()(
          overlayProps,
          tooltipProps,
          tooltipHoverProps,
          {
            id,
            className: ariaComponents.TOOLTIP_STYLES({
              className,
              variant,
              rounded,
              size,
              maxWidth,
            }),
            // eslint-disable-next-line @typescript-eslint/naming-convention
            'aria-hidden': true,
            // Note that this is a `@ts-expect-error` so that an update to the outdated type
            // definitions will notify that this `@ts-expect-error` can be safely removed.
            // @ts-expect-error This is a new DOM property.
            popover: '',
            role: 'presentation',
            'data-testid': testId,
            // Remove z-index from the overlay style because it is not needed.
            // We show the latest element on top, and z-index can cause issues with
            // the stacking context.
            style: { zIndex: '' },
          },
        )}
      >
        {children}
      </span>
    </Portal>
  )

  return {
    targetProps: aria.mergeProps<React.HTMLAttributes<HTMLElement>>()(targetHoverProps, { id }),
    tooltip: state.isOpen ? createTooltipElement() : null,
  } as const
}

const DISPLAY_STRATEGIES: Record<DisplayStrategy, (target: HTMLElement) => boolean> = {
  always: () => true,
  whenOverflowing: (target) =>
    target.scrollWidth > target.clientWidth || target.scrollHeight > target.clientHeight,
}
