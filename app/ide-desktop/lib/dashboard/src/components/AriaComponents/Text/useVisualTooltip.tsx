/**
 * @file
 *
 * A hook for creating a visual tooltip that appears when the target element is hovered over.
 */

import * as React from 'react'

import clsx from 'clsx'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import Portal from '#/components/Portal'

/**
 * Props for {@link useVisualTooltip}.
 */
export interface VisualTooltipProps {
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

/**
 * The display strategy for the tooltip.
 */
type DisplayStrategy = 'always' | 'whenOverflowing'

const DEFAULT_OFFSET = 6

/**
 * Creates a tooltip that appears when the target element is hovered over.
 * Works with any element that has a ref.
 * doesn't have a11y support. It's a visual tooltip.
 * Common use case is to show a tooltip when the content of an element is overflowing,
 * Or show a description of the element when hovered over.
 */
export function useVisualTooltip(props: VisualTooltipProps) {
  const {
    children,
    targetRef,
    className,
    isDisabled = false,
    overlayPositionProps = {},
    display = 'always',
    testId = 'visual-tooltip',
  } = props

  const {
    containerPadding = 0,
    offset = DEFAULT_OFFSET,
    placement = 'bottom',
  } = overlayPositionProps

  const popoverRef = React.useRef<HTMLDivElement>(null)
  const id = React.useId()

  const { hoverProps, isHovered } = aria.useHover({
    onHoverStart: () => {
      if (targetRef.current) {
        const shouldDisplay =
          typeof display === 'function'
            ? display(targetRef.current)
            : DISPLAY_STRATEGIES[display](targetRef.current)

        if (shouldDisplay) {
          popoverRef.current?.showPopover()
        }
      }
    },
    onHoverEnd: () => {
      popoverRef.current?.hidePopover()
    },
    isDisabled,
  })

  const { overlayProps } = aria.useOverlayPosition({
    isOpen: isHovered,
    overlayRef: popoverRef,
    targetRef,
    offset,
    placement,
    containerPadding,
  })

  return {
    targetProps: aria.mergeProps<React.HTMLAttributes<HTMLElement>>()(hoverProps, { id }),
    tooltip: isDisabled ? null : (
      <Portal>
        <span
          id={id}
          ref={popoverRef}
          className={ariaComponents.TOOLTIP_STYLES({
            className: clsx(className, 'hidden animate-in fade-in [&:popover-open]:flex'),
          })}
          // @ts-expect-error popover attribute does not exist on React.HTMLAttributes yet
          popover=""
          aria-hidden="true"
          role="presentation"
          data-testid={testId}
          {...overlayProps}
        >
          {children}
        </span>
      </Portal>
    ),
  } as const
}

const DISPLAY_STRATEGIES: Record<DisplayStrategy, (target: HTMLElement) => boolean> = {
  always: () => true,
  whenOverflowing: target =>
    target.scrollWidth > target.clientWidth || target.scrollHeight > target.clientHeight,
}
