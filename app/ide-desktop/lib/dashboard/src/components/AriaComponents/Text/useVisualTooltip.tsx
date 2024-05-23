/**
 * @file
 *
 * A hook for creating a visual tooltip that appears when the target element is hovered over.
 */

import * as React from 'react'

import clsx from 'clsx'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

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
}

const DEFAULT_OFFSET = 6

/**
 * Creates a tooltip that appears when the target element is hovered over.
 * Works with any element that has a ref.
 * doesn't have a11y support. It's a visual tooltip.
 * Common use case is to show a tooltip when the content of an element is overflowing.
 */
export function useVisualTooltip(props: VisualTooltipProps) {
  const { children, targetRef, className, isDisabled = false, overlayPositionProps = {} } = props

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
        const isOverflowing =
          targetRef.current.scrollWidth > targetRef.current.clientWidth ||
          targetRef.current.scrollHeight > targetRef.current.clientHeight

        if (isOverflowing) {
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
    overlayRef: popoverRef,
    targetRef,
    offset,
    isOpen: isHovered,
    placement,
    containerPadding,
  })

  return {
    targetProps: aria.mergeProps<React.HTMLAttributes<HTMLElement>>()(hoverProps, { id }),
    tooltip: isDisabled ? null : (
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
        {...overlayProps}
      >
        {children}
      </span>
    ),
  }
}
