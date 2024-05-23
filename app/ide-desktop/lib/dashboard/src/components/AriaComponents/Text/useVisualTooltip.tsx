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
}

/**
 * Creates a tooltip that appears when the target element is hovered over.
 * Works with any element that has a ref.
 * doesn't have a11y support. It's a visual tooltip.
 * Common use case is to show a tooltip when the content of an element is overflowing.
 */
export function useVisualTooltip(props: VisualTooltipProps) {
  const { children, targetRef, className, isDisabled = false } = props

  const [showTooltip, setShowTooltip] = React.useState(false)
  const popoverRef = React.useRef<HTMLDivElement>(null)
  const id = React.useId()

  const { hoverProps } = aria.useHover({
    onHoverStart: () => {
      if (targetRef.current && popoverRef.current) {
        const isOverflowing =
          targetRef.current.scrollWidth > targetRef.current.clientWidth ||
          targetRef.current.scrollHeight > targetRef.current.clientHeight

        console.log('isOverflowing', isOverflowing, {
          scrollWidth: targetRef.current.scrollWidth,
          clientWidth: targetRef.current.clientWidth,
          scrollHeight: targetRef.current.scrollHeight,
          clientHeight: targetRef.current.clientHeight,
        })

        if (isOverflowing) {
          React.startTransition(() => {
            setShowTooltip(true)
            popoverRef.current?.showPopover()
            updatePosition()
          })
        }
      }
    },
    onHoverEnd: () => {
      React.startTransition(() => {
        setShowTooltip(false)
        popoverRef.current?.hidePopover()
      })
    },
    isDisabled,
  })

  // eslint-disable-next-line @typescript-eslint/unbound-method
  const { overlayProps, updatePosition } = aria.useOverlayPosition({
    overlayRef: popoverRef,
    targetRef,
  })

  return {
    targetProps: aria.mergeProps<React.HTMLAttributes<HTMLElement>>()(hoverProps, { id }),
    tooltip: (
      <div
        id={id}
        ref={popoverRef}
        className={ariaComponents.TOOLTIP_STYLES({
          className: clsx(className, { hidden: !showTooltip || isDisabled }),
        })}
        // @ts-expect-error popover attribute does not exist on React.HTMLAttributes yet
        popover=""
        {...overlayProps}
      >
        {children}
      </div>
    ),
  }
}
