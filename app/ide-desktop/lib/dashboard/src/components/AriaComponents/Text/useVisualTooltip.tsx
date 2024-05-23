/**
 * @file
 *
 * A hook for creating a visual tooltip that appears when the target element is hovered over.
 */

import * as React from 'react'

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

  const { hoverProps, isHovered } = aria.useHover({
    onHoverStart: () => {
      if (targetRef.current && popoverRef.current) {
        const isOverflowing =
          targetRef.current.scrollWidth > targetRef.current.clientWidth ||
          targetRef.current.scrollHeight > targetRef.current.clientHeight

        if (isOverflowing) {
          React.startTransition(() => {
            setShowTooltip(true)
            popoverRef.current?.showPopover()
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

  const { overlayProps } = aria.useOverlayPosition({
    overlayRef: popoverRef,
    targetRef,
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    offset: 6,
    isOpen: isHovered,
  })

  return {
    targetProps: aria.mergeProps<React.HTMLAttributes<HTMLElement>>()(hoverProps, { id }),
    tooltip: isDisabled ? null : (
      <span
        id={id}
        ref={popoverRef}
        className={ariaComponents.TOOLTIP_STYLES({ className })}
        // @ts-expect-error popover attribute does not exist on React.HTMLAttributes yet
        popover=""
        aria-hidden="true"
        role="presentation"
        {...overlayProps}
        style={{
          ...overlayProps.style,
          visibility: showTooltip ? 'visible' : 'hidden',
        }}
      >
        {children}
      </span>
    ),
  }
}
