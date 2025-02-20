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
        <TooltipInner
          id={id}
          overlayPositionProps={overlayPositionProps}
          className={className}
          variant={variant}
          rounded={rounded}
          size={size}
          maxWidth={maxWidth}
          children={children}
          testId={testId}
          state={state}
          targetRef={targetRef}
          triggerRef={triggerRef}
          disabled={disabled}
          handleHoverChange={handleHoverChange}
        />
      : null,
  } as const
}

/** Props for {@link TooltipInner}. */
interface TooltipInnerProps
  extends Pick<ariaComponents.TooltipProps, 'maxWidth' | 'rounded' | 'size' | 'variant'> {
  readonly id: string
  readonly disabled: boolean
  readonly handleHoverChange: (isHovered: boolean) => void
  readonly state: aria.TooltipTriggerState
  readonly targetRef: React.RefObject<HTMLElement>
  readonly triggerRef: React.RefObject<HTMLElement>
  readonly children: React.ReactNode
  readonly className?: string | undefined
  readonly testId?: string | undefined
  readonly overlayPositionProps: Pick<
    aria.AriaPositionProps,
    'containerPadding' | 'crossOffset' | 'offset' | 'placement'
  >
}

/** The inner component of the tooltip. */
function TooltipInner(props: TooltipInnerProps) {
  const {
    id,
    disabled,
    handleHoverChange,
    state,
    targetRef,
    triggerRef,
    className,
    variant,
    rounded,
    size,
    maxWidth,
    children,
    testId,
    overlayPositionProps,
  } = props

  const {
    containerPadding = 0,
    offset = DEFAULT_OFFSET,
    crossOffset = 0,
    placement = 'bottom',
  } = overlayPositionProps

  const popoverRef = React.useRef<HTMLDivElement>(null)

  const { hoverProps: tooltipHoverProps } = aria.useHover({
    isDisabled: disabled,
    onHoverChange: handleHoverChange,
  })

  const { tooltipProps } = aria.useTooltipTrigger({ isDisabled: disabled }, state, targetRef)

  // eslint-disable-next-line @typescript-eslint/unbound-method
  const { overlayProps, updatePosition } = aria.useOverlayPosition({
    isOpen: state.isOpen,
    overlayRef: popoverRef,
    targetRef: triggerRef,
    offset,
    crossOffset,
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

  return createTooltipElement()
}

const DISPLAY_STRATEGIES: Record<DisplayStrategy, (target: HTMLElement) => boolean> = {
  always: () => true,
  whenOverflowing: (target) =>
    target.scrollWidth > target.clientWidth || target.scrollHeight > target.clientHeight,
}
