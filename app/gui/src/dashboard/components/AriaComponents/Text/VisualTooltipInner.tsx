import {
  AriaPositionProps,
  TooltipTriggerState,
  mergeProps,
  useHover,
  useOverlayPosition,
  useTooltipTrigger,
} from '#/components/aria'
import Portal from '#/components/Portal'
import { useRef } from 'react'
import { TOOLTIP_STYLES, TooltipProps } from '../Tooltip'

const DEFAULT_OVERLAY_OFFSET_PX = 6

/** Props for {@link TooltipInner}. */
export interface VisualTooltipInnerProps
  extends Pick<TooltipProps, 'maxWidth' | 'rounded' | 'size' | 'variant'> {
  readonly id: string
  readonly disabled: boolean
  readonly handleHoverChange: (isHovered: boolean) => void
  readonly state: TooltipTriggerState
  readonly targetRef: React.RefObject<HTMLElement>
  readonly triggerRef: React.RefObject<HTMLElement>
  readonly children: React.ReactNode
  readonly className?: string | undefined
  readonly testId?: string | undefined
  readonly overlayPositionProps: Pick<
    AriaPositionProps,
    'containerPadding' | 'crossOffset' | 'offset' | 'placement'
  >
}

/** The inner component of the tooltip. */
export function VisualTooltipInner(props: VisualTooltipInnerProps) {
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
    offset = DEFAULT_OVERLAY_OFFSET_PX,
    crossOffset = 0,
    placement = 'bottom',
  } = overlayPositionProps

  const popoverRef = useRef<HTMLDivElement>(null)

  const { hoverProps: tooltipHoverProps } = useHover({
    isDisabled: disabled,
    onHoverChange: handleHoverChange,
  })

  const { tooltipProps } = useTooltipTrigger({ isDisabled: disabled }, state, targetRef)

  // eslint-disable-next-line @typescript-eslint/unbound-method
  const { overlayProps, updatePosition } = useOverlayPosition({
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
        {...mergeProps<React.HTMLAttributes<HTMLDivElement>>()(
          overlayProps,
          tooltipProps,
          tooltipHoverProps,
          {
            id,
            className: TOOLTIP_STYLES({
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
