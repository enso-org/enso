/** @file A selection brush to indicate the area being selected by the mouse drag action. */
import Portal from '#/components/Portal'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useEventListener } from '#/hooks/eventListenerHooks'
import { useRafThrottle } from '#/hooks/throttleHooks'
import { noop } from '#/utilities/functions'
import type { Coordinate2D, DetailedRectangle, Rectangle } from '#/utilities/geometry'
import { getDetailedRectangle, getDetailedRectangleFromRectangle } from '#/utilities/geometry'
import { findScrollContainers, type HTMLOrSVGElement } from '#/utilities/scrollContainers'
import { useEffect, useRef, useState, type RefObject } from 'react'

/**
 * Defines the minimal distance that the mouse must move before
 * we consider that user has started a selection.
 */
const DEAD_ZONE_SIZE = 24

/** Parameters for the `onDrag` callback. */
export interface OnDragParams {
  readonly diff: Coordinate2D
  readonly start: Coordinate2D
  readonly current: Coordinate2D
  readonly rectangle: DetailedRectangle
  readonly event: PointerEvent
}

/** Props for a {@link SelectionBrush}. */
export interface SelectionBrushV2Props {
  readonly onDragStart?: (event: PointerEvent) => void
  readonly onDrag?: (params: OnDragParams) => void
  readonly onDragEnd?: (event: PointerEvent) => void
  readonly onDragCancel?: () => void
  readonly targetRef: RefObject<HTMLElement>
  readonly isDisabled?: boolean
  readonly preventDrag?: (event: PointerEvent) => boolean
}

/** A selection brush to indicate the area being selected by the mouse drag action. */
export function SelectionBrush(props: SelectionBrushV2Props) {
  const {
    targetRef,
    preventDrag = () => false,
    onDragStart = noop,
    onDrag = noop,
    onDragEnd = noop,
    onDragCancel = noop,
    isDisabled = false,
  } = props

  const [isDragging, setIsDragging] = useState(false)

  /**
   * Whether the pointer has passed the dead zone,
   * and user started dragging.
   * This is used to prevent the selection brush from being
   * invoked when user clicks on the element with tiny movement.
   */
  const hasPassedDeadZoneRef = useRef<boolean>(false)

  const startPositionRef = useRef<Coordinate2D | null>(null)
  const previousPositionRef = useRef<Coordinate2D | null>(null)
  const currentPositionRef = useRef<Coordinate2D>({ left: 0, top: 0 })
  const currentRectangleRef = useRef<DetailedRectangle | null>(null)

  const scrollContainersLastScrollPositionRef = useRef<
    Map<HTMLOrSVGElement, { left: number; top: number }>
  >(new Map())

  const [box, setBox] = useState<{
    left: number
    top: number
    width: number
    height: number
  } | null>(null)

  const preventDragStableCallback = useEventCallback(preventDrag)
  const onDragStartStableCallback = useEventCallback(onDragStart)
  const onDragStableCallback = useEventCallback(onDrag)
  const onDragEndStableCallback = useEventCallback(onDragEnd)
  const onDragCancelStableCallback = useEventCallback(onDragCancel)

  const { scheduleRAF, cancelRAF } = useRafThrottle()
  const { scheduleRAF: scheduleRAFScroll, cancelRAF: cancelRAFScroll } = useRafThrottle()

  const startDragging = useEventCallback(() => {
    setIsDragging(true)
    hasPassedDeadZoneRef.current = true
    document.documentElement.style.userSelect = 'none'
  })

  const applyBrushPosition = useEventCallback((rectangle: DetailedRectangle) => {
    setBox({
      left: rectangle.left,
      top: rectangle.top,
      width: rectangle.width,
      height: rectangle.height,
    })
  })

  const resetState = useEventCallback(() => {
    setIsDragging(false)
    cancelRAF()
    cancelRAFScroll()
    hasPassedDeadZoneRef.current = false
    startPositionRef.current = null
    currentPositionRef.current = { left: 0, top: 0 }
    previousPositionRef.current = null
    currentRectangleRef.current = null
    setBox(null)
    document.documentElement.style.userSelect = ''
  })

  const updateBrush = useEventCallback((rectangle: DetailedRectangle) => {
    if (!isDragging) {
      startDragging()
    }

    applyBrushPosition(rectangle)
  })

  useEffect(() => {
    if (!isDragging) return

    const scrollContainers = findScrollContainers(targetRef.current)

    const callback = (event: Event) => {
      const start = startPositionRef.current
      const current = currentPositionRef.current
      const currentRectangle = currentRectangleRef.current

      scheduleRAFScroll(() => {
        if (!(event.target instanceof HTMLElement) && !(event.target instanceof SVGElement)) {
          return
        }

        const target = event.target

        if (!scrollContainers.includes(target)) {
          return
        }

        // If we don't have a start position or a current rectangle, we can't update the brush.
        // and thus we ignore the event.
        if (currentRectangle == null || start == null) {
          return
        }

        const nextLeft = target.scrollLeft
        const nextTop = target.scrollTop

        const lastX = scrollContainersLastScrollPositionRef.current.get(target)?.left ?? 0
        const lastY = scrollContainersLastScrollPositionRef.current.get(target)?.top ?? 0

        const diffX = nextLeft - lastX
        const diffY = nextTop - lastY

        if (diffX === 0 && diffY === 0) {
          return
        }

        // Calculate the direction of the scroll.
        // This is used to understand, where we should extend the rectangle.
        const direction = getDirectionFromScrollDiff(diffX, diffY)

        // Calculate the next rectangle based on the scroll direction.
        // New rectangle extends by the scroll distance.
        const nextRectangle = calculateRectangleFromScrollDirection(currentRectangle, direction, {
          left: diffX,
          top: diffY,
        })

        const detailedRectangle = getDetailedRectangleFromRectangle(nextRectangle)

        // Since we scroll the container, we need to update the start position
        // (the position of the cursor when the drag started)
        // to make it on sync with apropriate corner of the rectangle.
        startPositionRef.current = calculateNewStartPositionFromScrollDirection(
          start,
          current,
          nextRectangle,
        )

        currentRectangleRef.current = detailedRectangle

        updateBrush(detailedRectangle)

        scrollContainersLastScrollPositionRef.current.set(target, { left: nextLeft, top: nextTop })
      })
    }

    scrollContainers.forEach((container) => {
      scrollContainersLastScrollPositionRef.current.set(container, {
        left: container.scrollLeft,
        top: container.scrollTop,
      })

      container.addEventListener('scroll', callback, { passive: true, capture: true })
    })

    const lastScrollContainersLastScrollPositionRef = scrollContainersLastScrollPositionRef.current

    return () => {
      scrollContainers.forEach((container) => {
        container.removeEventListener('scroll', callback)
        lastScrollContainersLastScrollPositionRef.delete(container)
      })
    }
  }, [onDragStableCallback, targetRef, updateBrush, isDragging, scheduleRAFScroll])

  useEventListener(
    'pointerdown',
    (event) => {
      resetState()

      if (preventDragStableCallback(event)) {
        return
      }

      startPositionRef.current = { left: event.pageX, top: event.pageY }
      previousPositionRef.current = startPositionRef.current
      currentPositionRef.current = startPositionRef.current

      currentRectangleRef.current = getDetailedRectangle(
        startPositionRef.current,
        currentPositionRef.current,
      )

      onDragStartStableCallback(event)
    },
    targetRef,
    { isDisabled, capture: true, passive: true },
  )

  useEventListener(
    'pointermove',
    (event) => {
      const start = startPositionRef.current
      const current = currentPositionRef.current
      const currentRectangle = currentRectangleRef.current

      const previous = previousPositionRef.current ?? start

      // Pointer events have higher priority than scroll events.
      // Cancel the scroll RAF to prevent the scroll callback from being called.
      cancelRAFScroll()

      scheduleRAF(() => {
        if (start == null || currentRectangle == null || previous == null) {
          return
        }

        currentPositionRef.current = { left: event.pageX, top: event.pageY }

        // Check if the user has passed the dead zone.
        // Dead zone shall be passed only once.
        if (hasPassedDeadZoneRef.current === false) {
          hasPassedDeadZoneRef.current = !isInDeadZone(start, current, DEAD_ZONE_SIZE)
        }

        if (hasPassedDeadZoneRef.current) {
          const diff: Coordinate2D = {
            left: current.left - previous.left,
            top: current.top - previous.top,
          }

          const detailedRectangle = getDetailedRectangle(start, current)

          // Capture the pointer events to lock the whole selection to the target.
          // and don't invoke hover events. when the user is dragging.
          targetRef.current?.setPointerCapture(event.pointerId)
          currentRectangleRef.current = detailedRectangle
          previousPositionRef.current = { left: current.left, top: current.top }

          updateBrush(detailedRectangle)

          onDragStableCallback({
            diff,
            start,
            current,
            rectangle: detailedRectangle,
            event,
          })
        }
      })
    },
    document,
    { isDisabled, capture: true, passive: true },
  )

  useEventListener(
    'pointerup',
    (event) => {
      resetState()
      targetRef.current?.releasePointerCapture(event.pointerId)
      if (isDragging) {
        onDragEndStableCallback(event)
      }
    },
    document,
    { isDisabled, capture: true, passive: true },
  )

  useEventListener(
    'pointercancel',
    (event) => {
      resetState()
      targetRef.current?.releasePointerCapture(event.pointerId)
      if (isDragging) {
        onDragEndStableCallback(event)
        onDragCancelStableCallback()
      }
    },
    document,
    { isDisabled, capture: true, passive: true },
  )

  const brushStyle =
    box == null ?
      {}
    : {
        left: `${box.left}px`,
        top: `${box.top}px`,
        width: `${box.width}px`,
        height: `${box.height}px`,
      }

  return (
    <Portal>
      <div
        data-testid="selection-brush"
        data-is-dragging={isDragging}
        className="pointer-events-none absolute before:absolute before:-inset-1 before:rounded-xl before:border-2 before:border-primary/5 before:bg-primary/5"
        style={{ ...brushStyle, opacity: isDragging ? 1 : 0 }}
      />
    </Portal>
  )
}

/**
 * Whether the current position is in the dead zone.
 * @param initialPosition - The initial position.
 * @param currentPosition - The current position.
 * @param deadZoneSize - The size of the dead zone.
 * @returns Whether the current position is in the dead zone.
 */
function isInDeadZone(
  initialPosition: Coordinate2D,
  currentPosition: Coordinate2D,
  deadZoneSize: number,
) {
  const horizontalDistance = Math.abs(initialPosition.left - currentPosition.left)
  const verticalDistance = Math.abs(initialPosition.top - currentPosition.top)

  return horizontalDistance < deadZoneSize && verticalDistance < deadZoneSize
}

/**
 *
 */
type Direction =
  | 'bottom-left'
  | 'bottom-right'
  | 'bottom'
  | 'left'
  | 'none'
  | 'right'
  | 'top-left'
  | 'top-right'
  | 'top'

/**
 * Get the direction from the scroll difference.
 * @param diffX - The difference in the x direction.
 * @param diffY - The difference in the y direction.
 * @returns The direction.
 */
function getDirectionFromScrollDiff(diffX: number, diffY: number): Direction {
  if (diffX > 0 && diffY === 0) return 'right'
  if (diffX < 0 && diffY === 0) return 'left'
  if (diffX === 0 && diffY > 0) return 'bottom'
  if (diffX === 0 && diffY < 0) return 'top'
  if (diffX > 0 && diffY > 0) return 'bottom-right'
  if (diffX < 0 && diffY > 0) return 'bottom-left'
  if (diffX < 0 && diffY < 0) return 'top-left'
  if (diffX > 0 && diffY < 0) return 'top-right'
  return 'none'
}

/**
 * Calculate new rectangle from the scroll direction.
 * @param start - The start rectangle.
 * @param direction - The direction.
 * @param diff - The difference.
 * @returns The rectangle.
 */
function calculateRectangleFromScrollDirection(
  start: Rectangle,
  direction: Direction,
  diff: Coordinate2D,
): Rectangle {
  switch (direction) {
    case 'left':
      return { ...start, right: start.right - diff.left }
    case 'right':
      return { ...start, left: start.left + diff.left }
    case 'top':
      return { ...start, bottom: start.bottom - diff.top }
    case 'bottom':
      return { ...start, top: start.top - diff.top }
    case 'bottom-left':
      return { ...start, right: start.right + diff.left, top: start.top - diff.top }
    case 'bottom-right':
      return { ...start, left: start.left - diff.left, top: start.top - diff.top }
    case 'top-left':
      return { ...start, right: start.right + diff.left, bottom: start.bottom - diff.top }
    case 'top-right':
      return { ...start, bottom: start.bottom - diff.top, left: start.left - diff.left }
    case 'none':
    default:
      return start
  }
}

/**
 * Calculate new start position from the scroll direction.
 * @param start - The start position of the cursor.
 * @param current - The current position of the cursor.
 * @param rectangle - The rectangle.
 * @returns The new start position.
 */
function calculateNewStartPositionFromScrollDirection(
  start: Coordinate2D,
  current: Coordinate2D,
  rectangle: Rectangle,
) {
  const cursorPositionInRectangle = (() => {
    if (start.left < current.left && start.top < current.top) return 'bottom-right'
    if (start.left > current.left && start.top > current.top) return 'top-left'
    if (start.left < current.left && start.top > current.top) return 'bottom-left'
    if (start.left > current.left && start.top < current.top) return 'top-right'
    return 'none'
  })()

  switch (cursorPositionInRectangle) {
    case 'top-left':
      return { top: rectangle.top, left: rectangle.left }
    case 'bottom-right':
      return { top: rectangle.bottom, left: rectangle.right }
    case 'top-right':
      return { top: rectangle.top, left: rectangle.right }
    case 'bottom-left':
      return { top: rectangle.bottom, left: rectangle.left }
    case 'none':
    default:
      return start
  }
}
