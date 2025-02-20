/** @file A selection brush to indicate the area being selected by the mouse drag action. */
import * as React from 'react'

import Portal from '#/components/Portal'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useEventListener } from '#/hooks/eventListenerHooks'
import { useRafThrottle } from '#/hooks/throttleHooks'
import type * as geometry from '#/utilities/geometry'
import { getDetailedRectangle, getDetailedRectangleFromRectangle } from '#/utilities/geometry'
import { findScrollContainers, type HTMLOrSVGElement } from '#/utilities/scrollContainers'
import { motion, useMotionValue } from 'framer-motion'

// =================
// === Constants ===
// =================

/**
 * Defines the minimal distance that the mouse must move before
 * we consider that user has started a selection.
 */
const DEAD_ZONE_SIZE = 24

// eslint-disable-next-line no-restricted-syntax
const noop = () => {}

// ======================
// === SelectionBrush ===
// ======================

/**
 * Parameters for the onDrag callback.
 */
export interface OnDragParams {
  readonly diff: geometry.Coordinate2D
  readonly start: geometry.Coordinate2D
  readonly current: geometry.Coordinate2D
  readonly rectangle: geometry.DetailedRectangle
  readonly event: PointerEvent
}

/**
 * Props for a {@link SelectionBrush}.
 */
export interface SelectionBrushV2Props {
  readonly onDragStart?: (event: PointerEvent) => void
  readonly onDrag?: (params: OnDragParams) => void
  readonly onDragEnd?: (event: PointerEvent) => void
  readonly onDragCancel?: () => void

  readonly targetRef: React.RefObject<HTMLElement>
  readonly isDisabled?: boolean
  readonly preventDrag?: (event: PointerEvent) => boolean
}

/**
 * The direction of the Drag/Scroll.
 */
const enum DIRECTION {
  /**
   * •
   */
  NONE = 0,
  /**
   * ⬅️
   */
  LEFT = 1,
  /**
   * ➡️
   */
  RIGHT = 2,
  /**
   * ⬆️
   */
  TOP = 3,
  /**
   * ⬇️
   */
  BOTTOM = 4,
  /**
   * ↙️
   */
  BOTTOM_LEFT = 5,
  /**
   * ↘️
   */
  BOTTOM_RIGHT = 6,
  /**
   * ↖️
   */
  TOP_LEFT = 7,
  /**
   * ↗️
   */
  TOP_RIGHT = 8,
}

/**
 * A selection brush to indicate the area being selected by the mouse drag action.
 */
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

  const [isDragging, setIsDragging] = React.useState(false)

  /**
   * Whether the pointer has passed the dead zone,
   * and user started dragging.
   * This is used to prevent the selection brush from being
   * invoked when user clicks on the element with tiny movement.
   */
  const hasPassedDeadZoneRef = React.useRef<boolean>(false)

  const startPositionRef = React.useRef<geometry.Coordinate2D | null>(null)
  const previousPositionRef = React.useRef<geometry.Coordinate2D | null>(null)
  const currentPositionRef = React.useRef<geometry.Coordinate2D>({ left: 0, top: 0 })
  const currentRectangleRef = React.useRef<geometry.DetailedRectangle | null>(null)

  const scrollContainersLastScrollPositionRef = React.useRef<
    Map<HTMLOrSVGElement, { left: number; top: number }>
  >(new Map())

  const left = useMotionValue<geometry.DetailedRectangle['left'] | null>(null)
  const top = useMotionValue<geometry.DetailedRectangle['top'] | null>(null)
  const width = useMotionValue<geometry.DetailedRectangle['width'] | null>(null)
  const height = useMotionValue<geometry.DetailedRectangle['height'] | null>(null)

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
  })

  const applyBrushPosition = useEventCallback((rectangle: geometry.DetailedRectangle) => {
    left.set(rectangle.left)
    top.set(rectangle.top)
    width.set(rectangle.width)
    height.set(rectangle.height)
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
    left.set(null)
    top.set(null)
    width.set(null)
    height.set(null)
  })

  const updateBrush = useEventCallback((rectangle: geometry.DetailedRectangle) => {
    if (!isDragging) {
      startDragging()
    }

    applyBrushPosition(rectangle)
  })

  React.useEffect(() => {
    if (!isDragging) {
      return
    }

    const scrollContainers = findScrollContainers(targetRef.current)

    const callback = (event: Event) => {
      const start = startPositionRef.current
      const current = currentPositionRef.current
      const currentRectangle = currentRectangleRef.current

      scheduleRAFScroll(() => {
        // eslint-disable-next-line no-restricted-syntax
        const target = event.target as unknown as HTMLOrSVGElement

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
          const diff: geometry.Coordinate2D = {
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

  return (
    <Portal>
      <motion.div
        data-testid="selection-brush"
        data-is-dragging={isDragging}
        className="pointer-events-none absolute before:absolute before:-inset-1 before:rounded-xl before:border-2 before:border-primary/5 before:bg-primary/5"
        style={{ left, top, width, height, opacity: isDragging ? 1 : 0 }}
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
  initialPosition: geometry.Coordinate2D,
  currentPosition: geometry.Coordinate2D,
  deadZoneSize: number,
) {
  const horizontalDistance = Math.abs(initialPosition.left - currentPosition.left)
  const verticalDistance = Math.abs(initialPosition.top - currentPosition.top)

  return horizontalDistance < deadZoneSize && verticalDistance < deadZoneSize
}

/**
 * Get the direction from the scroll difference.
 * @param diffX - The difference in the x direction.
 * @param diffY - The difference in the y direction.
 * @returns The direction.
 */
function getDirectionFromScrollDiff(diffX: number, diffY: number): DIRECTION {
  if (diffX > 0 && diffY === 0) {
    return DIRECTION.RIGHT
  }

  if (diffX < 0 && diffY === 0) {
    return DIRECTION.LEFT
  }

  if (diffX === 0 && diffY > 0) {
    return DIRECTION.BOTTOM
  }

  if (diffX === 0 && diffY < 0) {
    return DIRECTION.TOP
  }

  if (diffX > 0 && diffY > 0) {
    return DIRECTION.BOTTOM_RIGHT
  }

  if (diffX < 0 && diffY > 0) {
    return DIRECTION.BOTTOM_LEFT
  }

  if (diffX < 0 && diffY < 0) {
    return DIRECTION.TOP_LEFT
  }

  if (diffX > 0 && diffY < 0) {
    return DIRECTION.TOP_RIGHT
  }

  return DIRECTION.NONE
}

/**
 * Calculate new rectangle from the scroll direction.
 * @param start - The start rectangle.
 * @param direction - The direction.
 * @param diff - The difference.
 * @returns The rectangle.
 */
function calculateRectangleFromScrollDirection(
  start: geometry.Rectangle,
  direction: DIRECTION,
  diff: geometry.Coordinate2D,
): geometry.Rectangle {
  switch (direction) {
    case DIRECTION.LEFT:
      return {
        ...start,
        right: start.right - diff.left,
      }
    case DIRECTION.RIGHT:
      return {
        ...start,
        left: start.left + diff.left,
      }
    case DIRECTION.TOP:
      return {
        ...start,
        bottom: start.bottom - diff.top,
      }
    case DIRECTION.BOTTOM:
      return {
        ...start,
        top: start.top - diff.top,
      }
    case DIRECTION.BOTTOM_LEFT:
      return {
        ...start,
        right: start.right + diff.left,
        top: start.top - diff.top,
      }
    case DIRECTION.BOTTOM_RIGHT:
      return {
        ...start,
        left: start.left - diff.left,
        top: start.top - diff.top,
      }
    case DIRECTION.TOP_LEFT:
      return {
        ...start,
        right: start.right + diff.left,
        bottom: start.bottom - diff.top,
      }
    case DIRECTION.TOP_RIGHT:
      return {
        ...start,
        bottom: start.bottom - diff.top,
        left: start.left - diff.left,
      }
    case DIRECTION.NONE:
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
  start: geometry.Coordinate2D,
  current: geometry.Coordinate2D,
  rectangle: geometry.Rectangle,
) {
  const cursorPositionInRectangle = (() => {
    if (start.left < current.left && start.top < current.top) {
      return DIRECTION.BOTTOM_RIGHT
    }

    if (start.left > current.left && start.top > current.top) {
      return DIRECTION.TOP_LEFT
    }

    if (start.left < current.left && start.top > current.top) {
      return DIRECTION.BOTTOM_LEFT
    }

    if (start.left > current.left && start.top < current.top) {
      return DIRECTION.TOP_RIGHT
    }

    return DIRECTION.NONE
  })()

  switch (cursorPositionInRectangle) {
    case DIRECTION.TOP_LEFT:
      return {
        top: rectangle.top,
        left: rectangle.left,
      }
    case DIRECTION.BOTTOM_RIGHT:
      return {
        top: rectangle.bottom,
        left: rectangle.right,
      }
    case DIRECTION.TOP_RIGHT:
      return {
        top: rectangle.top,
        left: rectangle.right,
      }
    case DIRECTION.BOTTOM_LEFT:
      return {
        top: rectangle.bottom,
        left: rectangle.left,
      }
    case DIRECTION.NONE:
    default:
      return start
  }
}
