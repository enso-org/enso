/** @file Hooks for. */
import * as React from 'react'

// =================
// === Constants ===
// =================

/** See {@link AutoScrollOptions.threshold}. */
const AUTOSCROLL_THRESHOLD_PX = 50
/** See {@link AutoScrollOptions.speed}. */
const AUTOSCROLL_SPEED = 100
/** See {@link AutoScrollOptions.falloff}. */
const AUTOSCROLL_FALLOFF = 10

// ===========================
// === AutoScrollDirection ===
// ===========================

/** The direction(s) in which autoscroll should happen. */
export type AutoScrollDirection = 'both' | 'horizontal' | 'none' | 'vertical'

// =========================
// === AutoScrollOptions ===
// =========================

/** Options for {@link useAutoScroll}. */
export interface AutoScrollOptions {
  readonly direction?: AutoScrollDirection
  readonly insets?: AutoScrollInsets
  /**
   * If the drag pointer is less than this distance away from the top or bottom of the
   * scroll container, then the scroll container automatically scrolls upwards if the cursor is near
   * the top of the scroll container, or downwards if the cursor is near the bottom.
   */
  readonly threshold?: number
  /** An arbitrary constant that controls the speed of autoscroll. */
  readonly speed?: number
  /** The autoscroll speed is `speed / (distance + falloff)`. */
  readonly falloff?: number
}

// =========================
// === AutoScrollOffsets ===
// =========================

/**
 * The amount of space on which side on which scrolling should have no effect.
 * The container is treated as this much smaller, meaning that autoscroll speed will be calculated
 * as though the pointer is that much closer to an edge.
 */
interface AutoScrollInsets {
  readonly top?: number
  readonly bottom?: number
  readonly left?: number
  readonly right?: number
}

// =====================
// === useAutoScroll ===
// =====================

/** Scroll a container when the mouse is near the edges of a container. */
export function useAutoScroll(
  scrollContainerRef: React.MutableRefObject<HTMLDivElement | null>,
  options: AutoScrollOptions = {},
) {
  const isScrolling = React.useRef(false)
  const animationFrameHandle = React.useRef(0)
  const pointerX = React.useRef(0)
  const pointerY = React.useRef(0)
  const optionsRef = React.useRef(options)
  optionsRef.current = options

  const onMouseEvent = React.useCallback((event: MouseEvent | React.MouseEvent) => {
    pointerX.current = event.clientX
    pointerY.current = event.clientY
  }, [])

  const onAnimationFrame = React.useCallback(() => {
    const scrollContainer = scrollContainerRef.current
    if (isScrolling.current && scrollContainer) {
      const {
        direction = 'vertical',
        insets = {},
        threshold = AUTOSCROLL_THRESHOLD_PX,
        speed = AUTOSCROLL_SPEED,
        falloff = AUTOSCROLL_FALLOFF,
      } = optionsRef.current
      const {
        top: insetTop = 0,
        bottom: insetBottom = 0,
        left: insetLeft = 0,
        right: insetRight = 0,
      } = insets
      const rect = scrollContainer.getBoundingClientRect()
      if (direction === 'vertical' || direction === 'both') {
        if (scrollContainer.scrollTop > 0) {
          const distanceToTop = Math.max(0, pointerY.current - rect.top - insetTop)
          if (distanceToTop < threshold) {
            scrollContainer.scrollTop -= Math.floor(speed / (distanceToTop + falloff))
          }
        }
        if (scrollContainer.scrollTop + rect.height < scrollContainer.scrollHeight) {
          const distanceToBottom = Math.max(0, rect.bottom - pointerY.current - insetBottom)
          if (distanceToBottom < threshold) {
            scrollContainer.scrollTop += Math.floor(speed / (distanceToBottom + falloff))
          }
        }
      }
      if (direction === 'horizontal' || direction === 'both') {
        if (scrollContainer.scrollLeft > 0) {
          const distanceToLeft = Math.max(0, pointerX.current - rect.top - insetLeft)
          if (distanceToLeft < threshold) {
            scrollContainer.scrollLeft -= Math.floor(speed / (distanceToLeft + falloff))
          }
        }
        if (scrollContainer.scrollLeft + rect.width < scrollContainer.scrollWidth) {
          const distanceToRight = Math.max(0, rect.right - pointerX.current - insetRight)
          if (distanceToRight < threshold) {
            scrollContainer.scrollLeft += Math.floor(speed / (distanceToRight + falloff))
          }
        }
      }
      animationFrameHandle.current = requestAnimationFrame(onAnimationFrame)
    }
  }, [scrollContainerRef])

  const startAutoScroll = React.useCallback(() => {
    if (!isScrolling.current) {
      isScrolling.current = true
      animationFrameHandle.current = requestAnimationFrame(onAnimationFrame)
    }
  }, [onAnimationFrame])

  const endAutoScroll = React.useCallback(() => {
    isScrolling.current = false
    window.cancelAnimationFrame(animationFrameHandle.current)
  }, [])

  return { startAutoScroll, endAutoScroll, onMouseEvent }
}
