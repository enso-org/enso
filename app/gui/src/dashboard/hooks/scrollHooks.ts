/** @file Execute a function on scroll. */
import * as React from 'react'

import useOnScroll from '#/hooks/useOnScroll'

// ====================================
// === useStickyTableHeaderOnScroll ===
// ====================================

/** Options for the {@link useStickyTableHeaderOnScroll} hook. */
interface UseStickyTableHeaderOnScrollOptions {
  readonly trackShadowClass?: boolean
}

/**
 * Properly clip the table body to avoid the table header on scroll.
 * This is required to prevent the table body from overlapping the table header,
 * because the table header is transparent.
 *
 * NOTE: The returned event handler should be attached to the scroll container
 * (the closest ancestor element with `overflow-y-auto`).
 * @param rootRef - a {@link React.useRef} to the scroll container
 * @param bodyRef - a {@link React.useRef} to the `tbody` element that needs to be clipped.
 */
export function useStickyTableHeaderOnScroll(
  rootRef: React.MutableRefObject<HTMLDivElement | null>,
  bodyRef: React.RefObject<HTMLTableSectionElement>,
  options: UseStickyTableHeaderOnScrollOptions = {},
) {
  const { trackShadowClass = false } = options
  const trackShadowClassRef = React.useRef(trackShadowClass)
  trackShadowClassRef.current = trackShadowClass
  const [shadowClassName, setShadowClass] = React.useState('')
  const onScroll = useOnScroll(() => {
    if (rootRef.current != null && bodyRef.current != null) {
      bodyRef.current.style.clipPath = `inset(${rootRef.current.scrollTop}px 0 0 0)`
      if (trackShadowClassRef.current) {
        const isAtTop = rootRef.current.scrollTop === 0
        const isAtBottom =
          rootRef.current.scrollTop + rootRef.current.clientHeight >= rootRef.current.scrollHeight
        const newShadowClass =
          isAtTop ?
            isAtBottom ? ''
            : 'shadow-inset-b-lg'
          : isAtBottom ? 'shadow-inset-t-lg'
          : 'shadow-inset-v-lg'
        setShadowClass(newShadowClass)
      }
    }
  }, [bodyRef, rootRef])
  return { onScroll, shadowClassName }
}
