/** @file Execute a function on scroll. */
import { useState, type MutableRefObject, type RefObject } from 'react'

import { useSyncRef } from '#/hooks/syncRefHooks'
import useOnScroll from '#/hooks/useOnScroll'

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
 * @param rootRef - a {@link useRef} to the scroll container
 * @param bodyRef - a {@link useRef} to the `tbody` element that needs to be clipped.
 */
export function useStickyTableHeaderOnScroll(
  rootRef: MutableRefObject<HTMLDivElement | null>,
  bodyRef: RefObject<HTMLTableSectionElement>,
  options: UseStickyTableHeaderOnScrollOptions = {},
) {
  const { trackShadowClass = false } = options
  const trackShadowClassRef = useSyncRef(trackShadowClass)
  const [shadowClassName, setShadowClass] = useState('')
  const onScroll = useOnScroll(() => {
    if (rootRef.current && bodyRef.current) {
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
  }, [bodyRef, rootRef, trackShadowClassRef])
  return { onScroll, shadowClassName }
}
