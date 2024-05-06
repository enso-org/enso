/** @file Execute a function on scroll. */
import * as React from 'react'

// ===================
// === useOnScroll ===
// ===================

/** Execute a function on scroll. */
export function useOnScroll(callback: () => void, dependencies: React.DependencyList = []) {
  const callbackRef = React.useRef(callback)
  callbackRef.current = callback
  const updateClipPathRef = React.useRef(() => {})

  const onScroll = React.useMemo(() => {
    let isClipPathUpdateQueued = false
    const updateClipPath = () => {
      isClipPathUpdateQueued = false
      callbackRef.current()
    }
    updateClipPathRef.current = updateClipPath
    updateClipPath()
    return () => {
      if (!isClipPathUpdateQueued) {
        isClipPathUpdateQueued = true
        requestAnimationFrame(updateClipPath)
      }
    }
  }, [])

  React.useLayoutEffect(() => {
    updateClipPathRef.current()
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, dependencies)

  React.useEffect(() => {
    window.addEventListener('resize', onScroll)
    return () => {
      window.removeEventListener('resize', onScroll)
    }
  }, [onScroll])

  return onScroll
}

// ====================================
// === useStickyTableHeaderOnScroll ===
// ====================================

/** Properly clip the table body to avoid the table header on scroll.
 * This is required to prevent the table body from overlapping the table header,
 * because the table header is transparent.
 *
 * NOTE: The returned event handler should be attached to the scroll container
 * (the closest ancestor element with `overflow-y-auto`).
 * @param rootRef - a {@link React.useRef} to the scroll container
 * @param bodyRef - a {@link React.useRef} to the `tbody` element that needs to be clipped. */
export function useStickyTableHeaderOnScroll(
  rootRef: React.MutableRefObject<HTMLDivElement | null>,
  bodyRef: React.RefObject<HTMLTableSectionElement>
) {
  return useOnScroll(() => {
    if (rootRef.current != null && bodyRef.current != null) {
      bodyRef.current.style.clipPath = `inset(${rootRef.current.scrollTop}px 0 0 0)`
    }
  })
}
