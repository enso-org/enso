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

  return onScroll
}
