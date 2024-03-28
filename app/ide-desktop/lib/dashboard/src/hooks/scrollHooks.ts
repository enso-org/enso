/** @file Execute a function on scroll. */
import * as React from 'react'

/** Execute a function on scroll. */
export function useOnScroll(callback: () => void) {
  const callbackRef = React.useRef(callback)
  callbackRef.current = callback

  return React.useMemo(() => {
    let isClipPathUpdateQueued = false
    const updateClipPath = () => {
      isClipPathUpdateQueued = false
      callbackRef.current()
    }
    updateClipPath()
    return () => {
      if (!isClipPathUpdateQueued) {
        isClipPathUpdateQueued = true
        requestAnimationFrame(updateClipPath)
      }
    }
  }, [])
}
