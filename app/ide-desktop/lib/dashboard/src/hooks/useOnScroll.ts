/** @file Execute a function on scroll. */
import * as React from 'react'

import * as eventCallbackHooks from '#/hooks/eventCallbackHooks'

// ===================
// === useOnScroll ===
// ===================

/** Execute a function on scroll. */
export default function useOnScroll(callback: () => void, dependencies: React.DependencyList) {
  const callbackTrampoline = eventCallbackHooks.useEventCallback(callback)
  const updateClipPathRef = React.useRef(() => {})

  const onScroll = React.useMemo(() => {
    let isClipPathUpdateQueued = false
    const updateClipPath = () => {
      isClipPathUpdateQueued = false
      callbackTrampoline()
    }
    updateClipPathRef.current = updateClipPath
    updateClipPath()
    return () => {
      if (!isClipPathUpdateQueued) {
        isClipPathUpdateQueued = true
        requestAnimationFrame(updateClipPath)
      }
    }
  }, [callbackTrampoline])

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
