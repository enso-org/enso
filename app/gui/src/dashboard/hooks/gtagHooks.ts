/** @file A hook that makes `gtag.event()` a no-op if the user is offline. */
import * as React from 'react'

import * as gtag from 'enso-common/src/gtag'

// ====================
// === useGtagEvent ===
// ====================

/**
 * A hook that returns a no-op if the user is offline, otherwise it returns
 * a transparent wrapper around `gtag.event`.
 */
export function useGtagEvent() {
  return React.useCallback((name: string, params?: object) => {
    gtag.event(name, params)
  }, [])
}

// =============================
// === gtagOpenCloseCallback ===
// =============================

/**
 * Send an event indicating that something has been opened, and return a cleanup function
 * sending an event indicating that it has been closed.
 *
 * Also sends the close event when the window is unloaded.
 */
export function gtagOpenCloseCallback(
  gtagEvent: ReturnType<typeof useGtagEvent>,
  openEvent: string,
  closeEvent: string,
) {
  gtagEvent(openEvent)

  const onBeforeUnload = () => {
    gtagEvent(closeEvent)
  }
  window.addEventListener('beforeunload', onBeforeUnload)

  return () => {
    window.removeEventListener('beforeunload', onBeforeUnload)
    gtagEvent(closeEvent)
  }
}
