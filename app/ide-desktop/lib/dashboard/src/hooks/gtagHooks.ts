/** @file A hook that makes `gtag.event()` a no-op if the user is offline. */
import * as React from 'react'

import * as gtag from 'enso-common/src/gtag'

import * as authProvider from '#/providers/AuthProvider'

// ====================
// === useGtagEvent ===
// ====================

/** A hook that returns a no-op if the user is offline, otherwise it returns
 * a transparent wrapper around `gtag.event`. */
export function useGtagEvent() {
  const { type: sessionType } = authProvider.useNonPartialUserSession()
  return React.useCallback(
    (name: string, params?: object) => {
      if (sessionType !== authProvider.UserSessionType.offline) {
        gtag.event(name, params)
      }
    },
    [sessionType]
  )
}

// =============================
// === gtagOpenCloseCallback ===
// =============================

/** Send an event indicating that something has been opened, and return a cleanup function
 * sending an event indicating that it has been closed.
 *
 * Also sends the close event when the window is unloaded. */
export function gtagOpenCloseCallback(
  gtagEventRef: React.MutableRefObject<ReturnType<typeof useGtagEvent>>,
  openEvent: string,
  closeEvent: string
) {
  const gtagEventCurrent = gtagEventRef.current
  gtagEventCurrent(openEvent)
  const onBeforeUnload = () => {
    gtagEventCurrent(closeEvent)
  }
  window.addEventListener('beforeunload', onBeforeUnload)
  return () => {
    window.removeEventListener('beforeunload', onBeforeUnload)
    gtagEventCurrent(closeEvent)
  }
}
