/** @file A hook that makes `gtag.event()` a no-op if the user is offline. */
import * as React from 'react'

import * as gtag from 'enso-common/src/gtag'

import * as authProvider from '#/providers/AuthProvider'

// ===================
// === useGtag ===
// ===================

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
