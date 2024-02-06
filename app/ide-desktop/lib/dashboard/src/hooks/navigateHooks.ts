/** @file A wrapper around {@link router.useNavigate} that goes into offline mode when
 * offline. */
import * as React from 'react'

import * as router from 'react-router'

import * as appUtils from '#/appUtils'

import * as authProvider from '#/providers/AuthProvider'

// ===================
// === useNavigate ===
// ===================

/** A wrapper around {@link router.useNavigate} that goes into offline mode when
 * offline. */
export function useNavigate() {
  const { goOffline } = authProvider.useAuth()
  // This function is a wrapper around `router.useNavigate`. It shouldbe the only place where
  // `router.useNavigate` is used.
  // eslint-disable-next-line no-restricted-properties
  const originalNavigate = router.useNavigate()

  const navigate: router.NavigateFunction = React.useCallback(
    (...args: [unknown, unknown?]) => {
      const isOnline = navigator.onLine
      if (!isOnline) {
        void goOffline()
        originalNavigate(appUtils.DASHBOARD_PATH)
      } else {
        // This is safe, because the arguments are being passed through transparently.
        // eslint-disable-next-line no-restricted-syntax
        originalNavigate(...(args as [never, never?]))
      }
    },
    [/* should never change */ goOffline, /* should never change */ originalNavigate]
  )

  return navigate
}
