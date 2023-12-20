/** @file A wrapper around {@link router.useNavigate} that goes into offline mode when
 * offline. */
import * as router from 'react-router'

import * as app from '@/app'
import * as auth from '@/providers/auth'

// ===================
// === useNavigate ===
// ===================

/** A wrapper around {@link router.useNavigate} that goes into offline mode when
 * offline. */
export function useNavigate() {
    const { goOffline } = auth.useAuth()
    // This function is a wrapper around `router.useNavigate`. It shouldbe the only place where
    // `router.useNavigate` is used.
    // eslint-disable-next-line no-restricted-properties
    const originalNavigate = router.useNavigate()

    const navigate: router.NavigateFunction = (...args: [unknown, unknown?]) => {
        const isOnline = navigator.onLine
        if (!isOnline) {
            void goOffline()
            originalNavigate(app.DASHBOARD_PATH)
        } else {
            // This is safe, because the arguments are being passed through transparently.
            // eslint-disable-next-line no-restricted-syntax
            originalNavigate(...(args as [never, never?]))
        }
    }

    return navigate
}
