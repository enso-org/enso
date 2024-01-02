/** @file Page to enter offlin mode and redirect to dashboard. */
import * as React from 'react'

import * as appUtils from '#/appUtils'
import * as hooks from '#/hooks'
import * as authProvider from '#/providers/AuthProvider'

// ========================
// === EnterOfflineMode ===
// ========================

/** An empty component redirecting users based on the backend response to user registration. */
export default function EnterOfflineMode() {
    const { goOffline } = authProvider.useAuth()
    const navigate = hooks.useNavigate()

    React.useEffect(() => {
        void (async () => {
            await goOffline(false)
            window.setTimeout(() => {
                navigate(appUtils.DASHBOARD_PATH)
            }, 0)
        })()
        // This MUST only run once. This is fine because the above function *always* `navigate`s
        // away.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])

    return <></>
}
