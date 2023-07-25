/** @file Registration confirmation page for when a user clicks the confirmation link set to their
 * email address. */
import * as React from 'react'
import * as router from 'react-router-dom'
import * as toastify from 'react-toastify'

import * as app from '../../components/app'
import * as authModule from '../providers/auth'
import * as hooks from '../../hooks'
import * as loggerProvider from '../../providers/logger'

// =================
// === Constants ===
// =================

const REGISTRATION_QUERY_PARAMS = {
    verificationCode: 'verification_code',
    email: 'email',
} as const

// ============================
// === Confirm Registration ===
// ============================

/** An empty component redirecting users based on the backend response to user registration. */
function ConfirmRegistration() {
    const logger = loggerProvider.useLogger()
    const auth = authModule.useAuth()
    const location = router.useLocation()
    const navigate = hooks.useNavigate()

    const { verificationCode, email } = parseUrlSearchParams(location.search)

    React.useEffect(() => {
        if (email == null || verificationCode == null) {
            navigate(app.LOGIN_PATH)
        } else {
            void (async () => {
                try {
                    await auth.confirmSignUp(email, verificationCode)
                    navigate(app.LOGIN_PATH + location.search.toString())
                } catch (error) {
                    logger.error('Error while confirming sign-up', error)
                    toastify.toast.error(
                        'Something went wrong! Please try again or contact the administrators.'
                    )
                    navigate(app.LOGIN_PATH)
                }
            })()
        }
        // This MUST only run once - this is fine because the above function *always* `navigate`s
        // away.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])

    return <></>
}

/** Return an object containing the query parameters, with keys renamed to `camelCase`. */
function parseUrlSearchParams(search: string) {
    const query = new URLSearchParams(search)
    const verificationCode = query.get(REGISTRATION_QUERY_PARAMS.verificationCode)
    const email = query.get(REGISTRATION_QUERY_PARAMS.email)
    return { verificationCode, email }
}

export default ConfirmRegistration
