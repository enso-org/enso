/** @file Registration confirmation page for when a user clicks the confirmation link set to their
 * email address. */
import * as React from 'react'

import * as router from 'react-router-dom'
import * as toastify from 'react-toastify'

import * as appUtils from '#/appUtils'

import * as navigateHooks from '#/hooks/navigateHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as loggerProvider from '#/providers/LoggerProvider'

// ===========================
// === ConfirmRegistration ===
// ===========================

/** An empty component redirecting users based on the backend response to user registration. */
export default function ConfirmRegistration() {
  const logger = loggerProvider.useLogger()
  const auth = authProvider.useAuth()
  const location = router.useLocation()
  const navigate = navigateHooks.useNavigate()

  const query = new URLSearchParams(location.search)
  const verificationCode = query.get('verification_code')
  const email = query.get('email')
  const redirectUrl = query.get('redirect_url')

  React.useEffect(() => {
    if (email == null || verificationCode == null) {
      navigate(appUtils.LOGIN_PATH)
    } else {
      void (async () => {
        try {
          await auth.confirmSignUp(email, verificationCode)
          if (redirectUrl != null) {
            window.location.href = redirectUrl
          } else {
            navigate(appUtils.LOGIN_PATH + location.search.toString())
          }
        } catch (error) {
          logger.error('Error while confirming sign-up', error)
          toastify.toast.error(
            'Something went wrong! Please try again or contact the administrators.'
          )
          navigate(appUtils.LOGIN_PATH)
        }
      })()
    }
    // This MUST only run once - this is fine because the above function *always* `navigate`s
    // away.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  return <></>
}
