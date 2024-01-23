/** @file Container responsible for rendering and interactions in second half of forgot password
 * flow. */
import * as React from 'react'

import * as router from 'react-router-dom'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import GoBackIcon from 'enso-assets/go_back.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as appUtils from '#/appUtils'
import * as navigateHooks from '#/hooks/navigateHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'
import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'
import * as string from '#/utilities/string'
import * as validation from '#/utilities/validation'

import Input from '#/components/Input'
import Link from '#/components/Link'
import SubmitButton from '#/components/SubmitButton'

// =================
// === Constants ===
// =================

const RESET_PASSWORD_QUERY_PARAMS = {
  email: 'email',
  verificationCode: 'verification_code',
} as const

// =====================
// === ResetPassword ===
// =====================

/** A form for users to reset their password. */
export default function ResetPassword() {
  const { resetPassword } = authProvider.useAuth()
  const { search } = router.useLocation()
  const { getText } = textProvider.useText()
  const navigate = navigateHooks.useNavigate()
  const toastAndLog = toastAndLogHooks.useToastAndLog()

  const { verificationCode, email } = parseUrlSearchParams(search)

  const [newPassword, setNewPassword] = React.useState('')
  const [newPasswordConfirm, setNewPasswordConfirm] = React.useState('')

  React.useEffect(() => {
    if (email == null) {
      toastAndLog('missingEmailError')
      navigate(appUtils.LOGIN_PATH)
    } else if (verificationCode == null) {
      toastAndLog('missingVerificationCodeError')
      navigate(appUtils.LOGIN_PATH)
    }
  }, [email, navigate, verificationCode, getText, /* should never change */ toastAndLog])

  const onSubmit = () => {
    if (newPassword !== newPasswordConfirm) {
      toastAndLog('passwordMismatchError')
      return Promise.resolve()
    } else {
      // These should never be nullish, as the effect should immediately navigate away.
      return resetPassword(email ?? '', verificationCode ?? '', newPassword)
    }
  }

  return (
    <div className="flex flex-col gap-6 text-primary text-sm items-center justify-center min-h-screen">
      <form
        className="flex flex-col gap-6 bg-frame-selected rounded-4xl shadow-md p-8 w-full max-w-md"
        onSubmit={async event => {
          event.preventDefault()
          await onSubmit()
        }}
      >
        <div className="font-medium self-center text-xl">{getText('resetYourPassword')}</div>
        <input
          required
          readOnly
          hidden
          type="email"
          autoComplete="email"
          placeholder={getText('emailPlaceholder')}
          value={email ?? ''}
        />
        <input
          required
          readOnly
          hidden
          type="text"
          autoComplete="one-time-code"
          placeholder={getText('confirmationCodePlaceholder')}
          value={verificationCode ?? ''}
        />
        <Input
          required
          validate
          allowShowingPassword
          type="password"
          autoComplete="new-password"
          label={getText('newPasswordLabel')}
          icon={LockIcon}
          placeholder={getText('newPasswordPlaceholder')}
          pattern={validation.PASSWORD_PATTERN}
          error={getText('passwordValidationError')}
          value={newPassword}
          setValue={setNewPassword}
        />
        <Input
          required
          validate
          allowShowingPassword
          type="password"
          autoComplete="new-password"
          label={getText('confirmNewPasswordLabel')}
          icon={LockIcon}
          placeholder={getText('confirmNewPasswordPlaceholder')}
          pattern={string.regexEscape(newPassword)}
          error={getText('passwordMismatchError')}
          value={newPasswordConfirm}
          setValue={setNewPasswordConfirm}
        />
        <SubmitButton text={getText('reset')} icon={ArrowRightIcon} />
      </form>
      <Link to={appUtils.LOGIN_PATH} icon={GoBackIcon} text={getText('goBackToLogin')} />
    </div>
  )
}

/** Return an object containing the query parameters, with keys renamed to `camelCase`. */
function parseUrlSearchParams(search: string) {
  const query = new URLSearchParams(search)
  const verificationCode = query.get(RESET_PASSWORD_QUERY_PARAMS.verificationCode)
  const email = query.get(RESET_PASSWORD_QUERY_PARAMS.email)
  return { verificationCode, email }
}
