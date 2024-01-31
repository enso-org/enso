/** @file Container responsible for rendering and interactions in second half of forgot password
 * flow. */
import * as React from 'react'

import * as router from 'react-router-dom'
import * as toastify from 'react-toastify'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import GoBackIcon from 'enso-assets/go_back.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as appUtils from '#/appUtils'

import * as navigateHooks from '#/hooks/navigateHooks'

import * as authProvider from '#/providers/AuthProvider'

import Input from '#/components/Input'
import Link from '#/components/Link'
import SubmitButton from '#/components/SubmitButton'

import * as string from '#/utilities/string'
import * as validation from '#/utilities/validation'

// =====================
// === ResetPassword ===
// =====================

/** A form for users to reset their password. */
export default function ResetPassword() {
  const { resetPassword } = authProvider.useAuth()
  const location = router.useLocation()
  const navigate = navigateHooks.useNavigate()

  const query = new URLSearchParams(location.search)
  const email = query.get('email')
  const verificationCode = query.get('verification_code')

  const [newPassword, setNewPassword] = React.useState('')
  const [newPasswordConfirm, setNewPasswordConfirm] = React.useState('')

  React.useEffect(() => {
    if (email == null) {
      toastify.toast.error('Could not reset password: missing email address')
      navigate(appUtils.LOGIN_PATH)
    } else if (verificationCode == null) {
      toastify.toast.error('Could not reset password: missing verification code')
      navigate(appUtils.LOGIN_PATH)
    }
  }, [email, navigate, verificationCode])

  const onSubmit = () => {
    if (newPassword !== newPasswordConfirm) {
      toastify.toast.error('Passwords do not match')
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
        <div className="font-medium self-center text-xl">Reset your password</div>
        <input
          required
          readOnly
          hidden
          type="email"
          autoComplete="email"
          placeholder="Enter your email"
          value={email ?? ''}
        />
        <input
          required
          readOnly
          hidden
          type="text"
          autoComplete="one-time-code"
          placeholder="Enter the confirmation code"
          value={verificationCode ?? ''}
        />
        <Input
          required
          validate
          allowShowingPassword
          type="password"
          autoComplete="new-password"
          label="New password"
          icon={LockIcon}
          placeholder="Enter your new password"
          pattern={validation.PASSWORD_PATTERN}
          error={validation.PASSWORD_ERROR}
          value={newPassword}
          setValue={setNewPassword}
        />
        <Input
          required
          validate
          allowShowingPassword
          type="password"
          autoComplete="new-password"
          label="Confirm new password"
          icon={LockIcon}
          placeholder="Confirm your new password"
          pattern={string.regexEscape(newPassword)}
          error={validation.CONFIRM_PASSWORD_ERROR}
          value={newPasswordConfirm}
          setValue={setNewPasswordConfirm}
        />
        <SubmitButton text="Reset" icon={ArrowRightIcon} />
      </form>
      <Link to={appUtils.LOGIN_PATH} icon={GoBackIcon} text="Go back to login" />
    </div>
  )
}
