/** @file Container responsible for rendering and interactions in second half of forgot password
 * flow. */
import * as React from 'react'
import * as router from 'react-router-dom'

import * as z from 'zod'

import { LOGIN_PATH } from '#/appUtils'
import GoBackIcon from '#/assets/go_back.svg'
import { Form, Input } from '#/components/AriaComponents'
import Link from '#/components/Link'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import AuthenticationPage from '#/pages/authentication/AuthenticationPage'
import { useAuth } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useText } from '#/providers/TextProvider'
import { PASSWORD_REGEX } from '#/utilities/validation'

const RESET_PASSWORD_FORM_SCHEMA = z.object({
  email: z.string(),
  verificationCode: z.string(),
  newPassword: z.string(),
  confirmNewPassword: z.string(),
})

// =====================
// === ResetPassword ===
// =====================

/** A form for users to reset their password. */
export default function ResetPassword() {
  const { resetPassword } = useAuth()
  const { getText } = useText()
  const location = router.useLocation()
  const navigate = router.useNavigate()
  const toastAndLog = useToastAndLog()
  const localBackend = useLocalBackend()
  const supportsOffline = localBackend != null

  const query = new URLSearchParams(location.search)
  const email = query.get('email')
  const verificationCode = query.get('verification_code')

  React.useEffect(() => {
    if (email == null) {
      toastAndLog('missingEmailError')
      navigate(LOGIN_PATH)
    } else if (verificationCode == null) {
      toastAndLog('missingVerificationCodeError')
      navigate(LOGIN_PATH)
    }
  }, [email, navigate, verificationCode, getText, toastAndLog])

  return (
    <AuthenticationPage
      supportsOffline={supportsOffline}
      title={getText('resetYourPassword')}
      schema={RESET_PASSWORD_FORM_SCHEMA}
      footer={<Link to={LOGIN_PATH} icon={GoBackIcon} text={getText('goBackToLogin')} />}
      onSubmit={async ({ newPassword, confirmNewPassword }) => {
        if (!PASSWORD_REGEX.test(newPassword)) {
          throw new Error(getText('passwordValidationError'))
        } else if (newPassword !== confirmNewPassword) {
          throw new Error(getText('passwordMismatchError'))
        } else {
          // These should never be nullish, as the effect should immediately navigate away.
          return await resetPassword(email ?? '', verificationCode ?? '', newPassword)
        }
      }}
    >
      <Input
        required
        readOnly
        hidden
        name="email"
        type="email"
        autoComplete="email"
        placeholder={getText('emailPlaceholder')}
        value={email ?? ''}
      />
      <Input
        required
        readOnly
        hidden
        name="verificationCode"
        type="text"
        autoComplete="one-time-code"
        placeholder={getText('confirmationCodePlaceholder')}
        value={verificationCode ?? ''}
      />
      <Input
        autoFocus
        required
        name="newPassword"
        type="password"
        autoComplete="new-password"
        placeholder={getText('newPasswordPlaceholder')}
      />
      <Input
        required
        name="confirmNewPassword"
        type="password"
        autoComplete="new-password"
        placeholder={getText('confirmNewPasswordPlaceholder')}
      />

      <Form.FormError />
      <Form.Submit className="w-full">{getText('reset')}</Form.Submit>
    </AuthenticationPage>
  )
}
