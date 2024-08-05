/** @file Container responsible for rendering and interactions in second half of forgot password
 * flow. */
import * as React from 'react'
import * as router from 'react-router-dom'

import isEmail from 'validator/lib/isEmail'
import * as z from 'zod'

import { LOGIN_PATH } from '#/appUtils'
import GoBackIcon from '#/assets/go_back.svg'
import { Form, Input } from '#/components/AriaComponents'
import Link from '#/components/Link'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import AuthenticationPage from '#/pages/authentication/AuthenticationPage'
import { passwordWithPatternSchema } from '#/pages/authentication/schemas'
import { useAuth } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { type GetText, useText } from '#/providers/TextProvider'

/** Create the schema for this page. */
function createResetPasswordFormSchema(getText: GetText) {
  return z
    .object({
      email: z.string().refine(isEmail, getText('invalidEmailValidationError')),
      verificationCode: z.string(),
      newPassword: passwordWithPatternSchema(getText),
      confirmNewPassword: z.string(),
    })
    .refine(
      (object) => object.newPassword === object.confirmNewPassword,
      getText('passwordMismatchError'),
    )
}

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
  const defaultEmail = query.get('email')
  const defaultVerificationCode = query.get('verification_code')

  React.useEffect(() => {
    if (defaultEmail == null) {
      toastAndLog('missingEmailError')
      navigate(LOGIN_PATH)
    } else if (defaultVerificationCode == null) {
      toastAndLog('missingVerificationCodeError')
      navigate(LOGIN_PATH)
    }
  }, [defaultEmail, navigate, defaultVerificationCode, getText, toastAndLog])

  return (
    <AuthenticationPage
      supportsOffline={supportsOffline}
      title={getText('resetYourPassword')}
      schema={createResetPasswordFormSchema(getText)}
      footer={<Link to={LOGIN_PATH} icon={GoBackIcon} text={getText('goBackToLogin')} />}
      onSubmit={({ email, verificationCode, newPassword }) =>
        resetPassword(email, verificationCode, newPassword)
      }
    >
      <Input
        required
        readOnly
        hidden
        name="email"
        type="email"
        autoComplete="email"
        placeholder={getText('emailPlaceholder')}
        value={defaultEmail ?? ''}
      />
      <Input
        required
        readOnly
        hidden
        name="verificationCode"
        type="text"
        autoComplete="one-time-code"
        placeholder={getText('confirmationCodePlaceholder')}
        value={defaultVerificationCode ?? ''}
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
