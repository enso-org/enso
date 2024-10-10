/**
 * @file Container responsible for rendering and interactions in second half of forgot password
 * flow.
 */
import * as React from 'react'
import * as router from 'react-router-dom'

import isEmail from 'validator/lib/isEmail'
import * as z from 'zod'

import { LOGIN_PATH } from '#/appUtils'
import ArrowRightIcon from '#/assets/arrow_right.svg'
import GoBackIcon from '#/assets/go_back.svg'
import LockIcon from '#/assets/lock.svg'
import { Form, Input, Password } from '#/components/AriaComponents'
import Link from '#/components/Link'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import AuthenticationPage from '#/pages/authentication/AuthenticationPage'
import { passwordWithPatternSchema } from '#/pages/authentication/schemas'
import { useAuth } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { type GetText, useText } from '#/providers/TextProvider'
import { PASSWORD_REGEX } from '#/utilities/validation'

/** Create the schema for this form. */
function createResetPasswordFormSchema(getText: GetText) {
  return z
    .object({
      email: z.string().refine(isEmail, getText('invalidEmailValidationError')),
      verificationCode: z.string(),
      newPassword: passwordWithPatternSchema(getText),
      confirmNewPassword: z.string(),
    })
    .superRefine((object, context) => {
      if (
        PASSWORD_REGEX.test(object.newPassword) &&
        object.newPassword !== object.confirmNewPassword
      ) {
        context.addIssue({
          path: ['confirmNewPassword'],
          code: 'custom',
          message: getText('passwordMismatchError'),
        })
      }
    })
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
      footer={
        <Link
          to={`${LOGIN_PATH}?${new URLSearchParams({ email: defaultEmail ?? '' }).toString()}`}
          icon={GoBackIcon}
          text={getText('goBackToLogin')}
        />
      }
      onSubmit={({ email, verificationCode, newPassword }) =>
        resetPassword(email, verificationCode, newPassword)
      }
    >
      <Input
        required
        readOnly
        hidden
        data-testid="email-input"
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
        data-testid="verification-code-input"
        name="verificationCode"
        type="text"
        autoComplete="one-time-code"
        placeholder={getText('confirmationCodePlaceholder')}
        value={defaultVerificationCode ?? ''}
      />
      <Password
        autoFocus
        required
        data-testid="new-password-input"
        name="newPassword"
        label={getText('newPasswordLabel')}
        autoComplete="new-password"
        icon={LockIcon}
        placeholder={getText('newPasswordPlaceholder')}
        description={getText('passwordValidationMessage')}
      />
      <Password
        required
        data-testid="confirm-new-password-input"
        name="confirmNewPassword"
        label={getText('confirmNewPasswordLabel')}
        autoComplete="new-password"
        icon={LockIcon}
        placeholder={getText('confirmNewPasswordPlaceholder')}
      />

      <Form.FormError />
      <Form.Submit size="large" icon={ArrowRightIcon} className="w-full">
        {getText('reset')}
      </Form.Submit>
    </AuthenticationPage>
  )
}
