/**
 * @file Container responsible for rendering and interactions in first half of forgot password
 * flow.
 */
import { useState } from 'react'

import isEmail from 'validator/lib/isEmail'
import * as z from 'zod'

import { LOGIN_PATH } from '#/appUtils'
import ArrowRightIcon from '#/assets/arrow_right.svg'
import AtIcon from '#/assets/at.svg'
import GoBackIcon from '#/assets/go_back.svg'
import { Form, Input } from '#/components/AriaComponents'
import Link from '#/components/Link'
import AuthenticationPage from '#/pages/authentication/AuthenticationPage'
import { useAuth } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { type GetText, useText } from '#/providers/TextProvider'
import { useLocation } from 'react-router'

/** Create the schema for this form. */
function createForgotPasswordFormSchema(getText: GetText) {
  return z.object({
    email: z.string().refine(isEmail, getText('invalidEmailValidationError')),
  })
}

// ======================
// === ForgotPassword ===
// ======================

/** A form for users to request for their password to be reset. */
export default function ForgotPassword() {
  const { forgotPassword } = useAuth()
  const location = useLocation()
  const { getText } = useText()
  const localBackend = useLocalBackend()
  const supportsOffline = localBackend != null

  const query = new URLSearchParams(location.search)
  const initialEmail = query.get('email')
  const [emailInput, setEmailInput] = useState(initialEmail ?? '')

  return (
    <AuthenticationPage
      title={getText('forgotYourPassword')}
      schema={createForgotPasswordFormSchema(getText)}
      footer={
        <Link
          to={`${LOGIN_PATH}?${new URLSearchParams({ email: emailInput }).toString()}`}
          icon={GoBackIcon}
          text={getText('goBackToLogin')}
        />
      }
      supportsOffline={supportsOffline}
      onSubmit={({ email }) => forgotPassword(email)}
    >
      <Input
        autoFocus
        required
        data-testid="email-input"
        name="email"
        label={getText('emailLabel')}
        type="email"
        autoComplete="email"
        icon={AtIcon}
        placeholder={getText('emailPlaceholder')}
        defaultValue={initialEmail ?? undefined}
        onChange={(event) => {
          setEmailInput(event.currentTarget.value)
        }}
      />

      <Form.Submit size="large" icon={ArrowRightIcon} iconPosition="end" fullWidth>
        {getText('sendLink')}
      </Form.Submit>

      <Form.FormError />
    </AuthenticationPage>
  )
}
