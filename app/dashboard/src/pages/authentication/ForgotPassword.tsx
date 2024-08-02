/** @file Container responsible for rendering and interactions in first half of forgot password
 * flow. */
import * as React from 'react'

import * as z from 'zod'

import { LOGIN_PATH } from '#/appUtils'
import GoBackIcon from '#/assets/go_back.svg'
import { Form, Input } from '#/components/AriaComponents'
import Link from '#/components/Link'
import AuthenticationPage from '#/pages/authentication/AuthenticationPage'
import { useAuth } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useText } from '#/providers/TextProvider'

const FORGOT_PASSWORD_FORM_SCHEMA = z.object({ email: z.string() })

// ======================
// === ForgotPassword ===
// ======================

/** A form for users to request for their password to be reset. */
export default function ForgotPassword() {
  const { forgotPassword } = useAuth()
  const { getText } = useText()
  const localBackend = useLocalBackend()
  const supportsOffline = localBackend != null

  return (
    <AuthenticationPage
      title={getText('forgotYourPassword')}
      schema={FORGOT_PASSWORD_FORM_SCHEMA}
      footer={<Link to={LOGIN_PATH} icon={GoBackIcon} text={getText('goBackToLogin')} />}
      supportsOffline={supportsOffline}
      onSubmit={async ({ email }) => {
        await forgotPassword(email)
      }}
    >
      <Input
        autoFocus
        required
        name="email"
        type="email"
        autoComplete="email"
        placeholder={getText('emailPlaceholder')}
      />

      <Form.FormError />
      <Form.Submit className="w-full">{getText('sendLink')}</Form.Submit>
    </AuthenticationPage>
  )
}
