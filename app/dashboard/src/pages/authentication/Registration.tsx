/** @file Registration container responsible for rendering and interactions in sign up flow. */
import * as React from 'react'
import * as router from 'react-router-dom'

import * as z from 'zod'

import { LOGIN_PATH } from '#/appUtils'
import GoBackIcon from '#/assets/go_back.svg'
import { Form, Input } from '#/components/AriaComponents'
import Link from '#/components/Link'
import AuthenticationPage from '#/pages/authentication/AuthenticationPage'
import { useAuth } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useLocalStorage } from '#/providers/LocalStorageProvider'
import { useText } from '#/providers/TextProvider'
import LocalStorage from '#/utilities/LocalStorage'
import { PASSWORD_PATTERN, PASSWORD_REGEX } from '#/utilities/validation'

// ============================
// === Global configuration ===
// ============================

declare module '#/utilities/LocalStorage' {
  /** */
  interface LocalStorageData {
    readonly loginRedirect: string
  }
}

LocalStorage.registerKey('loginRedirect', {
  isUserSpecific: true,
  tryParse: (value) => (typeof value === 'string' ? value : null),
})

const REGISTRATION_FORM_SCHEMA = z.object({
  email: z.string(),
  password: z.string(),
  confirmPassword: z.string(),
})

// ====================
// === Registration ===
// ====================

/** A form for users to register an account. */
export default function Registration() {
  const auth = useAuth()
  const location = router.useLocation()
  const { localStorage } = useLocalStorage()
  const { getText } = useText()
  const localBackend = useLocalBackend()
  const supportsOffline = localBackend != null

  const query = new URLSearchParams(location.search)
  const initialEmail = query.get('email')
  const organizationId = query.get('organization_id')
  const redirectTo = query.get('redirect_to')

  React.useEffect(() => {
    if (redirectTo != null) {
      localStorage.set('loginRedirect', redirectTo)
    } else {
      localStorage.delete('loginRedirect')
    }
  }, [localStorage, redirectTo])

  return (
    <AuthenticationPage
      schema={REGISTRATION_FORM_SCHEMA}
      title={getText('createANewAccount')}
      supportsOffline={supportsOffline}
      footer={<Link to={LOGIN_PATH} icon={GoBackIcon} text={getText('alreadyHaveAnAccount')} />}
      onSubmit={async ({ email, password, confirmPassword }): Promise<void> => {
        if (!PASSWORD_REGEX.test(password)) {
          throw new Error(getText('passwordValidationError'))
        } else if (password !== confirmPassword) {
          throw new Error(getText('passwordMismatchError'))
        } else {
          await auth.signUp(email, password, organizationId)
          return
        }
      }}
    >
      <Input
        autoFocus
        required
        name="email"
        type="email"
        autoComplete="email"
        placeholder={getText('emailPlaceholder')}
        defaultValue={initialEmail ?? undefined}
      />
      <Input
        required
        name="password"
        type="password"
        autoComplete="new-password"
        placeholder={getText('passwordPlaceholder')}
        pattern={PASSWORD_PATTERN}
      />
      <Input
        required
        name="confirmPassword"
        type="password"
        autoComplete="new-password"
        placeholder={getText('confirmPasswordPlaceholder')}
      />

      <Form.FormError />
      <Form.Submit className="w-full">{getText('register')}</Form.Submit>
    </AuthenticationPage>
  )
}
