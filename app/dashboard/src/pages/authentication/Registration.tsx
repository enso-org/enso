/** @file Registration container responsible for rendering and interactions in sign up flow. */
import { useEffect, useState } from 'react'
import { useLocation } from 'react-router-dom'

import * as z from 'zod'

import { LOGIN_PATH } from '#/appUtils'
import AtIcon from '#/assets/at.svg'
import CreateAccountIcon from '#/assets/create_account.svg'
import GoBackIcon from '#/assets/go_back.svg'
import LockIcon from '#/assets/lock.svg'
import { Form, Input, Password } from '#/components/AriaComponents'
import Link from '#/components/Link'
import AuthenticationPage from '#/pages/authentication/AuthenticationPage'
import { passwordWithPatternSchema } from '#/pages/authentication/schemas'
import { useAuth } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useLocalStorage } from '#/providers/LocalStorageProvider'
import { type GetText, useText } from '#/providers/TextProvider'
import LocalStorage from '#/utilities/LocalStorage'
import { PASSWORD_PATTERN } from '#/utilities/validation'

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

/** Create the schema for this form. */
function createRegistrationFormSchema(getText: GetText) {
  return z
    .object({
      email: z.string().email(getText('invalidEmailValidationError')),
      password: passwordWithPatternSchema(getText),
      confirmPassword: z.string(),
    })
    .refine(
      (object) => object.password === object.confirmPassword,
      getText('passwordMismatchError'),
    )
}

// ====================
// === Registration ===
// ====================

/** A form for users to register an account. */
export default function Registration() {
  const { signUp } = useAuth()
  const location = useLocation()
  const { localStorage } = useLocalStorage()
  const { getText } = useText()
  const localBackend = useLocalBackend()
  const supportsOffline = localBackend != null

  const query = new URLSearchParams(location.search)
  const initialEmail = query.get('email')
  const organizationId = query.get('organization_id')
  const redirectTo = query.get('redirect_to')
  const [emailInput, setEmailInput] = useState(initialEmail ?? '')

  useEffect(() => {
    if (redirectTo != null) {
      localStorage.set('loginRedirect', redirectTo)
    } else {
      localStorage.delete('loginRedirect')
    }
  }, [localStorage, redirectTo])

  return (
    <AuthenticationPage
      schema={createRegistrationFormSchema(getText)}
      title={getText('createANewAccount')}
      supportsOffline={supportsOffline}
      footer={
        <Link
          to={`${LOGIN_PATH}?${new URLSearchParams({ email: emailInput }).toString()}`}
          icon={GoBackIcon}
          text={getText('alreadyHaveAnAccount')}
        />
      }
      onSubmit={({ email, password }) => signUp(email, password, organizationId)}
    >
      <Input
        autoFocus
        required
        name="email"
        label={getText('emailLabel')}
        type="email"
        autoComplete="email"
        icon={AtIcon}
        placeholder={getText('emailPlaceholder')}
        defaultValue={initialEmail ?? undefined}
        onBlur={(event) => {
          setEmailInput(event.currentTarget.value)
        }}
      />
      <Password
        required
        name="password"
        label={getText('passwordLabel')}
        autoComplete="new-password"
        icon={LockIcon}
        placeholder={getText('passwordPlaceholder')}
        pattern={PASSWORD_PATTERN}
      />
      <Password
        required
        name="confirmPassword"
        label={getText('confirmPasswordLabel')}
        autoComplete="new-password"
        icon={LockIcon}
        placeholder={getText('confirmPasswordPlaceholder')}
      />

      <Form.FormError />
      <Form.Submit size="large" icon={CreateAccountIcon} className="w-full">
        {getText('register')}
      </Form.Submit>
    </AuthenticationPage>
  )
}
