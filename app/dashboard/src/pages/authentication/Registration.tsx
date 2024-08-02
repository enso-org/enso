/** @file Registration container responsible for rendering and interactions in sign up flow. */
import * as React from 'react'
import * as router from 'react-router-dom'

import * as z from 'zod'

import AtIcon from '#/assets/at.svg'
import GoBackIcon from '#/assets/go_back.svg'
import LockIcon from '#/assets/lock.svg'

import * as appUtils from '#/appUtils'

import * as authProvider from '#/providers/AuthProvider'
import * as backendProvider from '#/providers/BackendProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as textProvider from '#/providers/TextProvider'

import AuthenticationPage from '#/pages/authentication/AuthenticationPage'

import Link from '#/components/Link'

import { Form, Input } from '#/components/AriaComponents'
import LocalStorage from '#/utilities/LocalStorage'
import * as string from '#/utilities/string'
import * as validation from '#/utilities/validation'

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

// ====================
// === Registration ===
// ====================

/** A form for users to register an account. */
export default function Registration() {
  const auth = authProvider.useAuth()
  const location = router.useLocation()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { getText } = textProvider.useText()
  const localBackend = backendProvider.useLocalBackend()
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

  const form = Form.useForm({
    schema: z.object({
      email: z.string(),
      password: z.string(),
      confirmPassword: z.string(),
    }),
  })

  return (
    <AuthenticationPage
      form={form}
      title={getText('createANewAccount')}
      supportsOffline={supportsOffline}
      footer={
        <Link to={appUtils.LOGIN_PATH} icon={GoBackIcon} text={getText('alreadyHaveAnAccount')} />
      }
      onSubmit={async ({ email, password, confirmPassword }) => {
        if (!validation.PASSWORD_REGEX.test(password)) {
          throw new Error(getText('passwordValidationError'))
        } else if (password !== confirmPassword) {
          throw new Error(getText('passwordMismatchError'))
        } else {
          await auth.signUp(email, password, organizationId)
        }
      }}
    >
      {({ register }) => (
        <>
          <Input
            autoFocus
            required
            type="email"
            autoComplete="email"
            placeholder={getText('emailPlaceholder')}
            defaultValue={initialEmail ?? undefined}
            {...register('email')}
          />
          <Input
            required
            type="password"
            autoComplete="new-password"
            placeholder={getText('passwordPlaceholder')}
            pattern={validation.PASSWORD_PATTERN}
            {...register('password')}
          />
          <Input
            required
            type="password"
            autoComplete="new-password"
            placeholder={getText('confirmPasswordPlaceholder')}
            {...register('confirmPassword')}
          />
          <Form.FormError />
          <Form.Submit className="w-full">{getText('register')}</Form.Submit>
        </>
      )}
    </AuthenticationPage>
  )
}
