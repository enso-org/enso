/** @file Registration container responsible for rendering and interactions in sign up flow. */
import * as React from 'react'

import * as router from 'react-router-dom'

import AtIcon from 'enso-assets/at.svg'
import CreateAccountIcon from 'enso-assets/create_account.svg'
import GoBackIcon from 'enso-assets/go_back.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as appUtils from '#/appUtils'

import * as authProvider from '#/providers/AuthProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as textProvider from '#/providers/TextProvider'

import Input from '#/components/Input'
import Link from '#/components/Link'
import AuthenticationPage from '#/components/styled/AuthenticationPage'
import SubmitButton from '#/components/SubmitButton'

import * as eventModule from '#/utilities/event'
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
  tryParse: value => (typeof value === 'string' ? value : null),
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

  const query = new URLSearchParams(location.search)
  const initialEmail = query.get('email')
  const organizationId = query.get('organization_id')
  const redirectTo = query.get('redirect_to')

  const [email, setEmail] = React.useState(initialEmail ?? '')
  const [password, setPassword] = React.useState('')
  const [confirmPassword, setConfirmPassword] = React.useState('')
  const [isSubmitting, setIsSubmitting] = React.useState(false)

  React.useEffect(() => {
    if (redirectTo != null) {
      localStorage.set('loginRedirect', redirectTo)
    } else {
      localStorage.delete('loginRedirect')
    }
  }, [localStorage, redirectTo])

  return (
    <AuthenticationPage
      title={getText('createANewAccount')}
      footer={
        <Link to={appUtils.LOGIN_PATH} icon={GoBackIcon} text={getText('alreadyHaveAnAccount')} />
      }
      onSubmit={async event => {
        event.preventDefault()
        setIsSubmitting(true)
        await auth.signUp(email, password, organizationId)
        setIsSubmitting(false)
      }}
    >
      <Input
        autoFocus
        required
        validate
        type="email"
        autoComplete="email"
        icon={AtIcon}
        placeholder={getText('emailPlaceholder')}
        value={email}
        setValue={setEmail}
      />
      <Input
        required
        validate
        allowShowingPassword
        type="password"
        autoComplete="new-password"
        icon={LockIcon}
        placeholder={getText('passwordPlaceholder')}
        pattern={validation.PASSWORD_PATTERN}
        error={getText('passwordValidationError')}
        value={password}
        setValue={setPassword}
      />
      <Input
        required
        validate
        allowShowingPassword
        type="password"
        autoComplete="new-password"
        icon={LockIcon}
        placeholder={getText('confirmPasswordPlaceholder')}
        pattern={string.regexEscape(password)}
        error={getText('passwordMismatchError')}
        value={confirmPassword}
        setValue={setConfirmPassword}
      />
      <SubmitButton
        isDisabled={isSubmitting}
        text={getText('register')}
        icon={CreateAccountIcon}
        onPress={eventModule.submitForm}
      />
    </AuthenticationPage>
  )
}
