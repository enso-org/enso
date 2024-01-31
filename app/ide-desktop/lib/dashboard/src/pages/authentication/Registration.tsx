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

import Input from '#/components/Input'
import Link from '#/components/Link'
import SubmitButton from '#/components/SubmitButton'

import * as localStorageModule from '#/utilities/LocalStorage'
import * as string from '#/utilities/string'
import * as validation from '#/utilities/validation'

// ====================
// === Registration ===
// ====================

/** A form for users to register an account. */
export default function Registration() {
  const auth = authProvider.useAuth()
  const location = router.useLocation()
  const { localStorage } = localStorageProvider.useLocalStorage()

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
      localStorage.set(localStorageModule.LocalStorageKey.loginRedirect, redirectTo)
    } else {
      localStorage.delete(localStorageModule.LocalStorageKey.loginRedirect)
    }
  }, [localStorage, redirectTo])

  return (
    <div className="flex flex-col gap-6 text-primary text-sm items-center justify-center min-h-screen">
      <form
        className="flex flex-col gap-6 bg-frame-selected rounded-4xl shadow-md p-8 w-full max-w-md"
        onSubmit={async event => {
          event.preventDefault()
          setIsSubmitting(true)
          await auth.signUp(email, password, organizationId)
          setIsSubmitting(false)
        }}
      >
        <div className="font-medium self-center text-xl">Create a new account</div>
        <Input
          required
          validate
          type="email"
          autoComplete="email"
          label="Email"
          icon={AtIcon}
          placeholder="Enter your email"
          value={email}
          setValue={setEmail}
        />
        <Input
          required
          validate
          allowShowingPassword
          type="password"
          autoComplete="new-password"
          label="Password"
          icon={LockIcon}
          placeholder="Enter your password"
          pattern={validation.PASSWORD_PATTERN}
          error={validation.PASSWORD_ERROR}
          value={password}
          setValue={setPassword}
        />
        <Input
          required
          validate
          allowShowingPassword
          type="password"
          autoComplete="new-password"
          label="Confirm password"
          icon={LockIcon}
          placeholder="Confirm your password"
          pattern={string.regexEscape(password)}
          error={validation.CONFIRM_PASSWORD_ERROR}
          value={confirmPassword}
          setValue={setConfirmPassword}
        />
        <SubmitButton disabled={isSubmitting} text="Register" icon={CreateAccountIcon} />
      </form>
      <Link to={appUtils.LOGIN_PATH} icon={GoBackIcon} text="Already have an account?" />
    </div>
  )
}
