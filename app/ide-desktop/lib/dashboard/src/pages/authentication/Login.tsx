/** @file Login component responsible for rendering and interactions in sign in flow. */
import * as React from 'react'

import * as fontawesomeIcons from '@fortawesome/free-brands-svg-icons'
import * as router from 'react-router-dom'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import AtIcon from 'enso-assets/at.svg'
import CreateAccountIcon from 'enso-assets/create_account.svg'
import LockIcon from 'enso-assets/lock.svg'

import * as appUtils from '#/appUtils'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import FontAwesomeIcon from '#/components/FontAwesomeIcon'
import Input from '#/components/Input'
import Link from '#/components/Link'
import SubmitButton from '#/components/SubmitButton'

// =============
// === Login ===
// =============

/** Props for a {@link Login}. */
export interface LoginProps {
  readonly supportsLocalBackend: boolean
}

/** A form for users to log in. */
export default function Login(props: LoginProps) {
  const { supportsLocalBackend } = props
  const location = router.useLocation()
  const { signInWithGoogle, signInWithGitHub, signInWithPassword } = authProvider.useAuth()
  const { getText } = textProvider.useText()

  const query = new URLSearchParams(location.search)
  const initialEmail = query.get('email')

  const [email, setEmail] = React.useState(initialEmail ?? '')
  const [password, setPassword] = React.useState('')
  const [isSubmitting, setIsSubmitting] = React.useState(false)
  const shouldReportValidityRef = React.useRef(true)

  return (
    <div className="flex min-h-screen flex-col items-center justify-center gap-auth text-sm text-primary">
      <div className="flex w-full max-w-md flex-col gap-auth rounded-auth bg-selected-frame p-auth shadow-md">
        <div className="self-center text-xl font-medium">{getText('loginToYourAccount')}</div>
        <div className="flex flex-col gap-auth">
          <button
            onMouseDown={() => {
              shouldReportValidityRef.current = false
            }}
            onClick={async event => {
              event.preventDefault()
              await signInWithGoogle()
            }}
            className="relative rounded-full bg-cloud/10 py-auth-input-y transition-all duration-auth hover:bg-cloud/20 focus:bg-cloud/20"
          >
            <FontAwesomeIcon icon={fontawesomeIcons.faGoogle} />
            {getText('signUpOrLoginWithGoogle')}
          </button>
          <button
            onMouseDown={() => {
              shouldReportValidityRef.current = false
            }}
            onClick={async event => {
              event.preventDefault()
              await signInWithGitHub()
            }}
            className="relative rounded-full bg-cloud/10 py-auth-input-y transition-all duration-auth hover:bg-cloud/20 focus:bg-cloud/20"
          >
            <FontAwesomeIcon icon={fontawesomeIcons.faGithub} />
            {getText('signUpOrLoginWithGitHub')}
          </button>
        </div>
        <div />
        <form
          className="flex flex-col gap-auth"
          onSubmit={async event => {
            event.preventDefault()
            setIsSubmitting(true)
            await signInWithPassword(email, password)
            shouldReportValidityRef.current = true
            setIsSubmitting(false)
          }}
        >
          <Input
            required
            validate
            type="email"
            autoComplete="email"
            icon={AtIcon}
            placeholder={getText('emailPlaceholder')}
            value={email}
            setValue={setEmail}
            shouldReportValidityRef={shouldReportValidityRef}
          />
          <div className="flex flex-col">
            <Input
              required
              validate
              allowShowingPassword
              type="password"
              autoComplete="current-password"
              icon={LockIcon}
              placeholder={getText('passwordPlaceholder')}
              error={getText('passwordValidationError')}
              value={password}
              setValue={setPassword}
              shouldReportValidityRef={shouldReportValidityRef}
            />
            <router.Link
              to={appUtils.FORGOT_PASSWORD_PATH}
              className="text-end text-xs text-blue-500 transition-all duration-auth hover:text-blue-700 focus:text-blue-700"
            >
              {getText('forgotYourPassword')}
            </router.Link>
          </div>
          <SubmitButton disabled={isSubmitting} text={getText('login')} icon={ArrowRightIcon} />
        </form>
      </div>
      <Link
        to={appUtils.REGISTRATION_PATH}
        icon={CreateAccountIcon}
        text={getText('dontHaveAnAccount')}
      />
      {supportsLocalBackend && (
        <Link
          to={appUtils.ENTER_OFFLINE_MODE_PATH}
          icon={ArrowRightIcon}
          text={getText('continueWithoutCreatingAnAccount')}
        />
      )}
    </div>
  )
}
