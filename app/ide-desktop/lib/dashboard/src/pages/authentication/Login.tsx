/** @file Login component responsible for rendering and interactions in sign in flow. */
import * as React from 'react'

import * as fontawesomeIcons from '@fortawesome/free-brands-svg-icons'
import * as router from 'react-router-dom'

import ArrowRightIcon from 'enso-assets/arrow_right.svg'
import AtIcon from 'enso-assets/at.svg'
import CreateAccountIcon from 'enso-assets/create_account.svg'
import LockIcon from 'enso-assets/lock.svg'
import * as common from 'enso-common'
import * as detect from 'enso-common/src/detect'

import * as appUtils from '#/appUtils'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import AuthenticationPage from '#/pages/authentication/AuthenticationPage'

import FontAwesomeIcon from '#/components/FontAwesomeIcon'
import Input from '#/components/Input'
import Link from '#/components/Link'
import SubmitButton from '#/components/SubmitButton'
import TextLink from '#/components/TextLink'
import UnstyledButton from '#/components/UnstyledButton'

import * as eventModule from '#/utilities/event'

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
  const formRef = React.useRef<HTMLFormElement>(null)

  return (
    <AuthenticationPage
      isNotForm
      title={getText('loginToYourAccount')}
      footer={
        <>
          <Link
            openInBrowser={detect.isOnElectron()}
            to={
              detect.isOnElectron()
                ? 'https://' + common.CLOUD_DASHBOARD_DOMAIN + appUtils.REGISTRATION_PATH
                : appUtils.REGISTRATION_PATH
            }
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
        </>
      }
    >
      <div className="flex flex-col gap-auth">
        <UnstyledButton
          onPress={() => {
            shouldReportValidityRef.current = false
            void signInWithGoogle()
          }}
          className="relative rounded-full bg-primary/5 py-auth-input-y transition-all duration-auth hover:bg-primary/10 focus:bg-primary/10"
        >
          <FontAwesomeIcon icon={fontawesomeIcons.faGoogle} />
          {getText('signUpOrLoginWithGoogle')}
        </UnstyledButton>
        <UnstyledButton
          onPress={() => {
            shouldReportValidityRef.current = false
            void signInWithGitHub()
          }}
          className="relative rounded-full bg-primary/5 py-auth-input-y transition-all duration-auth hover:bg-primary/10 focus:bg-primary/10"
        >
          <FontAwesomeIcon icon={fontawesomeIcons.faGithub} />
          {getText('signUpOrLoginWithGitHub')}
        </UnstyledButton>
      </div>
      <div />
      <form
        ref={formRef}
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
          autoFocus
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
          <TextLink to={appUtils.FORGOT_PASSWORD_PATH} text={getText('forgotYourPassword')} />
        </div>
        <SubmitButton
          isDisabled={isSubmitting}
          text={getText('login')}
          icon={ArrowRightIcon}
          onPress={eventModule.submitForm}
        />
      </form>
    </AuthenticationPage>
  )
}
