/** @file Login component responsible for rendering and interactions in sign in flow. */
import * as React from 'react'
import * as router from 'react-router-dom'

import { CLOUD_DASHBOARD_DOMAIN } from 'enso-common'

import { FORGOT_PASSWORD_PATH, REGISTRATION_PATH } from '#/appUtils'
import ArrowRightIcon from '#/assets/arrow_right.svg'
import AtIcon from '#/assets/at.svg'
import CreateAccountIcon from '#/assets/create_account.svg'
import GithubIcon from '#/assets/github.svg'
import GoogleIcon from '#/assets/google.svg'
import LockIcon from '#/assets/lock.svg'
import { Button } from '#/components/AriaComponents'
import Input from '#/components/Input'
import Link from '#/components/Link'
import SubmitButton from '#/components/SubmitButton'
import TextLink from '#/components/TextLink'
import AuthenticationPage from '#/pages/authentication/AuthenticationPage'
import { useAuth } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useText } from '#/providers/TextProvider'

// =============
// === Login ===
// =============

/** A form for users to log in. */
export default function Login() {
  const location = router.useLocation()
  const { signInWithGoogle, signInWithGitHub, signInWithPassword } = useAuth()
  const { getText } = useText()

  const query = new URLSearchParams(location.search)
  const initialEmail = query.get('email')

  const [email, setEmail] = React.useState(initialEmail ?? '')
  const [password, setPassword] = React.useState('')
  const [isSubmitting, setIsSubmitting] = React.useState(false)
  const shouldReportValidityRef = React.useRef(true)
  const formRef = React.useRef<HTMLFormElement>(null)

  const localBackend = useLocalBackend()
  const supportsOffline = localBackend != null

  return (
    <AuthenticationPage
      isNotForm
      title={getText('loginToYourAccount')}
      supportsOffline={supportsOffline}
      footer={
        <>
          <Link
            openInBrowser={localBackend != null}
            to={
              localBackend != null ?
                'https://' + CLOUD_DASHBOARD_DOMAIN + REGISTRATION_PATH
              : REGISTRATION_PATH
            }
            icon={CreateAccountIcon}
            text={getText('dontHaveAnAccount')}
          />
        </>
      }
    >
      <div className="flex flex-col gap-auth">
        <Button
          size="custom"
          variant="custom"
          fullWidthText
          icon={GoogleIcon}
          className="bg-primary/5 px-3 py-2 hover:bg-primary/10 focus:bg-primary/10"
          onPress={() => {
            shouldReportValidityRef.current = false
            void signInWithGoogle()
            setIsSubmitting(true)
          }}
        >
          {getText('signUpOrLoginWithGoogle')}
        </Button>
        <Button
          size="custom"
          variant="custom"
          fullWidthText
          icon={GithubIcon}
          className="bg-primary/5 px-3 py-2 hover:bg-primary/10 focus:bg-primary/10"
          onPress={() => {
            shouldReportValidityRef.current = false
            void signInWithGitHub()
            setIsSubmitting(true)
          }}
        >
          {getText('signUpOrLoginWithGitHub')}
        </Button>
      </div>
      <div />
      <form
        ref={formRef}
        className="flex flex-col gap-auth"
        onSubmit={async (event) => {
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
          <TextLink to={FORGOT_PASSWORD_PATH} text={getText('forgotYourPassword')} />
        </div>

        <SubmitButton
          isDisabled={isSubmitting}
          isLoading={isSubmitting}
          text={getText('login')}
          icon={ArrowRightIcon}
        />
      </form>
    </AuthenticationPage>
  )
}
