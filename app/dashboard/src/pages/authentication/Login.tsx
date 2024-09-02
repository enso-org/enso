/** @file Login component responsible for rendering and interactions in sign in flow. */
import * as router from 'react-router-dom'

import { CLOUD_DASHBOARD_DOMAIN } from 'enso-common'

import { FORGOT_PASSWORD_PATH, REGISTRATION_PATH } from '#/appUtils'
import ArrowRightIcon from '#/assets/arrow_right.svg'
import AtIcon from '#/assets/at.svg'
import CreateAccountIcon from '#/assets/create_account.svg'
import GithubIcon from '#/assets/github_color.svg'
import GoogleIcon from '#/assets/google_color.svg'
import LockIcon from '#/assets/lock.svg'
import { Button, Form, Input, Password } from '#/components/AriaComponents'
import Link from '#/components/Link'
import AuthenticationPage from '#/pages/authentication/AuthenticationPage'
import { passwordSchema } from '#/pages/authentication/schemas'
import { useAuth } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useText } from '#/providers/TextProvider'
import { useState } from 'react'

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
  const [emailInput, setEmailInput] = useState(initialEmail ?? '')
  const localBackend = useLocalBackend()
  const supportsOffline = localBackend != null

  return (
    <AuthenticationPage
      title={getText('loginToYourAccount')}
      supportsOffline={supportsOffline}
      footer={
        <Link
          openInBrowser={localBackend != null}
          to={(() => {
            const newQuery = new URLSearchParams({ email: emailInput }).toString()
            return localBackend != null ?
                `https://${CLOUD_DASHBOARD_DOMAIN}${REGISTRATION_PATH}?${newQuery}`
              : `${REGISTRATION_PATH}?${newQuery}`
          })()}
          icon={CreateAccountIcon}
          text={getText('dontHaveAnAccount')}
        />
      }
    >
      <div className="flex flex-col gap-auth">
        <Button
          size="large"
          variant="outline"
          icon={<img src={GoogleIcon} />}
          onPress={async () => {
            await signInWithGoogle()
          }}
        >
          {getText('signUpOrLoginWithGoogle')}
        </Button>
        <Button
          size="large"
          variant="outline"
          icon={<img src={GithubIcon} />}
          onPress={async () => {
            await signInWithGitHub()
          }}
        >
          {getText('signUpOrLoginWithGitHub')}
        </Button>
      </div>

      <Form
        schema={(z) =>
          z.object({
            email: z
              .string()
              .min(1, getText('arbitraryFieldRequired'))
              .email(getText('invalidEmailValidationError')),
            password: passwordSchema(getText),
          })
        }
        gap="medium"
        defaultValues={{ email: initialEmail ?? '' }}
        onSubmit={({ email, password }) => signInWithPassword(email, password)}
      >
        <Input
          autoFocus
          required
          data-testid="email-input"
          name="email"
          label={getText('email')}
          type="email"
          autoComplete="email"
          icon={AtIcon}
          defaultValue={initialEmail ?? undefined}
          placeholder={getText('emailPlaceholder')}
          onChange={(event) => {
            setEmailInput(event.currentTarget.value)
          }}
        />

        <div className="flex w-full flex-col">
          <Password
            required
            data-testid="password-input"
            name="password"
            label={getText('password')}
            autoComplete="current-password"
            icon={LockIcon}
            placeholder={getText('passwordPlaceholder')}
          />

          <Button
            variant="link"
            href={`${FORGOT_PASSWORD_PATH}?${new URLSearchParams({ email: emailInput }).toString()}`}
            size="small"
            className="self-end"
          >
            {getText('forgotYourPassword')}
          </Button>
        </div>

        <Form.Submit size="large" icon={ArrowRightIcon} iconPosition="end" fullWidth>
          {getText('login')}
        </Form.Submit>

        <Form.FormError />
      </Form>
    </AuthenticationPage>
  )
}
