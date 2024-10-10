/** @file Login component responsible for rendering and interactions in sign in flow. */
import * as router from 'react-router-dom'

import { CLOUD_DASHBOARD_DOMAIN } from 'enso-common'

import { DASHBOARD_PATH, FORGOT_PASSWORD_PATH, REGISTRATION_PATH } from '#/appUtils'
import ArrowRightIcon from '#/assets/arrow_right.svg'
import AtIcon from '#/assets/at.svg'
import CreateAccountIcon from '#/assets/create_account.svg'
import GithubIcon from '#/assets/github_color.svg'
import GoogleIcon from '#/assets/google_color.svg'
import LockIcon from '#/assets/lock.svg'
import type { CognitoUser } from '#/authentication/cognito'
import { Button, Form, Input, OTPInput, Password, Text } from '#/components/AriaComponents'
import Link from '#/components/Link'
import { Stepper } from '#/components/Stepper'
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
  const navigate = router.useNavigate()
  const { signInWithGoogle, signInWithGitHub, signInWithPassword, cognito } = useAuth()
  const { getText } = useText()

  const query = new URLSearchParams(location.search)
  const initialEmail = query.get('email') ?? ''

  const form = Form.useForm({
    schema: (z) =>
      z.object({
        email: z
          .string()
          .min(1, getText('arbitraryFieldRequired'))
          .email(getText('invalidEmailValidationError')),
        password: passwordSchema(getText),
      }),
    defaultValues: { email: initialEmail },
    onSubmit: async ({ email, password }) => {
      const res = await signInWithPassword(email, password)

      switch (res.challenge) {
        case 'NO_CHALLENGE':
          navigate(DASHBOARD_PATH)
          break
        case 'SMS_MFA':
        case 'SOFTWARE_TOKEN_MFA':
          setUser(res.user)
          nextStep()
          break
        default:
          throw new Error('Unsupported challenge')
      }
    },
  })

  const [emailInput, setEmailInput] = useState(initialEmail)

  const [user, setUser] = useState<CognitoUser | null>(null)
  const localBackend = useLocalBackend()
  const supportsOffline = localBackend != null

  const { nextStep, stepperState, previousStep } = Stepper.useStepperState({
    steps: 2,
    defaultStep: 0,
  })

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
      <Stepper state={stepperState} renderStep={() => null}>
        <Stepper.StepContent index={0}>
          {() => (
            <div className="flex flex-col gap-auth">
              <Button
                size="large"
                variant="outline"
                icon={<img src={GoogleIcon} alt={getText('googleIcon')} />}
                onPress={async () => {
                  await signInWithGoogle()
                }}
              >
                {getText('signUpOrLoginWithGoogle')}
              </Button>
              <Button
                size="large"
                variant="outline"
                icon={<img src={GithubIcon} alt={getText('gitHubIcon')} />}
                onPress={async () => {
                  await signInWithGitHub()
                }}
              >
                {getText('signUpOrLoginWithGitHub')}
              </Button>

              <Form form={form} gap="medium">
                <Input
                  autoFocus
                  required
                  data-testid="email-input"
                  name="email"
                  label={getText('email')}
                  type="email"
                  autoComplete="email"
                  icon={AtIcon}
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
            </div>
          )}
        </Stepper.StepContent>

        <Stepper.StepContent index={1}>
          {() => (
            <Form
              /* eslint-disable-next-line @typescript-eslint/no-magic-numbers */
              schema={(z) => z.object({ otp: z.string().min(6).max(6) })}
              onSubmit={async ({ otp }, formInstance) => {
                if (user) {
                  const res = await cognito.confirmSignIn(user, otp, 'SOFTWARE_TOKEN_MFA')

                  if (res.ok) {
                    navigate(DASHBOARD_PATH)
                  } else {
                    switch (res.val.code) {
                      case 'NotAuthorizedException':
                        previousStep()
                        form.setFormError(res.val.message)
                        setUser(null)
                        break
                      case 'CodeMismatchException':
                        formInstance.setError('otp', { message: res.val.message })
                        break
                      default:
                        throw res.val
                    }
                  }
                }
              }}
            >
              <Text>{getText('enterTotp')}</Text>

              <OTPInput
                autoFocus
                required
                testId="otp-input"
                name="otp"
                label={getText('totp')}
                maxLength={6}
              />

              <Form.Submit size="large" icon={ArrowRightIcon} iconPosition="end" fullWidth>
                {getText('login')}
              </Form.Submit>

              <Form.FormError />
            </Form>
          )}
        </Stepper.StepContent>
      </Stepper>
    </AuthenticationPage>
  )
}
