/** @file Login component responsible for rendering and interactions in sign in flow. */
import * as router from 'react-router-dom'

import { CLOUD_DASHBOARD_DOMAIN } from 'enso-common'
import { isOnElectron } from 'enso-common/src/detect'

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
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import AuthenticationPage from '#/pages/authentication/AuthenticationPage'
import { passwordSchema } from '#/pages/authentication/schemas'
import { useSessionAPI } from '#/providers/SessionProvider'
import { useText } from '#/providers/TextProvider'
import { useQueryClient } from '@tanstack/react-query'
import { useEffect, useState } from 'react'

// eslint-disable-next-line no-restricted-syntax
const GOOGLE_ICON = <img src={GoogleIcon} alt="" />
// eslint-disable-next-line no-restricted-syntax
const GITHUB_ICON = <img src={GithubIcon} alt="" />

// =============
// === Login ===
// =============

/** A form for users to log in. */
export default function Login() {
  const location = router.useLocation()
  const navigate = router.useNavigate()
  const queryClient = useQueryClient()
  const { signInWithGoogle, signInWithGitHub, signInWithPassword, confirmSignIn } = useSessionAPI()
  const { getText } = useText()

  const query = new URLSearchParams(location.search)
  const initialEmail = query.get('email') ?? ''

  useEffect(() => {
    void queryClient.clearWithPersister()
  }, [queryClient])

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
        case 'SMS_MFA':
        case 'SOFTWARE_TOKEN_MFA':
          setUser(res.user)
          nextStep()
          break
        case 'NO_CHALLENGE':
        case 'CUSTOM_CHALLENGE':
        case 'MFA_SETUP':
        case 'NEW_PASSWORD_REQUIRED':
        case 'SELECT_MFA_TYPE':
        default:
          navigate(DASHBOARD_PATH)
      }
    },
  })

  const [user, setUser] = useState<CognitoUser | null>(null)

  const isElectron = isOnElectron()
  const supportsOffline = isElectron

  const { nextStep, stepperState, previousStep } = Stepper.useStepperState({
    steps: 2,
    defaultStep: 0,
  })

  const handleGooglePress = useEventCallback(async () => {
    await signInWithGoogle()
  })

  const handleGitHubPress = useEventCallback(async () => {
    await signInWithGitHub()
  })

  return (
    <AuthenticationPage
      title={getText('loginToYourAccount')}
      supportsOffline={supportsOffline}
      footer={
        <Form.FieldValue form={form} name="email">
          {(email) => (
            <Link
              openInBrowser={isElectron}
              to={(() => {
                const newQuery = new URLSearchParams({ email }).toString()
                return isElectron ?
                    `https://${CLOUD_DASHBOARD_DOMAIN}${REGISTRATION_PATH}?${newQuery}`
                  : `${REGISTRATION_PATH}?${newQuery}`
              })()}
              icon={CreateAccountIcon}
              text={getText('dontHaveAnAccount')}
            />
          )}
        </Form.FieldValue>
      }
    >
      <Stepper state={stepperState} renderStep={() => null}>
        <Stepper.StepContent index={0}>
          {() => (
            <div className="flex flex-col gap-auth">
              <Button size="large" variant="outline" icon={GOOGLE_ICON} onPress={handleGooglePress}>
                {getText('signUpOrLoginWithGoogle')}
              </Button>
              <Button size="large" variant="outline" icon={GITHUB_ICON} onPress={handleGitHubPress}>
                {getText('signUpOrLoginWithGitHub')}
              </Button>

              <Form form={form} gap="medium">
                <Input
                  form={form}
                  autoFocus
                  required
                  data-testid="email-input"
                  name="email"
                  label={getText('email')}
                  type="email"
                  autoComplete="email"
                  icon={AtIcon}
                  placeholder={getText('emailPlaceholder')}
                />

                <div className="flex w-full flex-col">
                  <Password
                    form={form}
                    required
                    data-testid="password-input"
                    name="password"
                    label={getText('password')}
                    autoComplete="current-password"
                    icon={LockIcon}
                    placeholder={getText('passwordPlaceholder')}
                  />

                  <Form.FieldValue form={form} name="email">
                    {(email) => (
                      <Button
                        variant="link"
                        href={`${FORGOT_PASSWORD_PATH}?${new URLSearchParams({ email }).toString()}`}
                        size="small"
                        className="self-end"
                      >
                        {getText('forgotYourPassword')}
                      </Button>
                    )}
                  </Form.FieldValue>
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
                  const res = await confirmSignIn(user, otp)

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
