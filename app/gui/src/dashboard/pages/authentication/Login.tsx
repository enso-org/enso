/** @file Login component responsible for rendering and interactions in sign in flow. */
import AtIcon from '#/assets/at.svg'
import CreateAccountIcon from '#/assets/create_account.svg'
import LockIcon from '#/assets/lock.svg'
import { Button } from '#/components/Button'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { OTPInput } from '#/components/Inputs/OTPInput'
import { Password } from '#/components/Inputs/Password'
import Link from '#/components/Link'
import { Stepper } from '#/components/Stepper'
import { Text } from '#/components/Text'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import AuthenticationPage from '#/pages/authentication/AuthenticationPage'
import { passwordSchema } from '#/pages/authentication/schemas'
import { DASHBOARD_PATH, FORGOT_PASSWORD_PATH, REGISTRATION_PATH } from '$/appUtils'
import type { CognitoUser } from '$/authentication/cognito'
import { useRouter, useSession, useText } from '$/providers/react'
import { useQueryParam } from '$/providers/react/queryParams'
import { isOnElectron } from 'enso-common/src/utilities/detect'
import { useState } from 'react'

/** A form for users to log in. */
export default function Login() {
  const { router } = useRouter()
  const {
    signInWithMicrosoft,
    signInWithApple,
    signInWithGoogle,
    signInWithGitHub,
    signInWithPassword,
    confirmSignIn,
  } = useSession()
  const { getText } = useText()
  const [initialEmail] = useQueryParam('email')

  const form = Form.useForm({
    schema: (z) =>
      z.object({
        email: z
          .string()
          .min(1, getText('arbitraryFieldRequired'))
          .email(getText('invalidEmailValidationError')),
        password: passwordSchema(getText),
      }),
    defaultValues: { email: initialEmail ?? '' },
    onSubmit: async ({ email, password }) => {
      // This is special case, needed by package testing. See app/electron-client/tests/electronTest.ts.
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-member-access, no-restricted-syntax, @typescript-eslint/no-explicit-any
      const passwordOverride: string = (window as any).passwordOverride
      const { user, challenge } = await signInWithPassword(
        email,
        passwordOverride ? passwordOverride : password,
      )

      if (challenge) {
        setUser(user)
        nextStep()
      } else {
        await router.push(DASHBOARD_PATH)
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

  const handleMicrosoftPress = useEventCallback(async () => {
    await signInWithMicrosoft()
  })

  const handleApplePress = useEventCallback(async () => {
    await signInWithApple()
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
              to={`${REGISTRATION_PATH}?${new URLSearchParams({ email }).toString()}`}
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
              <Button
                size="large"
                variant="outline"
                icon="google_color"
                onPress={handleGooglePress}
              >
                {getText('signUpOrLoginWithGoogle')}
              </Button>
              <Button
                size="large"
                variant="outline"
                icon="github_color"
                onPress={handleGitHubPress}
              >
                {getText('signUpOrLoginWithGitHub')}
              </Button>
              <Button
                size="large"
                variant="outline"
                icon="microsoft_color"
                onPress={handleMicrosoftPress}
              >
                {getText('signUpOrLoginWithMicrosoft')}
              </Button>
              <Button size="large" variant="outline" icon="apple_color" onPress={handleApplePress}>
                {getText('signUpOrLoginWithApple')}
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

                <Form.Submit size="large" icon="arrow_right" iconPosition="end" fullWidth>
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
                    await router.push(DASHBOARD_PATH)
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

              <Form.Submit size="large" icon="arrow_right" iconPosition="end" fullWidth>
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
