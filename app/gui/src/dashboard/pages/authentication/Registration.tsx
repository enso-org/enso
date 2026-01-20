/** @file Registration container responsible for rendering and interactions in sign up flow. */
import AtIcon from '#/assets/at.svg'
import GoBackIcon from '#/assets/go_back.svg'
import LockIcon from '#/assets/lock.svg'
import { Alert } from '#/components/Alert'
import { Button } from '#/components/Button'
import { Checkbox } from '#/components/Checkbox'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import { Password } from '#/components/Inputs/Password'
import Link from '#/components/Link'
import { Stepper, useStepperState } from '#/components/Stepper'
import { Text } from '#/components/Text'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import AuthenticationPage from '#/pages/authentication/AuthenticationPage'
import { passwordWithPatternSchema } from '#/pages/authentication/schemas'
import { DASHBOARD_PATH, LOGIN_PATH } from '$/appUtils'
import { useAuth } from '$/providers/auth'
import { useBackends, useLocalStorage, useRouter, useSession, useText } from '$/providers/react'
import { useQueryParam } from '$/providers/react/queryParams'
import { useEffect, useState } from 'react'

const CONFIRM_SIGN_IN_INTERVAL = 5_000

/** Properties of {@link Registration} component. */
export interface RegistrationProps {
  /** Called when the user agrees to the current Terms of Service and Privacy Policy. */
  readonly userAgreed: () => void
}

/** A form for users to register an account. */
export default function Registration(props: RegistrationProps) {
  const { userAgreed } = props
  const { signUp, confirmSignUp, signInWithPassword } = useSession()

  const { router } = useRouter()
  const localStorage = useLocalStorage()
  const { getText } = useText()
  const { localBackend } = useBackends()
  const { refetchSession } = useAuth()
  const supportsOffline = localBackend != null

  const [initialEmail] = useQueryParam('email')
  const [organizationId] = useQueryParam('organization_id')
  const [redirectTo] = useQueryParam('redirect_to')
  const [isManualCodeEntry, setIsManualCodeEntry] = useState(false)

  const signupForm = Form.useForm({
    defaultValues: { email: initialEmail ?? '', agreedToTos: [], agreedToPrivacyPolicy: [] },
    resetOnSubmit: false,
    schema: (schema) =>
      schema
        .object({
          email: schema.string().email(getText('invalidEmailValidationError')),
          password: passwordWithPatternSchema(getText),
          confirmPassword: schema.string(),
          agreedToTos: schema
            .array(schema.string())
            .min(1, { message: getText('licenseAgreementCheckboxError') }),
          agreedToPrivacyPolicy: schema
            .array(schema.string())
            .min(1, { message: getText('privacyPolicyCheckboxError') }),
        })
        .superRefine((object, context) => {
          if (object.password !== object.confirmPassword) {
            context.addIssue({
              path: ['confirmPassword'],
              code: 'custom',
              message: getText('passwordMismatchError'),
            })
          }
        }),
    onSubmit: async ({ email, password }) => {
      userAgreed()

      await signUp(email, password, organizationId ?? null)

      stepperState.nextStep()
    },
  })

  const { stepperState } = useStepperState({ steps: 2, defaultStep: 0 })

  useEffect(() => {
    if (redirectTo != null) {
      localStorage.set('loginRedirect', redirectTo)
    } else {
      localStorage.delete('loginRedirect')
    }
  }, [localStorage, redirectTo])

  const trySignIn = useEventCallback(() => {
    const email = signupForm.getValues('email')
    const password = signupForm.getValues('password')

    return signInWithPassword(email, password)
  })

  useEffect(() => {
    if (stepperState.currentStep === 1) {
      const interval = setInterval(() => {
        void trySignIn().catch(() => {})
      }, CONFIRM_SIGN_IN_INTERVAL)

      return () => {
        clearInterval(interval)
      }
    }
  }, [stepperState.currentStep, trySignIn])

  return (
    <AuthenticationPage
      supportsOffline={supportsOffline}
      footer={
        <Form.FieldValue form={signupForm} name="email">
          {(email) => (
            <Link
              to={LOGIN_PATH + `?${new URLSearchParams({ email }).toString()}`}
              icon={GoBackIcon}
              text={getText('alreadyHaveAnAccount')}
            />
          )}
        </Form.FieldValue>
      }
    >
      <Stepper state={stepperState} renderStep={() => null}>
        <Stepper.StepContent index={0}>
          {() => (
            <>
              <Text.Heading level={1} balance className="mb-4 text-center">
                {getText('createANewAccount')}
              </Text.Heading>

              <Form form={signupForm}>
                {({ form }) => (
                  <>
                    <Input
                      form={form}
                      autoFocus
                      required
                      testId="email-input"
                      name="email"
                      label={getText('emailLabel')}
                      type="email"
                      autoComplete="email"
                      icon={AtIcon}
                      placeholder={getText('emailPlaceholder')}
                    />

                    <Password
                      form={form}
                      required
                      testId="password-input"
                      name="password"
                      label={getText('passwordLabel')}
                      autoComplete="new-password"
                      icon={LockIcon}
                      placeholder={getText('passwordPlaceholder')}
                      description={getText('passwordValidationMessage')}
                    />

                    <Password
                      form={form}
                      required
                      testId="confirm-password-input"
                      name="confirmPassword"
                      label={getText('confirmPasswordLabel')}
                      autoComplete="new-password"
                      icon={LockIcon}
                      placeholder={getText('confirmPasswordPlaceholder')}
                    />

                    <Checkbox.Group
                      form={form}
                      name="agreedToTos"
                      description={
                        <Button variant="link" target="_blank" href={`${$config.ENSO_HOST}/eula`}>
                          {getText('viewLicenseAgreement')}
                        </Button>
                      }
                    >
                      <Checkbox value="agree">{getText('licenseAgreementCheckbox')}</Checkbox>
                    </Checkbox.Group>

                    <Checkbox.Group
                      name="agreedToPrivacyPolicy"
                      description={
                        <Button
                          variant="link"
                          target="_blank"
                          href={`${$config.ENSO_HOST}/privacy`}
                        >
                          {getText('viewPrivacyPolicy')}
                        </Button>
                      }
                    >
                      <Checkbox value="agree">{getText('privacyPolicyCheckbox')}</Checkbox>
                    </Checkbox.Group>

                    <Form.Submit size="large" icon="create_account" fullWidth>
                      {getText('register')}
                    </Form.Submit>

                    <Form.FormError />
                  </>
                )}
              </Form>
            </>
          )}
        </Stepper.StepContent>

        <Stepper.StepContent index={1}>
          {() => (
            <>
              <Text.Heading level={1} balance className="mb-4 text-center">
                {getText('confirmRegistration')}
              </Text.Heading>

              <div className="flex flex-col gap-4 text-start">
                <div className="flex flex-col">
                  <Text disableLineHeightCompensation>
                    {getText('confirmRegistrationInstruction')}
                  </Text>
                  <ul>
                    <li>
                      <Text disableLineHeightCompensation>
                        {getText('confirmRegistrationMethod1')}
                      </Text>
                    </li>
                    <li>
                      <Text disableLineHeightCompensation>
                        {getText('confirmRegistrationMethod2')}
                      </Text>
                    </li>
                  </ul>
                </div>

                <Alert variant="neutral">
                  <Text>{getText('confirmRegistrationSpam')}</Text>
                </Alert>

                {!isManualCodeEntry && (
                  <Button
                    variant="outline"
                    onPress={() => {
                      setIsManualCodeEntry(true)
                    }}
                  >
                    {getText('enterCodeManually')}
                  </Button>
                )}

                {isManualCodeEntry && (
                  <Form
                    schema={(schema) =>
                      schema.object({ verificationCode: Form.schema.string().min(1) })
                    }
                    onSubmit={async ({ verificationCode }) => {
                      const email = signupForm.getValues('email')
                      const password = signupForm.getValues('password')

                      await confirmSignUp(email, verificationCode)
                      await signInWithPassword(email, password)
                      while (true) {
                        if ((await refetchSession()).data) {
                          await router.push(DASHBOARD_PATH)
                          break
                        } else {
                          await new Promise((resolve) => {
                            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
                            window.setTimeout(resolve, 3_000)
                          })
                        }
                      }
                    }}
                  >
                    <Input
                      name="verificationCode"
                      label={getText('confirmRegistrationVerificationCodeLabel')}
                    />

                    <Form.Submit fullWidth />

                    <Form.FormError />
                  </Form>
                )}
              </div>
            </>
          )}
        </Stepper.StepContent>
      </Stepper>
    </AuthenticationPage>
  )
}
