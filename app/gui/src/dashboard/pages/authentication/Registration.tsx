/** @file Registration container responsible for rendering and interactions in sign up flow. */
import { useEffect, useState } from 'react'
import { useLocation } from 'react-router-dom'

import * as z from 'zod'

import { LOGIN_PATH } from '#/appUtils'
import AtIcon from '#/assets/at.svg'
import CreateAccountIcon from '#/assets/create_account.svg'
import GoBackIcon from '#/assets/go_back.svg'
import LockIcon from '#/assets/lock.svg'
import { Alert, Button, Checkbox, Form, Input, Password, Text } from '#/components/AriaComponents'
import Link from '#/components/Link'
import { Stepper, useStepperState } from '#/components/Stepper'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import {
  latestPrivacyPolicyQueryOptions,
  latestTermsOfServiceQueryOptions,
} from '#/modals/AgreementsModal'
import AuthenticationPage from '#/pages/authentication/AuthenticationPage'
import { passwordWithPatternSchema } from '#/pages/authentication/schemas'
import { useAuth } from '#/providers/AuthProvider'
import { useLocalBackend } from '#/providers/BackendProvider'
import { useLocalStorage } from '#/providers/LocalStorageProvider'
import { useText } from '#/providers/TextProvider'
import LocalStorage from '#/utilities/LocalStorage'
import { useSuspenseQuery } from '@tanstack/react-query'

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
  schema: z.string(),
})

const CONFIRM_SIGN_IN_INTERVAL = 5_000

// ====================
// === Registration ===
// ====================

/** A form for users to register an account. */
export default function Registration() {
  const { signUp, confirmSignUp, signInWithPassword } = useAuth()

  const location = useLocation()
  const { localStorage } = useLocalStorage()
  const { getText } = useText()
  const localBackend = useLocalBackend()
  const supportsOffline = localBackend != null

  const query = new URLSearchParams(location.search)
  const initialEmail = query.get('email') ?? ''
  const organizationId = query.get('organization_id')
  const redirectTo = query.get('redirect_to')
  const [isManualCodeEntry, setIsManualCodeEntry] = useState(false)
  const [emailInput, setEmailInput] = useState(initialEmail)

  const signupForm = Form.useForm({
    defaultValues: { email: initialEmail, agreedToTos: [], agreedToPrivacyPolicy: [] },
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
      localStorage.set('termsOfService', { versionHash: tosHash })
      localStorage.set('privacyPolicy', { versionHash: privacyPolicyHash })

      await signUp(email, password, organizationId)

      stepperState.nextStep()
    },
  })

  const { stepperState } = useStepperState({ steps: 2, defaultStep: 0 })

  const cachedTosHash = localStorage.get('termsOfService')?.versionHash
  const { data: tosHash } = useSuspenseQuery({
    ...latestTermsOfServiceQueryOptions,
    // If the user has already accepted the EULA, we don't need to
    // block user interaction with the app while we fetch the latest version.
    // We can use the local version hash as the initial data.
    // and refetch in the background to check for updates.
    ...(cachedTosHash != null && {
      initialData: { hash: cachedTosHash },
    }),
    select: (data) => data.hash,
  })
  const cachedPrivacyPolicyHash = localStorage.get('privacyPolicy')?.versionHash
  const { data: privacyPolicyHash } = useSuspenseQuery({
    ...latestPrivacyPolicyQueryOptions,
    ...(cachedPrivacyPolicyHash != null && {
      initialData: { hash: cachedPrivacyPolicyHash },
    }),
    select: (data) => data.hash,
  })

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
    } else {
      return
    }
  }, [stepperState.currentStep, trySignIn])

  return (
    <AuthenticationPage
      supportsOffline={supportsOffline}
      footer={
        <Link
          to={LOGIN_PATH + `?${new URLSearchParams({ email: emailInput }).toString()}`}
          icon={GoBackIcon}
          text={getText('alreadyHaveAnAccount')}
        />
      }
    >
      <Stepper state={stepperState} renderStep={() => null}>
        {stepperState.currentStep === 0 && (
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
                    onChange={(event) => {
                      setEmailInput(event.target.value)
                    }}
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
                      <Button variant="link" target="_blank" href="https://ensoanalytics.com/eula">
                        {getText('viewLicenseAgreement')}
                      </Button>
                    }
                  >
                    <Checkbox value="agree">{getText('licenseAgreementCheckbox')}</Checkbox>
                  </Checkbox.Group>

                  <Checkbox.Group
                    form={form}
                    name="agreedToPrivacyPolicy"
                    description={
                      <Button
                        variant="link"
                        target="_blank"
                        href="https://ensoanalytics.com/privacy"
                      >
                        {getText('viewPrivacyPolicy')}
                      </Button>
                    }
                  >
                    <Checkbox value="agree">{getText('privacyPolicyCheckbox')}</Checkbox>
                  </Checkbox.Group>

                  <Form.Submit size="large" icon={CreateAccountIcon} fullWidth>
                    {getText('register')}
                  </Form.Submit>

                  <Form.FormError />
                </>
              )}
            </Form>
          </>
        )}
        {stepperState.currentStep === 1 && (
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

                    return confirmSignUp(email, verificationCode).then(() =>
                      signInWithPassword(email, password),
                    )
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
      </Stepper>
    </AuthenticationPage>
  )
}
