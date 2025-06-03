/**
 * @file Container responsible for rendering and interactions in second half of forgot password
 * flow.
 */
import { LOGIN_PATH } from '#/appUtils'
import GoBackIcon from '#/assets/go_back.svg'
import LockIcon from '#/assets/lock.svg'
import { Button } from '#/components/Button'
import { Form } from '#/components/Form'
import { Input, Password } from '#/components/Inputs'
import Link from '#/components/Link'
import { Result } from '#/components/Result'
import { Stepper } from '#/components/Stepper'
import { useMount } from '#/hooks/mountHooks'
import { useTimeoutAPI } from '#/hooks/timeoutHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import AuthenticationPage from '#/pages/authentication/AuthenticationPage'
import { passwordWithPatternSchema } from '#/pages/authentication/schemas'
import { useSessionAPI } from '#/providers/SessionProvider'
import { noop } from '#/utilities/functions'
import { PASSWORD_REGEX } from '#/utilities/validation'
import { unsafeWriteValue } from '#/utilities/write'
import { useBackends, useRouter, useText } from '$/providers/react'
import { type GetText } from '$/providers/text'
import { toast } from 'react-toastify'
import * as z from 'zod'

/** Create the schema for this form. */
function createResetPasswordFormSchema(getText: GetText) {
  return z
    .object({
      email: z.string().email(getText('invalidEmailValidationError')),
      verificationCode: z.string(),
      newPassword: passwordWithPatternSchema(getText),
      confirmNewPassword: z.string().trim(),
    })
    .superRefine((object, context) => {
      if (
        PASSWORD_REGEX.test(object.newPassword) &&
        object.newPassword !== object.confirmNewPassword
      ) {
        context.addIssue({
          path: ['confirmNewPassword'],
          code: 'custom',
          message: getText('passwordMismatchError'),
        })
      }
    })
}

const REDIRECT_TIMEOUT = 3000

/** A form for users to reset their password. */
export default function ResetPassword() {
  const { resetPassword } = useSessionAPI()
  const { getText } = useText()
  const { router, searchParams } = useRouter()

  const toastAndLog = useToastAndLog()
  const { localBackend } = useBackends()
  const supportsOffline = localBackend != null

  const defaultEmail = searchParams.get('email')
  const defaultVerificationCode = searchParams.get('verification_code')
  const redirectUrl = searchParams.get('redirect_url') ?? 'enso://auth/login'

  const { startTimer } = useTimeoutAPI({ ms: REDIRECT_TIMEOUT })

  useMount(() => {
    if (defaultEmail == null) {
      toastAndLog('missingEmailError')
      void router.push(LOGIN_PATH)
    }

    if (defaultVerificationCode == null) {
      toastAndLog('missingVerificationCodeError')
      void router.push(LOGIN_PATH)
    }
  })

  const { stepperState } = Stepper.useStepperState({ steps: 2, defaultStep: 0 })

  return (
    <AuthenticationPage
      supportsOffline={supportsOffline}
      title={getText('resetYourPassword')}
      schema={createResetPasswordFormSchema(getText)}
      footer={
        <Link
          to={`${LOGIN_PATH}?${new URLSearchParams({ email: defaultEmail ?? '' }).toString()}`}
          icon={GoBackIcon}
          text={getText('goBackToLogin')}
        />
      }
      onSubmit={({ email, verificationCode, newPassword }) =>
        resetPassword(email, verificationCode, newPassword).then(() => {
          toast.success(getText('resetPasswordSuccess'))

          stepperState.nextStep()

          void startTimer()
            .then(() => {
              unsafeWriteValue(window.location, 'href', redirectUrl)
            })
            .catch(noop)
        })
      }
    >
      <Stepper state={stepperState}>
        <Stepper.StepContent index={0}>
          <Input
            required
            readOnly
            hidden
            data-testid="email-input"
            name="email"
            type="email"
            autoComplete="email"
            placeholder={getText('emailPlaceholder')}
            value={defaultEmail ?? ''}
          />

          <Input
            required
            readOnly
            hidden
            data-testid="verification-code-input"
            name="verificationCode"
            type="text"
            autoComplete="one-time-code"
            placeholder={getText('confirmationCodePlaceholder')}
            value={defaultVerificationCode ?? ''}
          />

          <Password
            autoFocus
            required
            data-testid="new-password-input"
            name="newPassword"
            label={getText('newPasswordLabel')}
            autoComplete="new-password"
            icon={LockIcon}
            placeholder={getText('newPasswordPlaceholder')}
            description={getText('passwordValidationMessage')}
          />

          <Password
            required
            data-testid="confirm-new-password-input"
            name="confirmNewPassword"
            label={getText('confirmNewPasswordLabel')}
            autoComplete="new-password"
            icon={LockIcon}
            placeholder={getText('confirmNewPasswordPlaceholder')}
          />

          <Form.Submit size="large" icon="arrow_right" fullWidth>
            {getText('reset')}
          </Form.Submit>

          <Form.FormError />
        </Stepper.StepContent>

        <Stepper.StepContent index={1}>
          <Result
            title={getText('resetPasswordSuccess')}
            status="success"
            subtitle={getText('resetPasswordSuccessSubtitle')}
          >
            <Button.Group align="center">
              <Button href={redirectUrl} size="large" variant="submit" icon="arrow_right" fullWidth>
                {getText('openInDesktop')}
              </Button>
            </Button.Group>
          </Result>
        </Stepper.StepContent>
      </Stepper>
    </AuthenticationPage>
  )
}
