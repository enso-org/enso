/**
 * @file Container responsible for rendering and interactions in first half of forgot password
 * flow.
 */
import AtIcon from '#/assets/at.svg'
import GoBackIcon from '#/assets/go_back.svg'
import { Button } from '#/components/Button'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs/Input'
import Link from '#/components/Link'
import AuthenticationPage from '#/pages/authentication/AuthenticationPage'
import { LOGIN_PATH } from '$/appUtils'
import { useBackends, useRouter, useSession, useText } from '$/providers/react'
import { useQueryParam } from '$/providers/react/queryParams'
import { useState } from 'react'
import { toast } from 'react-toastify'

/** A form for users to request for their password to be reset. */
export default function ForgotPassword() {
  const [resendConfirmationButtonVisible, setResendConfirmationButtonVisible] = useState(false)
  const { forgotPassword, resendSignUp } = useSession()
  const { getText } = useText()

  const { router } = useRouter()

  const { localBackend } = useBackends()
  const supportsOffline = localBackend != null

  const [initialEmail] = useQueryParam('email')
  const [emailInput, setEmailInput] = useState(initialEmail ?? '')

  return (
    <AuthenticationPage
      title={getText('forgotYourPassword')}
      schema={(z) => z.object({ email: z.string().email() })}
      footer={
        <Link
          to={`${LOGIN_PATH}?${new URLSearchParams({ email: emailInput }).toString()}`}
          icon={GoBackIcon}
          text={getText('goBackToLogin')}
        />
      }
      supportsOffline={supportsOffline}
      onSubmit={async ({ email }) => {
        try {
          setResendConfirmationButtonVisible(false)
          await forgotPassword(email)
          void router.push(LOGIN_PATH)
          toast.success(getText('forgotPasswordSuccess'))
        } catch (error) {
          // eslint-disable-next-line no-restricted-syntax
          const safeError = error as Error
          const isUserNotConfirmed = /verify your email first/.test(safeError.message)

          if (isUserNotConfirmed) {
            setResendConfirmationButtonVisible(true)
          }
          throw error
        }
      }}
    >
      <Input
        autoFocus
        required
        data-testid="email-input"
        name="email"
        label={getText('emailLabel')}
        type="email"
        autoComplete="email"
        icon={AtIcon}
        placeholder={getText('emailPlaceholder')}
        defaultValue={initialEmail ?? undefined}
        onChange={(event) => {
          setEmailInput(event.currentTarget.value)
        }}
      />

      <Form.Submit size="large" icon="arrow_right" iconPosition="end" fullWidth>
        {getText('sendLink')}
      </Form.Submit>

      <Form.FormError />

      {resendConfirmationButtonVisible && (
        <Button
          variant="submit"
          size="large"
          fullWidth
          onPress={async () => {
            await resendSignUp(emailInput)
          }}
        >
          {getText('resendConfirmRegistrationEmail')}
        </Button>
      )}
    </AuthenticationPage>
  )
}
