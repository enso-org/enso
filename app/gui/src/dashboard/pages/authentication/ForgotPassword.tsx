/**
 * @file Container responsible for rendering and interactions in first half of forgot password
 * flow.
 */
import AtIcon from '#/assets/at.svg'
import GoBackIcon from '#/assets/go_back.svg'
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
  const { forgotPassword } = useSession()
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
      onSubmit={({ email }) =>
        forgotPassword(email).then(() => {
          void router.push(LOGIN_PATH)
          toast.success(getText('forgotPasswordSuccess'))
        })
      }
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
    </AuthenticationPage>
  )
}
