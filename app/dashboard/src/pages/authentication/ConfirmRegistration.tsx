/** @file Registration confirmation page for when a user clicks the confirmation link set to their
 * email address. */

import * as router from 'react-router-dom'

import * as appUtils from '#/appUtils'

import { Result } from '#/components/Result'

import { Button, ButtonGroup } from '#/components/AriaComponents'
import { useMount } from '#/hooks/mountHooks'
import * as authProvider from '#/providers/AuthProvider'
import { useText } from '#/providers/TextProvider'
import { useMutation } from '@tanstack/react-query'
import AuthenticationPage from './AuthenticationPage'

// ===========================
// === ConfirmRegistration ===
// ===========================

/** An empty component redirecting users based on the backend response to user registration. */
export default function ConfirmRegistration() {
  const auth = authProvider.useAuth()
  const { getText } = useText()

  const navigate = router.useNavigate()
  const [searchParams] = router.useSearchParams()

  const verificationCode = searchParams.get('verification_code')
  const email = searchParams.get('email')
  const redirectUrl = searchParams.get('redirect_url')

  const confirmRegistrationMutation = useMutation({
    mutationFn: (params: { email: string; verificationCode: string }) =>
      auth.confirmSignUp(params.email, params.verificationCode),

    onSuccess: () => {
      if (redirectUrl != null) {
        window.location.href = redirectUrl
      } else {
        navigate(appUtils.LOGIN_PATH + '?' + searchParams.toString())
      }
    },
  })

  useMount(() => {
    if (email == null || verificationCode == null) {
      navigate(appUtils.LOGIN_PATH)
    } else {
      confirmRegistrationMutation.mutate({ email, verificationCode })
    }
  })

  const textsByStatus: Record<
    typeof confirmRegistrationMutation.status,
    { title: string; subtitle: string }
  > = {
    pending: {
      title: getText('confirmRegistrationTitle'),
      subtitle: getText('confirmRegistrationSubtitle'),
    },
    error: {
      title: getText('confirmRegistrationTitle'),
      subtitle: getText('confirmRegistrationSubtitle'),
    },
    success: {
      title: getText('confirmRegistrationTitle'),
      subtitle: getText('confirmRegistrationSubtitle'),
    },
    idle: {
      title: getText('confirmRegistrationTitle'),
      subtitle: getText('confirmRegistrationSubtitle'),
    },
  }
  if (email == null || verificationCode == null) {
    return null
  } else {
    return (
      <AuthenticationPage title={''}>
        <Result
          status={confirmRegistrationMutation.status}
          title={textsByStatus[confirmRegistrationMutation.status].title}
          subtitle={textsByStatus[confirmRegistrationMutation.status].subtitle}
        >
          {confirmRegistrationMutation.isError && (
            <ButtonGroup align="center">
              <Button
                onPress={() => confirmRegistrationMutation.mutateAsync({ email, verificationCode })}
              >
                {getText('retry')}
              </Button>
            </ButtonGroup>
          )}
        </Result>
      </AuthenticationPage>
    )
  }
}
