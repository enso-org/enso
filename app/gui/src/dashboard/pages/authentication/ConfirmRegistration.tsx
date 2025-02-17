/**
 * @file Registration confirmation page for when a user clicks the confirmation link set to their
 * email address.
 */

import * as router from 'react-router-dom'

import * as appUtils from '#/appUtils'

import { Result } from '#/components/Result'

import { Button } from '#/components/AriaComponents'
import { useMounted } from '#/hooks/mountHooks'
import { useTimeoutAPI } from '#/hooks/timeoutHooks'
import { useSessionAPI } from '#/providers/SessionProvider'
import { useText } from '#/providers/TextProvider'
import { noop } from '#/utilities/functions'
import { unsafeWriteValue } from '#/utilities/write'
import { useMutation } from '@tanstack/react-query'
import AuthenticationPage from './AuthenticationPage'

// ===========================
// === ConfirmRegistration ===
// ===========================

const REDIRECT_TIMEOUT = 5_000

/** An empty component redirecting users based on the backend response to user registration. */
export default function ConfirmRegistration() {
  const { confirmSignUp } = useSessionAPI()
  const { getText } = useText()

  const [searchParams] = router.useSearchParams()

  const email = searchParams.get('email')
  const verificationCode = searchParams.get('verification_code')
  const redirectUrl = searchParams.get('redirect_url')

  const { startTimer } = useTimeoutAPI({ ms: REDIRECT_TIMEOUT })

  const url = (() => {
    if (redirectUrl != null) {
      return redirectUrl
    }

    searchParams.delete('verification_code')
    searchParams.delete('email')
    searchParams.delete('redirect_url')

    return appUtils.SETUP_PATH + '?' + searchParams.toString()
  })()

  const confirmRegistrationMutation = useMutation({
    mutationKey: ['confirmRegistration'],
    mutationFn: (params: { email: string; verificationCode: string }) =>
      confirmSignUp(params.email, params.verificationCode),
    onSuccess: () => {
      void startTimer()
        .then(() => {
          unsafeWriteValue(window.location, 'href', url)
        })
        .catch(noop)
    },
  })

  useMounted(() => {
    if (
      email != null &&
      verificationCode != null &&
      confirmRegistrationMutation.status === 'idle'
    ) {
      confirmRegistrationMutation.mutate({ email, verificationCode })
    }
  })

  const textsByStatus: Record<
    typeof confirmRegistrationMutation.status,
    { title: string; subtitle: string }
  > = {
    pending: {
      title: getText('confirmRegistrationTitlePending'),
      subtitle: getText('confirmRegistrationSubtitlePending'),
    },
    error: {
      title: getText('confirmRegistrationTitleError'),
      subtitle: getText('confirmRegistrationSubtitleError'),
    },
    success: {
      title: getText('confirmRegistrationTitleSuccess'),
      subtitle: getText('confirmRegistrationSubtitleSuccess'),
    },
    idle: {
      title: getText('confirmRegistrationTitleIdle'),
      subtitle: getText('confirmRegistrationSubtitleIdle'),
    },
  }

  if (email == null || verificationCode == null) {
    return <router.Navigate to={appUtils.LOGIN_PATH} replace />
  }

  return (
    <AuthenticationPage title={''}>
      <Result
        status={confirmRegistrationMutation.status}
        title={textsByStatus[confirmRegistrationMutation.status].title}
        subtitle={textsByStatus[confirmRegistrationMutation.status].subtitle}
      >
        <Button.Group align="center" buttonVariants={{ variant: 'submit' }}>
          {confirmRegistrationMutation.isIdle && (
            <Button
              onPress={() => confirmRegistrationMutation.mutateAsync({ email, verificationCode })}
            >
              {getText('confirm')}
            </Button>
          )}

          {confirmRegistrationMutation.isError && (
            <Button
              onPress={() => confirmRegistrationMutation.mutateAsync({ email, verificationCode })}
            >
              {getText('retry')}
            </Button>
          )}

          {confirmRegistrationMutation.isSuccess && (
            <Button href={url}>{getText('openInDesktop')}</Button>
          )}
        </Button.Group>
      </Result>
    </AuthenticationPage>
  )
}
