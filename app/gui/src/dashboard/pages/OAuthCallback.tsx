/** @file OAuth callback page. */
import { DASHBOARD_PATH, LOGIN_PATH, OPEN_IDE_DEEPLINK } from '#/appUtils'
import { Button } from '#/components/Button'
import { DIALOG_BACKGROUND } from '#/components/Dialog'
import Page from '#/components/Page'
import { Result } from '#/components/Result'
import { Text } from '#/components/Text'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useTimeoutAPI } from '#/hooks/timeoutHooks'
import { useAuth } from '#/providers/AuthProvider'
import { noop } from '#/utilities/functions'
import { useEffect } from 'react'
import { z } from 'zod'
import { useRouter, useText } from '../../providers/react'
import AuthenticationPage from './authentication/AuthenticationPage'

const URL_SCHEMA = z.object({
  status: z.enum(['success', 'error']),
  error: z.string().or(z.undefined()).or(z.null()),
  service: z.string(),
})

const AUTO_NAVIGATE_TIMEOUT_MS = 5_000
const NAVIGATE_USER_TIMEOUT_MS = 1_000

/**
 * OAuth callback page.
 */
export function OAuthCallback() {
  const { getText } = useText()

  const { session } = useAuth()

  const { router, searchParams } = useRouter()

  const autoNavigateTimeout = useTimeoutAPI({ ms: AUTO_NAVIGATE_TIMEOUT_MS })
  const navigateUserTimeout = useTimeoutAPI({ ms: NAVIGATE_USER_TIMEOUT_MS })

  const isAuthenticated = session != null
  const ContainerComponent = isAuthenticated ? Page : AuthenticationPage

  const params = URL_SCHEMA.safeParse({
    status: searchParams.get('status'),
    error: searchParams.get('error'),
    service: searchParams.get('service'),
  })

  const { status, error, service } = params.data ?? {
    status: 'error',
    error: 'Invalid URL',
    service: 'unknown',
  }

  const openCredentialsParams = new URLSearchParams()
  if (status === 'error') {
    openCredentialsParams.set('credentialsModal_open', 'true')
    openCredentialsParams.set('credentialsModal_service', service)
  }

  const dashboardUrl = `${DASHBOARD_PATH}?${openCredentialsParams.toString()}`
  const deeplink = `${OPEN_IDE_DEEPLINK}?${openCredentialsParams.toString()}`

  const navigateUser = useEventCallback(() => {
    window.location.href = deeplink

    return navigateUserTimeout
      .startTimer()
      .then(() => router.push(dashboardUrl))
      .catch(noop)
  })

  useEffect(() => {
    if (status !== 'success') {
      return
    }

    void autoNavigateTimeout
      .startTimer()
      .then(() => navigateUser())
      .catch(noop)
  }, [autoNavigateTimeout, navigateUser, status])

  // If the URL is invalid, there's a chance that the user opened it by mistake.
  // In that case, redirect to the dashboard or login page depending on whether
  // they are authenticated.
  if (!params.success) {
    void router.replace({ path: isAuthenticated ? DASHBOARD_PATH : LOGIN_PATH, replace: true })
    return null
  }

  const textsByStatus: Record<
    z.infer<typeof URL_SCHEMA>['status'],
    { title: string; subtitle: string }
  > = {
    success: {
      title: getText('oauthCallbackSuccessTitle'),
      subtitle: getText('oauthCallbackSuccessDescription', service),
    },
    error: {
      title: getText('oauthCallbackErrorTitle'),
      subtitle: error ?? getText('oauthCallbackErrorDescription', service),
    },
  }

  return (
    <ContainerComponent>
      <div
        className={DIALOG_BACKGROUND({
          className:
            'm-auto flex w-1/3 min-w-[420px] max-w-[640px] flex-col items-center justify-center rounded-3xl bg-invert p-12',
        })}
      >
        <Text.Heading level={1} className="mb-6">
          {getText('oauthCallbackTitle')}
        </Text.Heading>

        <Result
          centered="horizontal"
          className="mb-6"
          status={status}
          title={textsByStatus[status].title}
          subtitle={textsByStatus[status].subtitle}
        >
          {status === 'success' && <Text align="center">{getText('redirectingToDashboard')}</Text>}

          <Button.Group align="center" className="mt-4">
            {status === 'error' && (
              <Button onPress={navigateUser} variant="submit" icon="restore" iconPosition="end">
                {getText('oauthCallbackStartOver')}
              </Button>
            )}

            {status === 'success' && (
              <Button onPress={navigateUser} variant="submit" icon="open" iconPosition="end">
                {getText('openDashboard')}
              </Button>
            )}
          </Button.Group>
        </Result>
      </div>
    </ContainerComponent>
  )
}
