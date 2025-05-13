/**
 * @file Provider for the {@link SessionContextType}, which contains information about the
 * currently authenticated user's session.
 */
import * as React from 'react'

import * as sentry from '@sentry/vue'
import * as reactQuery from '@tanstack/react-query'

import * as httpClientProvider from '#/providers/HttpClientProvider'

import * as errorModule from '#/utilities/error'

import type * as cognito from '#/authentication/cognito'
import { CognitoErrorType, type CognitoUser, type ISessionProvider } from '#/authentication/cognito'
import * as listen from '#/authentication/listen'
import { Dialog } from '#/components/AriaComponents'
import { Result } from '#/components/Result'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import * as gtag from '#/hooks/gtagHooks'
import { useOffline } from '#/hooks/offlineHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { unsetModal } from '#/providers/ModalProvider'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { unsafeWriteValue } from '#/utilities/write'
import { toast } from 'react-toastify'
import { useText } from '../TextProvider'
import { SessionContext } from './hooks'
import type { SessionContextType, SessionProviderProps } from './types'

/** Create a query for the user session. */
function createSessionQuery(authService: ISessionProvider) {
  return reactQuery.queryOptions({
    queryKey: ['userSession'],
    queryFn: () => authService.userSession().catch(() => null),
  })
}

/** A React provider for the session of the authenticated user. */
export function SessionProvider(props: SessionProviderProps) {
  const { mainPageUrl, children, registerAuthEventListener, authService, onLogout } = props

  const { getText } = useText()

  const [isLoggingOut, setIsLoggingOut] = React.useState(false)

  // stabilize the callback so that it doesn't change on every render
  const saveAccessTokenEventCallback = useEventCallback((accessToken: cognito.UserSession) => {
    authService.saveAccessToken(accessToken)
  })

  const httpClient = httpClientProvider.useHttpClient()
  const queryClient = reactQuery.useQueryClient()
  const toastAndLog = useToastAndLog()

  const sessionQueryOptions = createSessionQuery(authService)

  const session = reactQuery.useSuspenseQuery(sessionQueryOptions)

  const refreshUserSessionMutation = useMutationCallback({
    mutationKey: ['refreshUserSession', { expireAt: session.data?.expireAt }],
    mutationFn: async () => authService.refreshUserSession(),
    onSuccess: (data) => {
      if (data) {
        httpClient.setSessionToken(data.accessToken)
      }
      return queryClient.invalidateQueries({ queryKey: sessionQueryOptions.queryKey })
    },
    onError: (error) => {
      // Something went wrong with the refresh token, so we need to sign the user out.
      toastAndLog('sessionExpiredError', error)
      queryClient.setQueryData(sessionQueryOptions.queryKey, null)
      return logoutMutation()
    },
  })

  const logoutMutation = useMutationCallback({
    mutationKey: ['session', 'logout', session.data?.clientId] as const,
    mutationFn: async () => {
      setIsLoggingOut(true)
      await authService.signOut()

      gtag.event('cloud_sign_out')
      const parentDomain = location.hostname.replace(/^[^.]*\./, '')
      unsafeWriteValue(document, 'cookie', `logged_in=no;max-age=0;domain=${parentDomain}`)

      authService.saveAccessToken(null)
      setIsLoggingOut(false)
    },
    // If the User Menu is still visible, it breaks when `userSession` is set to `null`.
    onMutate: unsetModal,
    onSuccess: async () => {
      await onLogout?.()

      sentry.setUser(null)
      toast.success(getText('signOutSuccess'))

      // On sign out, we need to clear the query client.
      // But we dont want to delay the logoutMutation to avoid possible side effects,
      // like refetching some data. By the moment of clearing the query client,
      // the logoutMutation is already resolved, and user is navigated to the login page.
      void queryClient.clearWithPersister()
    },
    onError: () => toast.error(getText('signOutError')),
    meta: { invalidates: [sessionQueryOptions.queryKey], awaitInvalidates: true },
  })

  const signUp = useEventCallback(
    async (username: string, password: string, organizationId: string | null) => {
      gtag.event('cloud_sign_up')
      const result = await authService.signUp(username, password, organizationId)

      if (result.err) {
        throw new Error(result.val.message)
      } else {
        return
      }
    },
  )

  const confirmSignUp = useEventCallback(async (email: string, code: string) => {
    gtag.event('cloud_confirm_sign_up')
    const result = await authService.confirmSignUp(email, code)

    if (result.err) {
      switch (result.val.type) {
        case CognitoErrorType.userAlreadyConfirmed:
        case CognitoErrorType.userNotFound: {
          return
        }
        default: {
          throw new errorModule.UnreachableCaseError(result.val.type)
        }
      }
    }
  })

  const signInWithPassword = useEventCallback(async (email: string, password: string) => {
    gtag.event('cloud_sign_in', { provider: 'Email' })

    const result = await authService.signInWithPassword(email, password)

    if (result.ok) {
      const user = result.unwrap()

      const challenge = user.challengeName ?? 'NO_CHALLENGE'

      if (['SMS_MFA', 'SOFTWARE_TOKEN_MFA'].includes(challenge)) {
        return { challenge, user } as const
      }

      return queryClient
        .invalidateQueries({ queryKey: sessionQueryOptions.queryKey })
        .then(() => ({ challenge, user }) as const)
    } else {
      throw new Error(result.val.message)
    }
  })

  const signInWithGoogle = useEventCallback(() => {
    gtag.event('cloud_sign_in', { provider: 'Google' })

    return authService.signInWithGoogle().then(
      () => true,
      () => false,
    )
  })

  const signInWithGitHub = useEventCallback(() => {
    gtag.event('cloud_sign_in', { provider: 'GitHub' })

    return authService.signInWithGitHub().then(
      () => true,
      () => false,
    )
  })

  const confirmSignIn = useEventCallback((user: CognitoUser, otp: string) =>
    authService.confirmSignIn(user, otp, 'SOFTWARE_TOKEN_MFA'),
  )

  const forgotPassword = useEventCallback(async (email: string) => {
    const result = await authService.forgotPassword(email)
    if (result.ok) {
      return null
    } else {
      throw new Error(result.val.message)
    }
  })

  const resetPassword = useEventCallback(async (email: string, code: string, password: string) => {
    const result = await authService.forgotPasswordSubmit(email, code, password)

    if (result.ok) {
      return null
    } else {
      throw new Error(result.val.message)
    }
  })

  const changePassword = useEventCallback(async (oldPassword: string, newPassword: string) => {
    const result = await authService.changePassword(oldPassword, newPassword)

    if (result.err) {
      throw new Error(result.val.message)
    }

    return result.ok
  })

  if (session.data) {
    httpClient.setSessionToken(session.data.accessToken)
  }

  // Register an effect that will listen for authentication events. When the event occurs, we
  // will refresh or clear the user's session, forcing a re-render of the page with the new
  // session.
  // For example, if a user clicks the "sign out" button, this will clear the user's session, which
  // means the login screen (which is a child of this provider) should render.
  React.useEffect(
    () =>
      registerAuthEventListener?.((event) => {
        switch (event) {
          case listen.AuthEvent.signIn:
          case listen.AuthEvent.signOut: {
            void queryClient.invalidateQueries({ queryKey: sessionQueryOptions.queryKey })
            break
          }
          case listen.AuthEvent.customOAuthState:
          case listen.AuthEvent.cognitoHostedUi: {
            // AWS Amplify doesn't provide a way to set the redirect URL for the OAuth flow, so
            // we have to hack it by replacing the URL in the browser's history. This is done
            // because otherwise the user will be redirected to a URL like `enso://auth`, which
            // will not work.
            // See https://github.com/aws-amplify/amplify-js/issues/3391#issuecomment-756473970
            history.replaceState({}, '', mainPageUrl)
            void queryClient.invalidateQueries({ queryKey: sessionQueryOptions.queryKey })
            break
          }
          default: {
            throw new errorModule.UnreachableCaseError(event)
          }
        }
      }),
    [registerAuthEventListener, mainPageUrl, queryClient, sessionQueryOptions.queryKey],
  )

  const organizationId = useEventCallback(() => authService.organizationId())

  const getMFAPreference = useEventCallback(async () => {
    const result = await authService.getMFAPreference()
    if (result.err) {
      throw result.val
    } else {
      return result.unwrap()
    }
  })

  const updateMFAPreference = useEventCallback(async (mfaType: cognito.MfaType) => {
    const result = await authService.updateMFAPreference(mfaType)

    if (result.err) {
      throw result.val
    }
  })

  const verifyTotpToken = useEventCallback(async (otp: string) => {
    const result = await authService.verifyTotpToken(otp)
    if (result.err) {
      throw result.val
    } else {
      return result.unwrap()
    }
  })

  const setupTOTP = useEventCallback(async () => {
    const result = await authService.setupTOTP()
    if (result.err) {
      throw result.val
    } else {
      return result.unwrap()
    }
  })

  React.useEffect(() => {
    if (session.data) {
      // Save access token so can it be reused by backend services
      saveAccessTokenEventCallback(session.data)
    }
  }, [session.data, saveAccessTokenEventCallback])

  const sessionContextValue = {
    signUp,
    session: session.data,
    confirmSignUp,
    signInWithPassword,
    signInWithGitHub,
    signInWithGoogle,
    confirmSignIn,
    forgotPassword,
    resetPassword,
    changePassword,
    signOut: logoutMutation,
    organizationId,
    getMFAPreference,
    updateMFAPreference,
    verifyTotpToken,
    setupTOTP,
  } satisfies SessionContextType

  return (
    <SessionContext.Provider value={sessionContextValue}>
      {typeof children === 'function' ? children(sessionContextValue) : children}

      {session.data && (
        <SessionRefresher session={session.data} refreshUserSession={refreshUserSessionMutation} />
      )}

      <Dialog
        aria-label={getText('loggingOut')}
        isDismissable={false}
        isKeyboardDismissDisabled
        hideCloseButton
        modalProps={{ isOpen: isLoggingOut }}
      >
        <Result status="loading" title={getText('loggingOut')} />
      </Dialog>
    </SessionContext.Provider>
  )
}

/** Props for a {@link SessionRefresher}. */
interface SessionRefresherProps {
  readonly session: cognito.UserSession
  readonly refreshUserSession: () => Promise<cognito.UserSession | null>
}

const TEN_SECONDS_MS = 10_000
const SIX_HOURS_MS = 21_600_000

/**
 * A component that will refresh the user's session at a given interval.
 */
function SessionRefresher(props: SessionRefresherProps) {
  const { refreshUserSession, session } = props

  const { isOffline } = useOffline()

  reactQuery.useQuery({
    queryKey: ['refreshUserSession', { refreshToken: session.refreshToken }] as const,
    queryFn: () => refreshUserSession(),
    meta: { persist: false },
    networkMode: 'online',
    initialData: session,
    initialDataUpdatedAt: Date.now(),
    refetchIntervalInBackground: true,
    refetchOnWindowFocus: 'always',
    refetchOnReconnect: 'always',
    refetchOnMount: 'always',
    enabled: !isOffline,
    refetchInterval: () => {
      const expireAt = session.expireAt

      const timeUntilRefresh =
        // If the session has not expired, we should refresh it when it is 5 minutes from expiring.
        // We use 1 second to ensure that we refresh even if the time is very close to expiring
        // and value won't be less than 0.
        Math.max(new Date(expireAt).getTime() - Date.now() - TEN_SECONDS_MS, TEN_SECONDS_MS)

      return timeUntilRefresh < SIX_HOURS_MS ? timeUntilRefresh : SIX_HOURS_MS
    },
  })

  return null
}
