import * as gtag from '#/hooks/gtagHooks'
import { unsetModal } from '#/providers/ModalProvider'
import { NotAuthorizedError } from '#/services/Backend'
import { unreachable } from '#/utilities/error'
import HttpClient from '#/utilities/HttpClient'
import { ALL_PATHS_REGEX } from '$/appUtils'
import * as cognito from '$/authentication/cognito'
import { AuthEvent } from '$/authentication/listen'
import { useInitAuthService } from '$/authentication/service'
import { useLocalStorageClass } from '$/stores/localStorage'
import { Opt } from '@/util/data/opt'
import { Err } from '@/util/data/result'
import { useToast } from '@/util/toast'
import * as sentry from '@sentry/vue'
import * as vueQuery from '@tanstack/vue-query'
import { createGlobalState } from '@vueuse/core'
import { IS_DEV_MODE, isOnElectron, isOnLinux } from 'enso-common/src/detect'
import { computed, onScopeDispose, proxyRefs, Ref, ref, watchEffect } from 'vue'
import { useHttpClient } from './httpClient'
import { TextStore, useText } from './text'

/** Create a query for the user session. */
export function createSessionQuery(authService: cognito.ISessionProvider) {
  return vueQuery.queryOptions({
    queryKey: ['userSession'],
    queryFn: () => authService.userSession().catch(() => null),
    meta: {
      persist: false,
    },
  })
}

/** Returns the URL to the main page. This is the current URL, with the current route removed. */
function getMainPageUrl() {
  const mainPageUrl = new URL(window.location.href)
  mainPageUrl.pathname = mainPageUrl.pathname.replace(ALL_PATHS_REGEX, '')
  return mainPageUrl
}

export type SessionStore = ReturnType<typeof useSession>
export const useSession = createGlobalState(() => {
  // -- these were parameters
  const httpClient: HttpClient = useHttpClient()
  const { getText }: TextStore = useText()
  const queryClient: vueQuery.QueryClient = vueQuery.useQueryClient()
  const localStorage = useLocalStorageClass()
  // ----

  const mainPageUrl = getMainPageUrl()
  const authService = useInitAuthService({
    supportsDeepLinks: !IS_DEV_MODE && !isOnLinux() && isOnElectron(),
  })
  const errorToast = useToast.error()
  const successToast = useToast.success()

  const isLoggingOut = ref(false)

  const sessionQueryOptions = createSessionQuery(authService.cognito)
  const session = vueQuery.useQuery(sessionQueryOptions)

  watchEffect(() => console.error('session', session.fetchStatus.value, session.data.value), {
    flush: 'sync',
  })

  const refreshUserSessionMutation = vueQuery.useMutation({
    mutationKey: computed(() => ['refreshUserSession', { expireAt: session.data.value?.expireAt }]),
    mutationFn: async () => authService.cognito.refreshUserSession(),
    onSuccess: (data) => {
      if (data) {
        httpClient.setSessionToken(data.accessToken)
      }
      return queryClient.invalidateQueries({ queryKey: sessionQueryOptions.queryKey })
    },
    onError: (error) => {
      // Something went wrong with the refresh token, so we need to sign the user out.
      errorToast.reportError(Err(error).error, getText('sessionExpiredError'))
      queryClient.setQueryData(sessionQueryOptions.queryKey, null)
      return logoutMutation.mutate()
    },
  })

  const logoutMutation = vueQuery.useMutation({
    mutationKey: computed(() => ['session', 'logout', session.data.value?.clientId] as const),
    mutationFn: async () => {
      isLoggingOut.value = true
      await authService.cognito.signOut()

      gtag.event('cloud_sign_out')
      const parentDomain = location.hostname.replace(/^[^.]*\./, '')
      document.cookie = `logged_in=no;max-age=0;domain=${parentDomain}`

      authService.cognito.saveAccessToken(null)
      isLoggingOut.value = false
    },
    // If the User Menu is still visible, it breaks when `userSession` is set to `null`.
    onMutate: unsetModal,
    onSuccess: async () => {
      localStorage.clearUserSpecificEntries()
      sentry.setUser(null)
      successToast.show(getText('signOutSuccess'))
    },
    onError: () => errorToast.show(getText('signOutError')),
    meta: { invalidates: [sessionQueryOptions.queryKey], awaitInvalidates: true },
  })

  const signUp = async (username: string, password: string, organizationId: string | null) => {
    gtag.event('cloud_sign_up')
    const result = await authService.cognito.signUp(username, password, organizationId)

    if (result.err) {
      throw new Error(result.val.message)
    } else {
      return
    }
  }

  const confirmSignUp = async (email: string, code: string) => {
    gtag.event('cloud_confirm_sign_up')
    const result = await authService.cognito.confirmSignUp(email, code)

    if (result.err) {
      switch (result.val.type) {
        case cognito.CognitoErrorType.userAlreadyConfirmed:
        case cognito.CognitoErrorType.userNotFound: {
          return
        }
        default: {
          unreachable(result.val.type)
        }
      }
    }
  }

  const signInWithPassword = async (email: string, password: string) => {
    gtag.event('cloud_sign_in', { provider: 'Email' })

    const result = await authService.cognito.signInWithPassword(email, password)

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
  }

  const signInWithGoogle = () => {
    gtag.event('cloud_sign_in', { provider: 'Google' })

    return authService.cognito.signInWithGoogle().then(
      () => true,
      () => false,
    )
  }

  const signInWithGitHub = () => {
    gtag.event('cloud_sign_in', { provider: 'GitHub' })

    return authService.cognito.signInWithGitHub().then(
      () => true,
      () => false,
    )
  }

  const confirmSignIn = (user: cognito.CognitoUser, otp: string) =>
    authService.cognito.confirmSignIn(user, otp, 'SOFTWARE_TOKEN_MFA')

  const forgotPassword = async (email: string) => {
    const result = await authService.cognito.forgotPassword(email)
    if (result.ok) {
      return null
    } else {
      throw new Error(result.val.message)
    }
  }

  const resetPassword = async (email: string, code: string, password: string) => {
    const result = await authService.cognito.forgotPasswordSubmit(email, code, password)

    if (result.ok) {
      return null
    } else {
      throw new Error(result.val.message)
    }
  }

  const changePassword = async (oldPassword: string, newPassword: string) => {
    const result = await authService.cognito.changePassword(oldPassword, newPassword)

    if (result.err) {
      throw new Error(result.val.message)
    }

    return result.ok
  }

  watchEffect(
    () => {
      if (session.data.value) {
        httpClient.setSessionToken(session.data.value.accessToken)
      }
    },
    { flush: 'sync' },
  )

  // Register an effect that will listen for authentication events. When the event occurs, we
  // will refresh or clear the user's session, forcing a re-render of the page with the new
  // session.
  // For example, if a user clicks the "sign out" button, this will clear the user's session, which
  // means the login screen (which is a child of this provider) should render.
  const unregister = authService.registerAuthEventListener((event) => {
    switch (event) {
      case AuthEvent.signIn:
      case AuthEvent.signOut: {
        void queryClient.invalidateQueries({ queryKey: sessionQueryOptions.queryKey })
        break
      }
      case AuthEvent.customOAuthState:
      case AuthEvent.cognitoHostedUi: {
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
        unreachable(event)
      }
    }
  })
  onScopeDispose(unregister)

  const organizationId = authService.cognito.organizationId

  const getMFAPreference = async () => {
    const result = await authService.cognito.getMFAPreference()
    if (result.err) {
      throw result.val
    } else {
      return result.unwrap()
    }
  }

  const updateMFAPreference = async (mfaType: cognito.MfaType) => {
    const result = await authService.cognito.updateMFAPreference(mfaType)

    if (result.err) {
      throw result.val
    }
  }

  const verifyTotpToken = async (otp: string) => {
    const result = await authService.cognito.verifyTotpToken(otp)
    if (result.err) {
      throw result.val
    } else {
      return result.unwrap()
    }
  }

  const setupTOTP = async () => {
    const result = await authService.cognito.setupTOTP()
    if (result.err) {
      throw result.val
    } else {
      return result.unwrap()
    }
  }

  watchEffect(() => {
    if (session.data.value) {
      // Save access token so can it be reused by backend services
      authService.cognito.saveAccessToken(session.data.value)
    }
  })

  queryClient.getQueryCache().config.onError = (error, query) => {
    if (error instanceof NotAuthorizedError) {
      void refreshUserSessionMutation
        .mutateAsync()
        .then(() => queryClient.refetchQueries({ queryKey: query.queryKey }))
    }
  }
  queryClient.getMutationCache().config.onError = (error, variables, _context, mutation) => {
    if (error instanceof NotAuthorizedError) {
      void refreshUserSessionMutation.mutateAsync().then(() => mutation.execute(variables))
    }
  }

  return proxyRefs({
    signUp,
    session: session.data,
    suspense: session.suspense,
    isLoggingOut,
    confirmSignUp,
    signInWithPassword,
    signInWithGitHub,
    signInWithGoogle,
    confirmSignIn,
    forgotPassword,
    resetPassword,
    changePassword,
    signOut: logoutMutation.mutateAsync,
    organizationId,
    getMFAPreference,
    updateMFAPreference,
    verifyTotpToken,
    setupTOTP,
  })
})

const TEN_SECONDS_MS = 10_000
const SIX_HOURS_MS = 21_600_000

function setupRefresh(
  refreshUserSession: () => Promise<cognito.UserSession | null>,
  session: Ref<Opt<cognito.UserSession>>,
) {
  vueQuery.useQuery({
    queryKey: computed(
      () => ['refreshUserSession', { refreshToken: session.value?.refreshToken }] as const,
    ),
    queryFn: () => refreshUserSession(),
    meta: { persist: false },
    networkMode: 'online',
    initialData: session.value,
    initialDataUpdatedAt: Date.now(),
    refetchIntervalInBackground: true,
    refetchOnWindowFocus: 'always',
    refetchOnReconnect: 'always',
    refetchOnMount: 'always',
    enabled: computed(() => session.value != null),
    refetchInterval: () => {
      if (!session.value) return false
      const expireAt = session.value.expireAt

      const timeUntilRefresh =
        // If the session has not expired, we should refresh it when it is 5 minutes from expiring.
        // We use 1 second to ensure that we refresh even if the time is very close to expiring
        // and value won't be less than 0.
        Math.max(new Date(expireAt).getTime() - Date.now() - TEN_SECONDS_MS, TEN_SECONDS_MS)

      return timeUntilRefresh < SIX_HOURS_MS ? timeUntilRefresh : SIX_HOURS_MS
    },
  })
}

// TODO[ao]: restore logout dialog before merge.
