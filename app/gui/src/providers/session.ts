import { unsetModal } from '#/providers/ModalProvider'
import LocalStorage from '#/utilities/LocalStorage'
import { ALL_PATHS_REGEX } from '$/appUtils'
import * as cognito from '$/authentication/cognito'
import { AuthEvent, type ListenFunction } from '$/authentication/listen'
import { useInitAuthService } from '$/authentication/service'
import { LOGOUT_EVENT } from '$/providers/session/constants'
import * as analytics from '$/utils/analytics'
import { proxyRefs, type ToValue } from '$/utils/reactivity'
import { waitForData } from '@/util/tanstack'
import { useToast } from '@/util/toast'
import * as sentry from '@sentry/vue'
import * as vueQuery from '@tanstack/vue-query'
import { createGlobalState } from '@vueuse/core'
import type { SignInOutput } from 'aws-amplify/auth'
import type { HttpClient } from 'enso-common/src/services/HttpClient'
import { unreachable } from 'enso-common/src/utilities/errors'
import { computed, onScopeDispose, ref, toRaw, toValue, watchEffect } from 'vue'
import { useHttpClient } from './httpClient'
import { useUnauthorizedRecovery } from './session/useUnauthorizedRecovery'
import { useText } from './text'

export const USER_SESSION_QUERY_KEY = ['userSession'] as const

/** Create a query for the user session. */
export function createSessionQuery(authService: ToValue<cognito.ISessionProvider | undefined>) {
  return vueQuery.queryOptions({
    queryKey: USER_SESSION_QUERY_KEY,
    queryFn: async () =>
      toValue(authService)
        ?.userSession()
        .catch(() => null) ?? null,
  })
}

/** Returns the URL to the main page. This is the current URL, with the current route removed. */
function getMainPageUrl() {
  const mainPageUrl = new URL(window.location.href)
  mainPageUrl.pathname = mainPageUrl.pathname.replace(ALL_PATHS_REGEX, '')
  return mainPageUrl
}

export type SessionStore = ReturnType<typeof createSessionStore>

/** Create a store maintaining session information. */
export function createSessionStore(
  authService: ToValue<cognito.ISessionProvider | undefined>,
  registerAuthEventListener: ListenFunction,
  httpClient: HttpClient = useHttpClient(),
  { getText } = useText(),
  queryClient = vueQuery.useQueryClient(),
  localStorage = LocalStorage.getInstance(),
) {
  const mainPageUrl = getMainPageUrl()
  const errorToast = useToast.error()
  const successToast = useToast.success()

  const isLoggingOut = ref(false)

  const sessionQueryOptions = createSessionQuery(authService)
  const session = vueQuery.useQuery(sessionQueryOptions)

  const assertAuthService = (): cognito.ISessionProvider => {
    const auth = toValue(authService)
    if (auth == null) throw Error('Cognito not initialized')
    return auth
  }

  const refreshUserSessionMutation = vueQuery.useMutation({
    mutationKey: computed(() => ['refreshUserSession', { expireAt: session.data.value?.expireAt }]),
    mutationFn: async () => assertAuthService().refreshUserSession(),
    onSuccess: (data) => {
      if (data) {
        httpClient.setSessionToken(data.accessToken)
      }
    },
    meta: {
      invalidates: [sessionQueryOptions.queryKey],
      awaitInvalidates: true,
    },
  })

  const logoutMutation = vueQuery.useMutation({
    mutationKey: computed(() => ['session', 'logout', session.data.value?.clientId] as const),
    mutationFn: async () => {
      const auth = assertAuthService()
      isLoggingOut.value = true
      document.dispatchEvent(new Event(LOGOUT_EVENT))
      try {
        await auth.signOut()

        const parentDomain = location.hostname.replace(/^[^.]*\./, '')
        document.cookie = `logged_in=no;max-age=0;domain=${parentDomain}`

        auth.saveAccessToken(null)
        httpClient.clearSessionToken()
      } finally {
        isLoggingOut.value = false
      }
    },
    // If the User Menu is still visible, it breaks when `userSession` is set to `null`.
    onMutate: unsetModal,
    onSuccess: async () => {
      analytics.cloudSignOut.after()
      localStorage.clearUserSpecificEntries()
      sentry.setUser(null)
      resetUnauthorizedRecoveryState()
      successToast.show(getText('signOutSuccess'))
    },
    onError: () => errorToast.show(getText('signOutError')),
    meta: { invalidates: [sessionQueryOptions.queryKey], awaitInvalidates: true },
  })

  const { isReconnectingSession, resetUnauthorizedRecoveryState } = useUnauthorizedRecovery({
    queryClient,
    isLoggingOut,
    refreshUserSession: () => refreshUserSessionMutation.mutateAsync(),
    logout: () => logoutMutation.mutateAsync(),
    clearSessionToken: () => httpClient.clearSessionToken(),
    cancelSessionQuery: () =>
      queryClient.cancelQueries({ queryKey: USER_SESSION_QUERY_KEY, exact: true }),
    clearSessionQuery: () => queryClient.setQueryData(sessionQueryOptions.queryKey, null),
    reportSessionExpiredError: (error) =>
      errorToast.reportError(error, getText('sessionExpiredError')),
    reportRepeatedUnauthorizedError: (error) => errorToast.reportError(error),
  })

  const signUp = async (username: string, password: string, organizationId: string | null) => {
    const auth = assertAuthService()
    analytics.cloudSignUp.before()
    const result = await auth.signUp(username, password, organizationId)

    if (result.err) {
      throw new Error(result.val.message)
    } else {
      return
    }
  }
  const confirmSignUp = async (email: string, code: string): Promise<void> => {
    const auth = assertAuthService()
    analytics.cloudSignUp.confirm.before()
    const result = await auth.confirmSignUp(email, code)

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
    analytics.cloudSignUp.confirm.after()
  }

  const resendSignUp = async (username: string): Promise<void> => {
    await assertAuthService().resendSignUp(username)
  }

  /**
   * Return an identifier of required challenge step, if any.
   *
   * This function is used for analytics log, so it returns "old" SOFTWARE_TOKEN_MFA identifier.
   */
  function challengeStepRequired(user: SignInOutput): 'SMS_MFA' | 'SOFTWARE_TOKEN_MFA' | null {
    switch (user.nextStep.signInStep) {
      case 'CONFIRM_SIGN_IN_WITH_SMS_CODE':
        return 'SMS_MFA'
      case 'CONFIRM_SIGN_IN_WITH_TOTP_CODE':
        return 'SOFTWARE_TOKEN_MFA'
      default: {
        return null
      }
    }
  }

  const signInWithPassword = async (
    email: string,
    password: string,
  ): Promise<{ challenge: boolean }> => {
    const auth = assertAuthService()
    analytics.signIn.before('Email')
    const result = await auth.signInWithPassword(email, password)
    if (!result.ok) {
      throw new Error(result.val.message)
    }

    const user = result.unwrap()
    const challengeType = challengeStepRequired(user)
    if (challengeType) {
      analytics.signIn.confirm.expected(challengeType)
    } else {
      await queryClient.invalidateQueries({ queryKey: sessionQueryOptions.queryKey })
    }
    return { challenge: challengeType != null }
  }

  function useSignIn(signIn: () => Promise<void>, provider: analytics.AuthProvider) {
    return async () => {
      analytics.signIn.before(provider)
      await signIn()
    }
  }

  const signInWithApple = useSignIn(() => assertAuthService().signInWithApple(), 'Apple')
  const signInWithGoogle = useSignIn(() => assertAuthService().signInWithGoogle(), 'Google')
  const signInWithGitHub = useSignIn(() => assertAuthService().signInWithGitHub(), 'GitHub')
  const signInWithMicrosoft = useSignIn(
    () => assertAuthService().signInWithMicrosoft(),
    'Microsoft',
  )

  const confirmSignIn = async (challengeResponse: string): cognito.ConfirmSignInReturn => {
    const auth = assertAuthService()
    analytics.signIn.confirm.before()
    return auth.confirmSignIn(challengeResponse)
  }

  const forgotPassword = async (email: string) => {
    const result = await assertAuthService().forgotPassword(email)
    if (result.ok) {
      return null
    } else {
      throw new Error(result.val.message)
    }
  }

  const resetPassword = async (email: string, code: string, password: string) => {
    const result = await assertAuthService().forgotPasswordSubmit(email, code, password)

    if (result.ok) {
      return null
    } else {
      throw new Error(result.val.message)
    }
  }

  const changePassword = async (oldPassword: string, newPassword: string) => {
    const result = await assertAuthService().changePassword(oldPassword, newPassword)

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
  const unregister = registerAuthEventListener((event) => {
    switch (event) {
      case AuthEvent.signedIn: {
        analytics.signIn.after()
        resetUnauthorizedRecoveryState()
        break
      }
      case AuthEvent.signedOut: {
        resetUnauthorizedRecoveryState()
        break
      }
      case AuthEvent.customOAuthState:
      case AuthEvent.signInWithRedirect: {
        // AWS Amplify doesn't provide a way to set the redirect URL for the OAuth flow, so
        // we have to hack it by replacing the URL in the browser's history. This is done
        // because otherwise the user will be redirected to a URL like `enso://auth`, which
        // will not work.
        // See https://github.com/aws-amplify/amplify-js/issues/3391#issuecomment-756473970
        history.replaceState({}, '', mainPageUrl)
        break
      }
      default: {
        unreachable(event)
      }
    }
    void queryClient.invalidateQueries({ queryKey: sessionQueryOptions.queryKey })
  })
  onScopeDispose(unregister)

  const organizationId = () => toValue(authService)?.organizationId()

  const getMFAPreference = async () => {
    const result = await assertAuthService().getMFAPreference()
    if (result.err) {
      throw result.val
    } else {
      return result.unwrap()
    }
  }

  const updateMFAPreference = async (mfaType: cognito.MfaType) => {
    const result = await assertAuthService().updateMFAPreference(mfaType)

    if (result.err) {
      throw result.val
    }
  }

  const verifyTotpToken = async (otp: string) => {
    const result = await assertAuthService().verifyTotpToken(otp)
    if (result.err) {
      throw result.val
    } else {
      return result.unwrap()
    }
  }

  const setupTOTP = async () => {
    const result = await assertAuthService().setupTOTP()
    if (result.err) {
      throw result.val
    } else {
      return result.unwrap()
    }
  }

  watchEffect(() => {
    if (session.data.value) {
      // Save access token so can it be reused by backend services
      // `saveAccessToken` passes its argument through Electron IPC.
      // `toRaw` is required because `session.data.value` is a reactive `Proxy`,
      // which cannot be `structuredClone`d (and therefore cannot be sent over IPC).
      assertAuthService().saveAccessToken(toRaw(session.data.value))
    }
  })

  return proxyRefs({
    signUp,
    session: session.data,
    waitForSession: () => waitForData(session),
    isLoggingOut,
    isReconnectingSession,
    confirmSignUp,
    resendSignUp,
    signInWithPassword,
    signInWithGitHub,
    signInWithGoogle,
    signInWithMicrosoft,
    signInWithApple,
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
}

export const useSession = createGlobalState(() => {
  const { cognito, registerAuthEventListener } = useInitAuthService()
  return createSessionStore(cognito, registerAuthEventListener)
})
