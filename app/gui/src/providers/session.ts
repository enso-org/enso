import { unsetModal } from '#/providers/ModalProvider'
import LocalStorage from '#/utilities/LocalStorage'
import { ALL_PATHS_REGEX } from '$/appUtils'
import * as cognito from '$/authentication/cognito'
import { AuthEvent, type ListenFunction } from '$/authentication/listen'
import { useInitAuthService } from '$/authentication/service'
import { LOGOUT_EVENT } from '$/providers/session/constants'
import * as analytics from '$/utils/analytics'
import { proxyRefs } from '@/util/reactivity'
import { waitForData } from '@/util/tanstack'
import { useToast } from '@/util/toast'
import * as sentry from '@sentry/vue'
import * as vueQuery from '@tanstack/vue-query'
import { createGlobalState } from '@vueuse/core'
import { NotAuthorizedError } from 'enso-common/src/services/Backend'
import type { HttpClient } from 'enso-common/src/services/HttpClient'
import { Err } from 'enso-common/src/utilities/data/result'
import { unreachable } from 'enso-common/src/utilities/errors'
import { computed, onScopeDispose, ref, toRaw, watchEffect } from 'vue'
import { useHttpClient } from './httpClient'
import { useText } from './text'

/** Create a query for the user session. */
export function createSessionQuery(authService: cognito.ISessionProvider) {
  return vueQuery.queryOptions({
    queryKey: ['userSession'],
    queryFn: async () => authService.userSession().catch(() => null),
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
  authService: cognito.ISessionProvider,
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

  const refreshUserSessionMutation = vueQuery.useMutation({
    mutationKey: computed(() => ['refreshUserSession', { expireAt: session.data.value?.expireAt }]),
    mutationFn: async () => authService.refreshUserSession(),
    onSuccess: (data) => {
      if (data) {
        httpClient.setSessionToken(data.accessToken)
      }
    },
    onError: (error) => {
      // Something went wrong with the refresh token, so we need to sign the user out.
      errorToast.reportError(Err(error).error, getText('sessionExpiredError'))
      queryClient.setQueryData(sessionQueryOptions.queryKey, null)
      return logoutMutation.mutate()
    },
    meta: {
      invalidates: [sessionQueryOptions.queryKey],
      awaitInvalidates: true,
    },
  })

  const logoutMutation = vueQuery.useMutation({
    mutationKey: computed(() => ['session', 'logout', session.data.value?.clientId] as const),
    mutationFn: async () => {
      isLoggingOut.value = true
      document.dispatchEvent(new Event(LOGOUT_EVENT))
      await authService.signOut()

      const parentDomain = location.hostname.replace(/^[^.]*\./, '')
      document.cookie = `logged_in=no;max-age=0;domain=${parentDomain}`

      authService.saveAccessToken(null)
      isLoggingOut.value = false
    },
    // If the User Menu is still visible, it breaks when `userSession` is set to `null`.
    onMutate: unsetModal,
    onSuccess: async () => {
      analytics.cloudSignOut.after()
      localStorage.clearUserSpecificEntries()
      sentry.setUser(null)
      successToast.show(getText('signOutSuccess'))
    },
    onError: () => errorToast.show(getText('signOutError')),
    meta: { invalidates: [sessionQueryOptions.queryKey], awaitInvalidates: true },
  })

  const signUp = async (username: string, password: string, organizationId: string | null) => {
    analytics.cloudSignUp.before()
    const result = await authService.signUp(username, password, organizationId)

    if (result.err) {
      throw new Error(result.val.message)
    } else {
      return
    }
  }

  const confirmSignUp = async (email: string, code: string): Promise<void> => {
    analytics.cloudSignUp.confirm.before()
    const result = await authService.confirmSignUp(email, code)

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

  function challengeStepRequired(
    user: cognito.CognitoUser,
  ): 'SMS_MFA' | 'SOFTWARE_TOKEN_MFA' | null {
    switch (user.challengeName) {
      case 'SMS_MFA':
      case 'SOFTWARE_TOKEN_MFA': {
        return user.challengeName
      }
      case undefined:
      case 'CUSTOM_CHALLENGE':
      case 'MFA_SETUP':
      case 'NEW_PASSWORD_REQUIRED':
      case 'SELECT_MFA_TYPE':
      default: {
        return null
      }
    }
  }

  const signInWithPassword = async (
    email: string,
    password: string,
  ): Promise<{ user: cognito.CognitoUser; challenge: boolean }> => {
    analytics.signIn.before('Email')
    const result = await authService.signInWithPassword(email, password)
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
    return { user, challenge: challengeType != null }
  }

  function useSignIn(
    signIn: () => Promise<void>,
    provider: analytics.AuthProvider,
  ): () => Promise<boolean> {
    analytics.signIn.before(provider)

    return () =>
      signIn().then(
        () => true,
        () => false,
      )
  }

  const signInWithApple = useSignIn(() => authService.signInWithApple(), 'Apple')
  const signInWithGoogle = useSignIn(() => authService.signInWithGoogle(), 'Google')
  const signInWithGitHub = useSignIn(() => authService.signInWithGitHub(), 'GitHub')
  const signInWithMicrosoft = useSignIn(() => authService.signInWithMicrosoft(), 'Microsoft')

  const confirmSignIn = async (
    user: cognito.CognitoUser,
    otp: string,
  ): cognito.ConfirmSignInReturn => {
    analytics.signIn.confirm.before()
    return authService.confirmSignIn(user, otp, 'SOFTWARE_TOKEN_MFA')
  }

  const forgotPassword = async (email: string) => {
    const result = await authService.forgotPassword(email)
    if (result.ok) {
      return null
    } else {
      throw new Error(result.val.message)
    }
  }

  const resetPassword = async (email: string, code: string, password: string) => {
    const result = await authService.forgotPasswordSubmit(email, code, password)

    if (result.ok) {
      return null
    } else {
      throw new Error(result.val.message)
    }
  }

  const changePassword = async (oldPassword: string, newPassword: string) => {
    const result = await authService.changePassword(oldPassword, newPassword)

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
      case AuthEvent.signIn: {
        analytics.signIn.after()
        break
      }
      case AuthEvent.signOut: {
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
        break
      }
      default: {
        unreachable(event)
      }
    }
    void queryClient.invalidateQueries({ queryKey: sessionQueryOptions.queryKey })
  })
  onScopeDispose(unregister)

  const organizationId = authService.organizationId

  const getMFAPreference = async () => {
    const result = await authService.getMFAPreference()
    if (result.err) {
      throw result.val
    } else {
      return result.unwrap()
    }
  }

  const updateMFAPreference = async (mfaType: cognito.MfaType) => {
    const result = await authService.updateMFAPreference(mfaType)

    if (result.err) {
      throw result.val
    }
  }

  const verifyTotpToken = async (otp: string) => {
    const result = await authService.verifyTotpToken(otp)
    if (result.err) {
      throw result.val
    } else {
      return result.unwrap()
    }
  }

  const setupTOTP = async () => {
    const result = await authService.setupTOTP()
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
      authService.saveAccessToken(toRaw(session.data.value))
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
    waitForSession: () => waitForData(session),
    isLoggingOut,
    confirmSignUp,
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
