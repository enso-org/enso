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
import { isUnauthorizedError } from 'enso-common/src/services/Backend'
import type { HttpClient } from 'enso-common/src/services/HttpClient'
import { Err } from 'enso-common/src/utilities/data/result'
import { unreachable } from 'enso-common/src/utilities/errors'
import { computed, onScopeDispose, ref, toRaw, toValue, watchEffect } from 'vue'
import { useHttpClient } from './httpClient'
import { useText } from './text'

/** Create a query for the user session. */
export function createSessionQuery(authService: ToValue<cognito.ISessionProvider | undefined>) {
  return vueQuery.queryOptions({
    queryKey: ['userSession'],
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

interface AuthRecoveryBackoffOptions {
  readonly maxAttempts: number
  readonly initialDelayMs: number
  readonly multiplier: number
  readonly maxDelayMs: number
  readonly jitter: number
}

interface RepeatedUnauthorizedRecoveryBackoffOptions extends AuthRecoveryBackoffOptions {
  readonly resetWindowMs: number
}

const AUTH_RECOVERY_BACKOFF_DEFAULTS: AuthRecoveryBackoffOptions = {
  maxAttempts: 4,
  initialDelayMs: 300,
  multiplier: 2,
  maxDelayMs: 5000,
  jitter: 0.2,
}

const REPEATED_UNAUTHORIZED_RECOVERY_BACKOFF_DEFAULTS: RepeatedUnauthorizedRecoveryBackoffOptions =
  {
    maxAttempts: 3,
    initialDelayMs: 300,
    multiplier: 2,
    maxDelayMs: 5000,
    jitter: 0.2,
    resetWindowMs: 30_000,
  }

const RECONNECTING_SESSION_DELAY_MS = 1000

function wait(delayMs: number) {
  return new Promise<void>((resolve) => {
    setTimeout(resolve, delayMs)
  })
}

function withJitter(delayMs: number, jitter: number) {
  if (jitter <= 0) {
    return delayMs
  }
  const jitterRange = delayMs * jitter
  const randomOffset = (Math.random() * 2 - 1) * jitterRange
  return Math.max(0, Math.round(delayMs + randomOffset))
}

function nextBackoffDelay(delayMs: number, options: AuthRecoveryBackoffOptions) {
  const boundedDelayMs = Math.min(delayMs, options.maxDelayMs)
  return {
    delayMs: withJitter(boundedDelayMs, options.jitter),
    nextDelayMs: Math.min(options.maxDelayMs, Math.round(delayMs * options.multiplier)),
  }
}

function isUsersMeQuery(query: { readonly queryKey: readonly unknown[] }) {
  return query.queryKey[1] === 'usersMe'
}

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
  const reconnectingSessionBackoffWaitCount = ref(0)
  const isReconnectingSession = computed(() => reconnectingSessionBackoffWaitCount.value > 0)
  let authRecoveryPromise: Promise<boolean> | null = null
  let repeatedUnauthorizedRecoveryPromise: Promise<boolean> | null = null
  let repeatedUnauthorizedRecoveryAttempts = 0
  let repeatedUnauthorizedDelayMs = REPEATED_UNAUTHORIZED_RECOVERY_BACKOFF_DEFAULTS.initialDelayMs
  let unauthorizedRecoveryLastActivityAt = 0
  let hasRecoveredUnauthorizedSession = false
  let hasReportedRepeatedUnauthorizedError = false
  let terminalAuthFailurePromise: Promise<void> | null = null
  let replayedQueryHashes = new Set<string>()
  let pendingRepeatedUnauthorizedQueries = new Map<string, readonly unknown[]>()
  let replayedMutations = new WeakSet<object>()

  const isAuthRecoveryBlocked = () => terminalAuthFailurePromise != null || isLoggingOut.value

  const recordUnauthorizedRecoveryActivity = (
    resetWindowMs = REPEATED_UNAUTHORIZED_RECOVERY_BACKOFF_DEFAULTS.resetWindowMs,
  ) => {
    const now = Date.now()
    if (
      unauthorizedRecoveryLastActivityAt > 0 &&
      now - unauthorizedRecoveryLastActivityAt > resetWindowMs
    ) {
      resetRepeatedUnauthorizedRecoveryState()
    }
    unauthorizedRecoveryLastActivityAt = now
  }

  const waitForRecoveryBackoff = async (delayMs: number) => {
    if (delayMs <= RECONNECTING_SESSION_DELAY_MS) {
      await wait(delayMs)
      return
    }

    reconnectingSessionBackoffWaitCount.value += 1
    try {
      await wait(delayMs)
    } finally {
      reconnectingSessionBackoffWaitCount.value = Math.max(
        0,
        reconnectingSessionBackoffWaitCount.value - 1,
      )
    }
  }

  const resetRepeatedUnauthorizedRecoveryState = () => {
    repeatedUnauthorizedRecoveryAttempts = 0
    repeatedUnauthorizedDelayMs = REPEATED_UNAUTHORIZED_RECOVERY_BACKOFF_DEFAULTS.initialDelayMs
    unauthorizedRecoveryLastActivityAt = 0
    hasRecoveredUnauthorizedSession = false
    hasReportedRepeatedUnauthorizedError = false
    replayedQueryHashes = new Set<string>()
    pendingRepeatedUnauthorizedQueries = new Map<string, readonly unknown[]>()
    replayedMutations = new WeakSet<object>()
    reconnectingSessionBackoffWaitCount.value = 0
  }

  const reportRepeatedUnauthorizedError = (error: unknown) => {
    if (hasReportedRepeatedUnauthorizedError) {
      return
    }
    hasReportedRepeatedUnauthorizedError = true
    errorToast.reportError(Err(error).error)
  }

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
      resetRepeatedUnauthorizedRecoveryState()
      successToast.show(getText('signOutSuccess'))
    },
    onError: () => errorToast.show(getText('signOutError')),
    meta: { invalidates: [sessionQueryOptions.queryKey], awaitInvalidates: true },
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
    analytics.signIn.before(provider)
    return signIn
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
        resetRepeatedUnauthorizedRecoveryState()
        break
      }
      case AuthEvent.signedOut: {
        resetRepeatedUnauthorizedRecoveryState()
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

  const reportTerminalAuthFailure = (error: unknown) => {
    if (terminalAuthFailurePromise) {
      return terminalAuthFailurePromise
    }

    terminalAuthFailurePromise = (async () => {
      errorToast.reportError(Err(error).error, getText('sessionExpiredError'))
      queryClient.setQueryData(sessionQueryOptions.queryKey, null)
      await queryClient.cancelQueries({ predicate: isUsersMeQuery })
      const usersMeQueries = queryClient.getQueryCache().findAll({ predicate: isUsersMeQuery })
      for (const usersMeQuery of usersMeQueries) {
        queryClient.setQueryData(usersMeQuery.queryKey, null)
      }
      queryClient.removeQueries({ predicate: isUsersMeQuery })
      httpClient.clearSessionToken()
      resetRepeatedUnauthorizedRecoveryState()
      await logoutMutation.mutateAsync().catch(() => undefined)
    })().finally(() => {
      terminalAuthFailurePromise = null
    })

    return terminalAuthFailurePromise
  }

  const refreshUserSessionWithBackoff = async (
    options: AuthRecoveryBackoffOptions = AUTH_RECOVERY_BACKOFF_DEFAULTS,
  ) => {
    let delayMs = options.initialDelayMs

    for (let attempt = 1; attempt <= options.maxAttempts; attempt += 1) {
      try {
        const refreshedSession = await refreshUserSessionMutation.mutateAsync()
        if (refreshedSession == null) {
          throw new Error('Session refresh returned null.')
        }
        return refreshedSession
      } catch (error) {
        if (attempt >= options.maxAttempts) {
          throw error
        }
        const nextDelay = nextBackoffDelay(delayMs, options)
        await waitForRecoveryBackoff(nextDelay.delayMs)
        delayMs = nextDelay.nextDelayMs
      }
    }

    throw new Error('Session refresh exhausted all retries.')
  }

  const recoverSessionAfterUnauthorizedError = () => {
    if (authRecoveryPromise) {
      return authRecoveryPromise
    }
    if (isAuthRecoveryBlocked()) {
      return Promise.resolve(false)
    }

    authRecoveryPromise = (async () => {
      try {
        await refreshUserSessionWithBackoff()
        hasRecoveredUnauthorizedSession = true
        recordUnauthorizedRecoveryActivity()
        return true
      } catch (error) {
        await reportTerminalAuthFailure(error)
        return false
      } finally {
        authRecoveryPromise = null
      }
    })()

    return authRecoveryPromise
  }

  type UnauthorizedFailedQuery = {
    readonly queryHash: string
    readonly queryKey: readonly unknown[]
  }

  const queueRepeatedUnauthorizedQuery = (query: UnauthorizedFailedQuery) => {
    pendingRepeatedUnauthorizedQueries.set(query.queryHash, query.queryKey)
  }

  const recoverSessionAfterRepeatedUnauthorizedError = (
    error: unknown,
    options: RepeatedUnauthorizedRecoveryBackoffOptions = REPEATED_UNAUTHORIZED_RECOVERY_BACKOFF_DEFAULTS,
  ) => {
    if (repeatedUnauthorizedRecoveryPromise) {
      return repeatedUnauthorizedRecoveryPromise
    }
    if (isAuthRecoveryBlocked()) {
      return Promise.resolve(false)
    }

    recordUnauthorizedRecoveryActivity(options.resetWindowMs)

    if (repeatedUnauthorizedRecoveryAttempts >= options.maxAttempts) {
      pendingRepeatedUnauthorizedQueries = new Map<string, readonly unknown[]>()
      reportRepeatedUnauthorizedError(error)
      return Promise.resolve(false)
    }

    repeatedUnauthorizedRecoveryAttempts += 1
    const nextDelay = nextBackoffDelay(repeatedUnauthorizedDelayMs, options)
    repeatedUnauthorizedDelayMs = nextDelay.nextDelayMs

    repeatedUnauthorizedRecoveryPromise = (async () => {
      await waitForRecoveryBackoff(nextDelay.delayMs)
      if (isAuthRecoveryBlocked()) {
        return false
      }
      const wasRecovered = await recoverSessionAfterUnauthorizedError()
      if (!wasRecovered) {
        return false
      }

      const queuedQueries = pendingRepeatedUnauthorizedQueries
      pendingRepeatedUnauthorizedQueries = new Map<string, readonly unknown[]>()
      await Promise.allSettled(
        [...queuedQueries.entries()].map(([queryHash, queryKey]) => {
          if (replayedQueryHashes.has(queryHash)) {
            return Promise.resolve()
          }
          replayedQueryHashes.add(queryHash)
          return queryClient.refetchQueries({ queryKey, exact: true })
        }),
      )
      return true
    })().finally(() => {
      repeatedUnauthorizedRecoveryPromise = null
    })

    return repeatedUnauthorizedRecoveryPromise
  }

  const queryCache = queryClient.getQueryCache()
  const mutationCache = queryClient.getMutationCache()
  const previousOnQueryError = queryCache.config.onError ?? (() => undefined)
  const previousOnMutationError = mutationCache.config.onError ?? (() => undefined)

  queryCache.config.onError = (error, query) => {
    previousOnQueryError(error, query)
    if (isUnauthorizedError(error)) {
      recordUnauthorizedRecoveryActivity()

      if (hasRecoveredUnauthorizedSession && isUsersMeQuery(query)) {
        void reportTerminalAuthFailure(error)
        return
      }

      if (hasRecoveredUnauthorizedSession) {
        if (replayedQueryHashes.has(query.queryHash)) {
          reportRepeatedUnauthorizedError(error)
          return
        }
        queueRepeatedUnauthorizedQuery(query)
        void recoverSessionAfterRepeatedUnauthorizedError(error)
        return
      }

      const queryHash = query.queryHash
      void recoverSessionAfterUnauthorizedError().then((wasRecovered) => {
        if (!wasRecovered || replayedQueryHashes.has(queryHash)) {
          return
        }
        replayedQueryHashes.add(queryHash)
        return queryClient.refetchQueries({ queryKey: query.queryKey, exact: true })
      })
    }
  }

  mutationCache.config.onError = (error, variables, onMutateResult, mutation, context) => {
    previousOnMutationError(error, variables, onMutateResult, mutation, context)
    if (isUnauthorizedError(error)) {
      if (replayedMutations.has(mutation)) {
        reportRepeatedUnauthorizedError(error)
        return
      }

      const recoverPromise =
        hasRecoveredUnauthorizedSession ?
          recoverSessionAfterRepeatedUnauthorizedError(error)
        : recoverSessionAfterUnauthorizedError()

      void recoverPromise.then((wasRecovered) => {
        if (!wasRecovered || replayedMutations.has(mutation)) {
          return
        }
        replayedMutations.add(mutation)
        return mutation.execute(variables)
      })
    }
  }

  onScopeDispose(() => {
    queryCache.config.onError = previousOnQueryError
    mutationCache.config.onError = previousOnMutationError
    resetRepeatedUnauthorizedRecoveryState()
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
