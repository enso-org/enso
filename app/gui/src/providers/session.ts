import { unsetModal } from '#/providers/ModalProvider'
import { NotAuthorizedError } from '#/services/Backend'
import HttpClient from '#/utilities/HttpClient'
import LocalStorage from '#/utilities/LocalStorage'
import { createAuth0SessionProvider, SessionProvider } from '$/authentication/auth0'
import { LOGOUT_EVENT } from '$/providers/session/constants'
import * as analytics from '$/utils/analytics'
import { Err } from '@/util/data/result'
import { proxyRefs } from '@/util/reactivity'
import { useToast } from '@/util/toast'
import * as sentry from '@sentry/vue'
import * as vueQuery from '@tanstack/vue-query'
import { createGlobalState } from '@vueuse/core'
import { computed, ref, toRaw, watchEffect } from 'vue'
import { useHttpClient } from './httpClient'
import { useText } from './text'

/** Create a query for the user session. */
export function createSessionQuery(authService: SessionProvider) {
  return vueQuery.queryOptions({
    queryKey: ['userSession'],
    queryFn: async () => authService.userSession().catch(() => null),
  })
}

export type SessionStore = ReturnType<typeof createSessionStore>
/** Create a store maintaining session information. */
export function createSessionStore(
  authService: SessionProvider,
  httpClient: HttpClient = useHttpClient(),
  { getText } = useText(),
  queryClient = vueQuery.useQueryClient(),
  localStorage = LocalStorage.getInstance(),
) {
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
      window.authenticationApi?.saveAccessToken(null)
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

  watchEffect(
    () => {
      if (session.data.value) {
        httpClient.setSessionToken(session.data.value.accessToken)
      }
    },
    { flush: 'sync' },
  )

  watchEffect(() => {
    if (session.data.value) {
      // Save access token so can it be reused by backend services
      // `saveAccessToken` passes its argument through Electron IPC.
      // `toRaw` is required because `session.data.value` is a reactive `Proxy`,
      // which cannot be `structuredClone`d (and therefore cannot be sent over IPC).
      window.authenticationApi?.saveAccessToken(toRaw(session.data.value))
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
    session: session.data,
    waitForSession: session.suspense,
    isLoggingOut,
    signUp: async (): Promise<void> => {
      analytics.cloudSignUp.before()
      return await authService.signUp()
    },
    signIn: async (): Promise<void> => {
      analytics.signIn.before()
      return await authService.signIn()
    },
    signOut: logoutMutation.mutateAsync,
  })
}

export const useSession = createGlobalState(() => createSessionStore(createAuth0SessionProvider()))
