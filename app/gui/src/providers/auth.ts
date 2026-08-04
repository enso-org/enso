import type * as cognitoModule from '$/authentication/cognito'
import { useFeatureFlag } from '$/providers/featureFlags'
import * as analytics from '$/utils/analytics'
import type { Opt } from '@/util/data/opt'
import { proxyRefs, type ToValue } from '@/util/reactivity'
import { waitForData } from '@/util/tanstack'
import { useToast } from '@/util/toast'
import * as sentry from '@sentry/vue'
import * as vueQuery from '@tanstack/vue-query'
import { createGlobalState } from '@vueuse/core'
import * as backendModule from 'enso-common/src/services/Backend'
import { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { Rfc3339DateTime } from 'enso-common/src/utilities/data/dateTime'
import invariant from 'tiny-invariant'
import { computed, inject, toRef, toValue, watchEffect } from 'vue'
import { useBackends, type BackendsStore } from './backends'
import { useSession, type SessionStore } from './session'
import { useText, type TextStore } from './text'

/** Object containing the currently signed-in user's session data. */
export interface UserSession extends cognitoModule.UserSession {
  readonly user: backendModule.User
  /**
   * `true` when this session is a placeholder synthesized because the Cloud could not supply
   * the real one — either `users/me` failed with a non-auth error, or no Cloud host could be
   * reached at all. Local projects remain usable, but the `user` field is a stub — cloud
   * features must be disabled by callers.
   *
   * Only set on deployments that have a local backend; cloud-only builds fall back to
   * the existing redirect-to-login path, so this flag is never `true` there.
   */
  readonly isCloudDataUnavailable?: boolean
  /**
   * `true` when this placeholder session carries no Cognito identity at all: the Cloud was
   * unreachable and no session could be read, so nobody is known to be signed in.
   * Identity-bound actions (e.g. signing out) must be disabled by callers.
   */
  readonly isNotSignedIn?: boolean
}

/** Query to fetch the user's session data from the backend. */
export function createUsersMeQueryKey(
  session: ToValue<Opt<cognitoModule.UserSession>>,
  remoteBackend: RemoteBackend,
) {
  return [
    remoteBackend.type,
    'usersMe',
    computed(() => toValue(session)?.clientId ?? null),
  ] as const
}

const ACCOUNT_FRESHNESS_THRESHOLD_MS = 1000 * 60 * 30 // 30 minutes

function extractTimestampFromKsuid(ksuid: string): Date {
  const decoded = [...ksuid].reduce(
    (p, c) =>
      p * 62n + BigInt('0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'.indexOf(c)),
    0n,
  )
  const timestamp = Number(decoded >> 128n) * 1000
  return new Date(1.4e12 + timestamp)
}

/** Query to fetch the user's session data from the backend. */
export function createUsersMeQuery(
  session: ToValue<Opt<cognitoModule.UserSession>>,
  remoteBackend: RemoteBackend,
  setUsername: (username: string) => Promise<boolean>,
) {
  let refetchCount = 0
  return vueQuery.queryOptions({
    queryKey: createUsersMeQueryKey(session, remoteBackend),
    // Disable the default 3-retry backoff: a failed `users/me` should surface immediately
    // so the degraded-auth UI can render instead of stalling navigation for ~10 s while
    // the query retries. Unauthorized errors keep the existing sign-out handling;
    // transient network errors are handled by the user clicking the "Retry" button.
    retry: false,
    queryFn: async (): Promise<UserSession | null> => {
      const sessionVal = toValue(session)
      if (!sessionVal) {
        return null
      }
      const user = await remoteBackend.usersMe()
      if (user == null) {
        void setUsername(sessionVal.email)
        return null
      }
      if (user.plan === backendModule.Plan.free && refetchCount < 10) {
        const date = extractTimestampFromKsuid(user.organizationId.replace(/^organization-/, ''))
        if (Number(new Date()) - Number(date) < ACCOUNT_FRESHNESS_THRESHOLD_MS) {
          refetchCount += 1
          return null
        }
      }

      refetchCount = 0
      return { user, ...sessionVal }
    },
  })
}

/**
 * A stand-in for a Cognito session, used when no Cloud host can be reached and so there is no way
 * to establish who — if anyone — is signed in.
 *
 * It deliberately carries no credentials: every field that could authenticate a Cloud request is
 * empty, so it grants access to nothing beyond the local backend the user already owns.
 */
const OFFLINE_COGNITO_SESSION: cognitoModule.UserSession = {
  email: '',
  accessToken: '',
  refreshToken: '',
  refreshUrl: '',
  expireAt: Rfc3339DateTime(new Date(0).toJSON()),
  clientId: '',
}

/**
 * Synthesize a placeholder {@link backendModule.User} used while the real `users/me`
 * response is unavailable. All cloud features must be treated as disabled — the
 * placeholder mirrors a real user without a licence (`isEnabled: false`,
 * `plan: Plan.free`) so the existing "not-enabled" rendering paths apply.
 *
 * Identifier fields embed the Cognito email so the placeholder is distinct per signed-in
 * user (the Cognito app `clientId` is a deployment-wide constant and would alias users).
 */
export function makeSyntheticUser(cognitoSession: cognitoModule.UserSession): backendModule.User {
  const identitySuffix = cognitoSession.email || 'unknown'
  return {
    userId: backendModule.UserId(`user-cloud-unavailable-${identitySuffix}`),
    organizationId: backendModule.OrganizationId('organization-00000000000000000000000000'),
    rootDirectoryId: backendModule.DirectoryId('directory-cloud-unavailable'),
    name: cognitoSession.email,
    email: backendModule.EmailAddress(cognitoSession.email),
    isEnabled: false,
    isOrganizationAdmin: false,
    userGroups: null,
    plan: backendModule.Plan.free,
    groups: [],
    isEnsoTeamMember: false,
  }
}

/** The parts of the session state that the user's identity is derived from. */
type AuthSessionData = Pick<
  SessionStore,
  'session' | 'organizationId' | 'signOut' | 'isLoggingOut' | 'isCloudUnreachable'
> & {
  /** Resolves once the session settles; the resolved value is not used. */
  readonly waitForSession: () => Promise<unknown>
}

export type AuthStore = ReturnType<typeof createAuthStore>
/**
 * Create a store exposing the signed-in user, falling back to a local-only session when the
 * Cloud cannot supply one.
 */
export function createAuthStore(
  onAuthenticated: ((accessToken: string | null) => void) | undefined = inject('onAuthenticated'),
  sessionData: AuthSessionData = useSession(),
  backends: Pick<BackendsStore, 'localBackend' | 'remoteBackend'> = useBackends(),
  { getText }: Pick<TextStore, 'getText'> = useText(),
) {
  const { remoteBackend } = backends
  const session = toRef(sessionData, 'session')
  const { organizationId, signOut } = sessionData
  const toastSuccess = useToast.success()

  const queryClient = vueQuery.useQueryClient()

  const usersMeQueryKey = createUsersMeQueryKey(session, remoteBackend)

  const planOverride = useFeatureFlag('developerPlanOverride')

  const createUserMutation = vueQuery.useMutation({
    mutationFn: (user: backendModule.CreateUserRequestBody) => remoteBackend.createUser(user),
    onSuccess: analytics.createUser.after,
    meta: { invalidates: [usersMeQueryKey], awaitInvalidates: true },
  })

  const deleteUserMutation = vueQuery.useMutation({
    mutationFn: () => remoteBackend.deleteUser(),
    meta: { invalidates: [usersMeQueryKey], awaitInvalidates: true },
  })

  const restoreUserMutation = vueQuery.useMutation({
    mutationFn: () => remoteBackend.restoreUser(),
    meta: { invalidates: [usersMeQueryKey], awaitInvalidates: true },
  })

  const updateUserMutation = vueQuery.useMutation({
    mutationFn: (user: backendModule.UpdateUserRequestBody) => remoteBackend.updateUser(user),
    meta: { invalidates: [usersMeQueryKey], awaitInvalidates: true },
  })

  const setUsername = async (username: string) => {
    if (isCloudDataUnavailable.value) {
      throw new Error('Cannot set username while Enso Cloud is unavailable.')
    }
    // Branch on the real `users/me` result, not the (possibly synthetic) `userData`:
    // a synthetic placeholder would otherwise route us into the update path and hit
    // the failing remote.
    if (usersMeQuery.data.value != null) {
      await updateUserMutation.mutateAsync({ username })
    } else {
      const orgId = await organizationId()
      const email = session.value?.email ?? ''

      invariant(orgId == null || backendModule.isOrganizationId(orgId), 'Invalid organization ID')

      await createUserMutation.mutateAsync({
        userName: username,
        userEmail: backendModule.EmailAddress(email),
        organizationId: orgId != null ? orgId : null,
      })
    }
    // Wait until the backend returns a value from `users/me`,
    // otherwise the rest of the steps are skipped.
    // This only happens on specific devices, and (seemingly) only when using
    // the Vite development server, not with the built application bundle.
    // i.e. PROD=1
    await refetchSession()

    return true
  }

  const usersMeQueryOptions = createUsersMeQuery(session, remoteBackend, setUsername)

  const usersMeQuery = vueQuery.useQuery(usersMeQueryOptions)

  // Keyed on `email`, not `clientId`: `clientId` is the Cognito app integration ID and is
  // identical across users on the same deployment, so caching on it would surface user A's
  // placeholder for user B after a sign-out/sign-in.
  let syntheticUserCache: { email: string; user: backendModule.User } | null = null
  const getSyntheticUser = (cognitoSession: cognitoModule.UserSession) => {
    if (syntheticUserCache?.email !== cognitoSession.email) {
      syntheticUserCache = { email: cognitoSession.email, user: makeSyntheticUser(cognitoSession) }
    }
    return syntheticUserCache.user
  }

  /**
   * `true` when no Cloud host answers — Cognito could not be reached to ask who is signed in.
   * Distinct from a rejected request: nothing about the credentials is known, so signing the
   * user out would be both wrong and useless.
   */
  const isCloudUnreachable = computed(() => sessionData.isCloudUnreachable)

  /**
   * `true` when the Cloud cannot supply the user's data: either it is unreachable, or sign-in
   * succeeded but the subsequent `users/me` fetch failed with a non-auth error. Auth (401/403)
   * failures keep the existing sign-out and redirect handling and are excluded here. Requires
   * a local backend so the synthesised session has somewhere to land — on cloud-only
   * deployments without `localBackend`, this stays `false` and the user falls through to the
   * existing redirect-to-login path.
   */
  const isCloudDataUnavailable = computed(() => {
    if (sessionData.isLoggingOut) return false
    if (backends.localBackend == null) return false
    if (isCloudUnreachable.value) return true
    // Without a Cognito session there is nothing to degrade from: the user is simply logged out.
    if (!session.value) return false
    const error = usersMeQuery.error.value
    if (!error || backendModule.isUnauthorizedError(error)) return false
    return true
  })

  const userData = computed(() => {
    const real = usersMeQuery.data.value
    if (real) return real
    if (!isCloudDataUnavailable.value) return null
    // While the Cloud is unreachable the session cannot be read, so fall back to a credential-less
    // stand-in rather than stranding the user on a login screen that cannot work either.
    const cognitoSession = session.value ?? OFFLINE_COGNITO_SESSION
    return {
      ...cognitoSession,
      user: getSyntheticUser(cognitoSession),
      isCloudDataUnavailable: true,
      isNotSignedIn: session.value == null,
    } satisfies UserSession
  })
  const user = computed(() => userData.value?.user ?? null)

  const refetchSession = usersMeQuery.refetch

  const deleteUser = async () => {
    await deleteUserMutation.mutateAsync()
    await signOut()

    toastSuccess.show(getText('deleteUserSuccess'))

    return true
  }

  const restoreUser = async () => {
    await restoreUserMutation.mutateAsync()

    toastSuccess.show(getText('restoreUserSuccess'))

    return true
  }

  /**
   * Update the user session data in the React Query cache.
   * This only works for full user sessions.
   * @deprecated Never use this function. Prefer particular functions like `setUsername` or `deleteUser`.
   */
  const setUser = (user: Partial<backendModule.User>) => {
    const currentUser = queryClient.getQueryData(usersMeQueryOptions.queryKey)

    if (currentUser != null) {
      const currentUserData = currentUser.user
      const nextUserData: backendModule.User = Object.assign(currentUserData, user)

      queryClient.setQueryData(usersMeQueryOptions.queryKey, {
        ...currentUser,
        user: nextUserData,
      })
    }
  }

  const isUserMarkedForDeletion = () => !!user.value?.removeAt

  const isUserDeleted = () => {
    if (user.value?.removeAt) {
      const removeAtDate = new Date(user.value.removeAt)
      const now = new Date()

      return removeAtDate <= now
    } else {
      return false
    }
  }

  const isUserSoftDeleted = () => {
    if (user.value?.removeAt) {
      const removeAtDate = new Date(user.value.removeAt)
      const now = new Date()

      return removeAtDate > now
    } else {
      return false
    }
  }

  watchEffect(() => {
    if (userData.value) {
      if (!userData.value.isCloudDataUnavailable) {
        sentry.setUser({
          id: userData.value.user.userId,
          email: userData.value.email,
          username: userData.value.user.name,
          // eslint-disable-next-line camelcase
          ip_address: '{{auto}}',
        })
      }
      onAuthenticated?.(userData.value.accessToken)
    }
  })

  const effectiveUserData = computed(() =>
    userData.value && planOverride.value != null ?
      { ...userData.value, user: { ...userData.value.user, plan: planOverride.value } }
    : userData.value,
  )

  return proxyRefs({
    refetchSession,
    session: effectiveUserData,
    isCloudDataUnavailable,
    waitForSession: async () => {
      await sessionData.waitForSession()
      // Resolve once `users/me` settles, regardless of outcome — a failure switches the
      // store into the degraded-auth path rather than blocking navigation.
      try {
        await waitForData(usersMeQuery)
      } catch {
        // Ignore — `isCloudDataUnavailable` / `session` reflect the failure state.
      }
    },
    setUsername,
    isUserMarkedForDeletion,
    isUserDeleted,
    isUserSoftDeleted,
    restoreUser,
    deleteUser,
    setUser,
  })
}

/** A React provider for the Cognito API. */
export const useAuth = createGlobalState(createAuthStore)
