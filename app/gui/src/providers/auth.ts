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
import invariant from 'tiny-invariant'
import { computed, inject, toRef, toValue, watchEffect } from 'vue'
import { useBackends } from './backends'
import { useSession } from './session'
import { useText } from './text'

/** Object containing the currently signed-in user's session data. */
export interface UserSession extends cognitoModule.UserSession {
  readonly user: backendModule.User
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
    queryFn: async () => {
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

export type AuthStore = ReturnType<typeof createAuthStore>
function createAuthStore(
  onAuthenticated: ((accessToken: string | null) => void) | undefined = inject('onAuthenticated'),
  sessionData = useSession(),
  { remoteBackend } = useBackends(),
  { getText } = useText(),
) {
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
    if (userData.value != null) {
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
  const userData = usersMeQuery.data
  const user = computed(() =>
    userData.value && 'user' in userData.value ? userData.value.user : null,
  )

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
      sentry.setUser({
        id: userData.value.user.userId,
        email: userData.value.email,
        username: userData.value.user.name,
        // eslint-disable-next-line camelcase
        ip_address: '{{auto}}',
      })
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
    waitForSession: () => sessionData.waitForSession().then(() => waitForData(usersMeQuery)),
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
