import * as gtagHooks from '#/hooks/gtagHooks'
import * as backendModule from '#/services/Backend'
import RemoteBackend from '#/services/RemoteBackend'
import { BLACK_SQUARE_IMAGE_512PX } from '#/utilities/image'
import type * as cognitoModule from '$/authentication/cognito'
import {
  featureFlagsForInternalTesting,
  setFeatureFlags,
  useFeatureFlag,
} from '$/providers/featureFlags'
import { useZustandStoreRef } from '$/utils/zustand'
import { useToast } from '@/util/toast'
import * as sentry from '@sentry/vue'
import * as vueQuery from '@tanstack/vue-query'
import { createGlobalState } from '@vueuse/core'
import * as detect from 'enso-common/src/detect'
import invariant from 'tiny-invariant'
import { computed, inject, proxyRefs, watchEffect } from 'vue'
import { createStore } from 'zustand'
import { persist } from 'zustand/middleware'
import { useBackends } from './backends'
import { useSession } from './session'
import { useText } from './text'

/** Possible types of {@link BaseUserSession}. */
export enum UserSessionType {
  partial = 'partial',
  full = 'full',
}

/** Properties common to all {@link UserSession}s. */
interface BaseUserSession extends cognitoModule.UserSession {
  /** A discriminator for TypeScript to be able to disambiguate between `UserSession` variants. */
  readonly type: UserSessionType
}

/**
 * Object containing the currently signed-in user's session data, if the user has not yet set their
 * username.
 *
 * If a user has not yet set their username, they do not yet have an organization associated with
 * their account. Otherwise, this type is identical to the `Session` type. This type should ONLY be
 * used by the `SetUsername` component.
 */
export interface PartialUserSession extends BaseUserSession {
  readonly type: UserSessionType.partial
}

/** Object containing the currently signed-in user's session data. */
export interface FullUserSession extends BaseUserSession {
  /** User's organization information. */
  readonly type: UserSessionType.full
  readonly user: backendModule.User
}

/** Query to fetch the user's session data from the backend. */
export function createUsersMeQuery(remoteBackend: RemoteBackend) {
  return vueQuery.queryOptions({
    queryKey: [remoteBackend.type, 'usersMe'] as const,
    queryFn: () => remoteBackend.usersMe(),
  })
}

/** State for {@link authOverridesStore}. */
interface AuthOverridesStoreState {
  readonly planOverride: backendModule.Plan | undefined
  readonly setPlanOverride: (planOverride: backendModule.Plan | undefined) => void
}

export const authOverridesStore = createStore<AuthOverridesStoreState>()(
  persist(
    (set): AuthOverridesStoreState => ({
      planOverride: undefined,
      setPlanOverride: (planOverride) => {
        set({ planOverride })
      },
    }),
    { name: 'enso-auth-overrides', version: 1 },
  ),
)

export type AuthStore = ReturnType<typeof createAuthStore>
function createAuthStore(
  onAuthenticated: ((accessToken: string | null) => void) | undefined = inject('onAuthenticated'),
  sessionData = useSession(),
  { remoteBackend } = useBackends(),
  { getText } = useText(),
) {
  const { organizationId, signOut } = sessionData
  const toastSuccess = useToast.success()

  const queryClient = vueQuery.useQueryClient()

  // This component cannot use `useGtagEvent` because `useGtagEvent` depends on the React Context
  // defined by this component.
  const gtagEvent = gtagHooks.event

  const usersMeQueryOptions = createUsersMeQuery(remoteBackend)

  const usersMeQuery = vueQuery.useQuery(usersMeQueryOptions)
  const userData = computed(() => {
    const session = sessionData.session
    if (session == null) return null
    const user = usersMeQuery.data.value
    return user == null ?
        ({ type: UserSessionType.partial, ...session } satisfies PartialUserSession)
      : ({ type: UserSessionType.full, user, ...session } satisfies FullUserSession)
  })

  const planOverride = useZustandStoreRef(authOverridesStore, (state) => state.planOverride)
  const overrideProfilePicture = useFeatureFlag('overrideProfilePicture')

  const createUserMutation = vueQuery.useMutation({
    mutationFn: (user: backendModule.CreateUserRequestBody) => remoteBackend.createUser(user),
    meta: { invalidates: [usersMeQueryOptions.queryKey], awaitInvalidates: true },
  })

  const deleteUserMutation = vueQuery.useMutation({
    mutationFn: () => remoteBackend.deleteUser(),
    meta: { invalidates: [usersMeQueryOptions.queryKey], awaitInvalidates: true },
  })

  const restoreUserMutation = vueQuery.useMutation({
    mutationFn: () => remoteBackend.restoreUser(),
    meta: { invalidates: [usersMeQueryOptions.queryKey], awaitInvalidates: true },
  })

  const updateUserMutation = vueQuery.useMutation({
    mutationFn: (user: backendModule.UpdateUserRequestBody) => remoteBackend.updateUser(user),
    meta: { invalidates: [usersMeQueryOptions.queryKey], awaitInvalidates: true },
  })

  const refetchSession = usersMeQuery.refetch

  const setUsername = async (username: string) => {
    gtagEvent('cloud_user_created')

    if (userData.value?.type === UserSessionType.full) {
      await updateUserMutation.mutateAsync({ username })
    } else {
      const orgId = await organizationId()
      const email = sessionData.session?.email ?? ''

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
    queryClient.setQueryData(usersMeQueryOptions.queryKey, (currentUserData) =>
      currentUserData == null ? null : Object.assign({ ...currentUserData }, user),
    )
  }

  const isUserMarkedForDeletion = computed(() => usersMeQuery.data.value?.removeAt != null)

  const isUserDeleted = () => {
    const user = usersMeQuery.data.value
    if (user?.removeAt) {
      const removeAtDate = new Date(user.removeAt)
      const now = new Date()

      return removeAtDate <= now
    } else {
      return false
    }
  }

  const isUserSoftDeleted = () => isUserMarkedForDeletion.value && !isUserDeleted()

  watchEffect(() => {
    if (userData.value?.type === UserSessionType.full) {
      sentry.setUser({
        id: userData.value.user.userId,
        email: userData.value.email,
        username: userData.value.user.name,
        // eslint-disable-next-line camelcase
        ip_address: '{{auto}}',
      })
      onAuthenticated?.(userData.value.accessToken)
    } else if (userData.value?.type === UserSessionType.partial) {
      sentry.setUser({ email: userData.value.email })
    }
  })

  gtagHooks.gtag('set', { platform: detect.platform(), architecture: detect.architecture() })
  gtagHooks.gtagOpenCloseCallback(gtagEvent, 'open_app', 'close_app')

  watchEffect(() => {
    if (usersMeQuery.data.value?.isEnsoTeamMember) {
      setFeatureFlags(featureFlagsForInternalTesting())
    }
  })

  const effectiveUserData = computed(() => {
    if (!(userData.value?.type === UserSessionType.full)) return userData.value
    const intermediate =
      planOverride.value != null ?
        { ...userData.value, user: { ...userData.value.user, plan: planOverride.value } }
      : userData.value
    return overrideProfilePicture.value ?
        {
          ...intermediate,
          user: { ...intermediate.user, profilePicture: BLACK_SQUARE_IMAGE_512PX },
        }
      : intermediate
  })

  return proxyRefs({
    refetchSession,
    session: effectiveUserData,
    waitForSession: () => sessionData.waitForSession().then(() => usersMeQuery.suspense()),
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
