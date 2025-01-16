/**
 * @file Module for authenticating users with AWS Cognito.
 *
 * Provides an `AuthProvider` component that wraps the entire application, and a `useAuth` hook that
 * can be used from any React component to access the currently logged-in user's session data. The
 * hook also provides methods for registering a user, logging in, logging out, etc.
 */
import * as React from 'react'

import * as sentry from '@sentry/react'
import * as reactQuery from '@tanstack/react-query'
import * as router from 'react-router-dom'
import * as toast from 'react-toastify'
import invariant from 'tiny-invariant'

import * as detect from 'enso-common/src/detect'

import * as appUtils from '#/appUtils'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import * as gtagHooks from '#/hooks/gtagHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as sessionProvider from '#/providers/SessionProvider'
import * as textProvider from '#/providers/TextProvider'

import * as backendModule from '#/services/Backend'
import type RemoteBackend from '#/services/RemoteBackend'

import type * as cognitoModule from '#/authentication/cognito'
import { isOrganizationId } from '#/services/RemoteBackend'

// ===================
// === UserSession ===
// ===================

/** Possible types of {@link BaseUserSession}. */
export enum UserSessionType {
  offline = 'offline',
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

/**
 * A user session for a user that may be either fully registered,
 * or in the process of registering.
 */
export type UserSession = FullUserSession | PartialUserSession

// ===================
// === AuthContext ===
// ===================

/**
 * Interface returned by the `useAuth` hook.
 *
 * Contains the currently authenticated user's session data, as well as methods for signing in,
 * signing out, etc. All interactions with the authentication API should be done through this
 * interface.
 *
 * See `Cognito` for details on each of the authentication functions.
 */
interface AuthContextType {
  readonly authQueryKey: reactQuery.QueryKey
  readonly setUsername: (username: string) => Promise<boolean>
  /** @deprecated Never use this function. Prefer particular functions like `setUsername` or `deleteUser`. */
  readonly setUser: (user: Partial<backendModule.User>) => void
  readonly deleteUser: () => Promise<boolean>
  readonly restoreUser: () => Promise<boolean>
  readonly refetchSession: (
    options?: reactQuery.RefetchOptions,
  ) => Promise<reactQuery.QueryObserverResult<UserSession | null>>
  /**
   * Session containing the currently authenticated user's authentication information.
   *
   * If the user has not signed in, the session will be `null`.
   */
  readonly session: UserSession | null
  /** Return `true` if the user is marked for deletion. */
  readonly isUserMarkedForDeletion: () => boolean
  /** Return `true` if the user is deleted completely. */
  readonly isUserDeleted: () => boolean
  /** Return `true` if the user is soft deleted. */
  readonly isUserSoftDeleted: () => boolean
}

const AuthContext = React.createContext<AuthContextType | null>(null)

// ====================
// === AuthProvider ===
// ====================

/** Query to fetch the user's session data from the backend. */
function createUsersMeQuery(
  session: cognitoModule.UserSession | null,
  remoteBackend: RemoteBackend,
) {
  return reactQuery.queryOptions({
    queryKey: [remoteBackend.type, 'usersMe', session?.clientId] as const,
    queryFn: async () => {
      if (session == null) {
        return Promise.resolve(null)
      }

      return remoteBackend.usersMe().then((user) => {
        return user == null ?
            ({ type: UserSessionType.partial, ...session } satisfies PartialUserSession)
          : ({ type: UserSessionType.full, user, ...session } satisfies FullUserSession)
      })
    },
  })
}

/** Props for an {@link AuthProvider}. */
export interface AuthProviderProps {
  /** Callback to execute once the user has authenticated successfully. */
  readonly onAuthenticated: (accessToken: string | null) => void
  readonly children: React.ReactNode
}

/** A React provider for the Cognito API. */
export default function AuthProvider(props: AuthProviderProps) {
  const { onAuthenticated, children } = props

  const remoteBackend = backendProvider.useRemoteBackend()
  const { session, organizationId, signOut } = sessionProvider.useSession()
  const { getText } = textProvider.useText()
  const toastId = React.useId()

  const queryClient = reactQuery.useQueryClient()

  // This component cannot use `useGtagEvent` because `useGtagEvent` depends on the React Context
  // defined by this component.
  const gtagEvent = React.useCallback((name: string, params?: object) => {
    gtagHooks.event(name, params)
  }, [])

  const usersMeQueryOptions = createUsersMeQuery(session, remoteBackend)

  const usersMeQuery = reactQuery.useSuspenseQuery(usersMeQueryOptions)
  const userData = usersMeQuery.data

  const createUserMutation = reactQuery.useMutation({
    mutationFn: (user: backendModule.CreateUserRequestBody) => remoteBackend.createUser(user),
    meta: { invalidates: [usersMeQueryOptions.queryKey], awaitInvalidates: true },
  })

  const deleteUserMutation = reactQuery.useMutation({
    mutationFn: () => remoteBackend.deleteUser(),
    meta: { invalidates: [usersMeQueryOptions.queryKey], awaitInvalidates: true },
  })

  const restoreUserMutation = reactQuery.useMutation({
    mutationFn: () => remoteBackend.restoreUser(),
    meta: { invalidates: [usersMeQueryOptions.queryKey], awaitInvalidates: true },
  })

  const updateUserMutation = reactQuery.useMutation({
    mutationFn: (user: backendModule.UpdateUserRequestBody) => remoteBackend.updateUser(user),
    meta: { invalidates: [usersMeQueryOptions.queryKey], awaitInvalidates: true },
  })

  const toastSuccess = (message: string) => {
    toast.toast.update(toastId, {
      isLoading: null,
      autoClose: null,
      closeOnClick: null,
      closeButton: null,
      draggable: null,
      type: toast.toast.TYPE.SUCCESS,
      render: message,
    })
  }

  const refetchSession = usersMeQuery.refetch

  const setUsername = useEventCallback(async (username: string) => {
    gtagEvent('cloud_user_created')

    if (userData?.type === UserSessionType.full) {
      await updateUserMutation.mutateAsync({ username })
    } else {
      const orgId = await organizationId()
      const email = session?.email ?? ''

      invariant(orgId == null || isOrganizationId(orgId), 'Invalid organization ID')

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
  })

  const deleteUser = useEventCallback(async () => {
    await deleteUserMutation.mutateAsync()
    await signOut()

    toastSuccess(getText('deleteUserSuccess'))

    return true
  })

  const restoreUser = useEventCallback(async () => {
    await restoreUserMutation.mutateAsync()

    toastSuccess(getText('restoreUserSuccess'))

    return true
  })

  /**
   * Update the user session data in the React Query cache.
   * This only works for full user sessions.
   * @deprecated Never use this function. Prefer particular functions like `setUsername` or `deleteUser`.
   */
  const setUser = useEventCallback((user: Partial<backendModule.User>) => {
    const currentUser = queryClient.getQueryData(usersMeQueryOptions.queryKey)

    if (currentUser != null && currentUser.type === UserSessionType.full) {
      const currentUserData = currentUser.user
      const nextUserData: backendModule.User = Object.assign(currentUserData, user)

      queryClient.setQueryData(usersMeQueryOptions.queryKey, { ...currentUser, user: nextUserData })
    }
  })

  const isUserMarkedForDeletion = useEventCallback(
    () => !!(userData && 'user' in userData && userData.user.removeAt),
  )

  const isUserDeleted = useEventCallback(() => {
    if (userData && 'user' in userData && userData.user.removeAt) {
      const removeAtDate = new Date(userData.user.removeAt)
      const now = new Date()

      return removeAtDate <= now
    } else {
      return false
    }
  })

  const isUserSoftDeleted = useEventCallback(() => {
    if (userData && 'user' in userData && userData.user.removeAt) {
      const removeAtDate = new Date(userData.user.removeAt)
      const now = new Date()

      return removeAtDate > now
    } else {
      return false
    }
  })

  React.useEffect(() => {
    if (userData?.type === UserSessionType.full) {
      sentry.setUser({
        id: userData.user.userId,
        email: userData.email,
        username: userData.user.name,
        // eslint-disable-next-line @typescript-eslint/naming-convention, camelcase
        ip_address: '{{auto}}',
      })
    }
  }, [userData])

  React.useEffect(() => {
    if (userData?.type === UserSessionType.partial) {
      sentry.setUser({ email: userData.email })
    }
  }, [userData])

  React.useEffect(() => {
    gtagHooks.gtag('set', { platform: detect.platform(), architecture: detect.architecture() })
    return gtagHooks.gtagOpenCloseCallback(gtagEvent, 'open_app', 'close_app')
  }, [gtagEvent])

  React.useEffect(() => {
    if (userData?.type === UserSessionType.full) {
      onAuthenticated(userData.accessToken)
    }
  }, [userData, onAuthenticated])

  const value: AuthContextType = {
    refetchSession,
    session: userData,
    setUsername,
    isUserMarkedForDeletion,
    isUserDeleted,
    isUserSoftDeleted,
    restoreUser,
    deleteUser,
    setUser,
    authQueryKey: usersMeQueryOptions.queryKey,
  }

  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>
}

// ===============
// === useAuth ===
// ===============

/**
 * A React hook that provides access to the authentication context.
 *
 * Only the hook is exported, and not the context, because we only want to use the hook directly and
 * never the context component.
 * @throws {Error} when used outside a {@link AuthProvider}.
 */
export function useAuth() {
  const context = React.useContext(AuthContext)

  invariant(context != null, '`useAuth` must be used within an `<AuthProvider />`.')

  return context
}

// =======================
// === ProtectedLayout ===
// =======================

/** A React Router layout route containing routes only accessible by users that are logged in. */
export function ProtectedLayout() {
  const { session } = useAuth()

  if (session == null) {
    return <router.Navigate to={appUtils.LOGIN_PATH} />
  } else if (session.type === UserSessionType.partial) {
    return <router.Navigate to={appUtils.SETUP_PATH} />
  } else {
    return (
      <>
        {/* This div is used as a flag to indicate that the dashboard has been loaded and the user is authenticated. */}
        {/* also it guarantees that the top-level suspense boundary is already resolved */}
        <div data-testid="after-auth-layout" aria-hidden />
        <router.Outlet context={session} />
      </>
    )
  }
}

// ===========================
// === SemiProtectedLayout ===
// ===========================

/**
 * A React Router layout route containing routes only accessible by users that are
 * in the process of registering.
 */
export function SemiProtectedLayout() {
  const { session } = useAuth()
  const { localStorage } = localStorageProvider.useLocalStorage()

  // The user is not logged in - redirect to the login page.
  if (session == null) {
    return <router.Navigate to={appUtils.LOGIN_PATH} replace />
    // User is registered, redirect to dashboard or to the redirect path specified during the registration / login.
  } else if (session.type === UserSessionType.full) {
    const redirectTo = localStorage.delete('loginRedirect') ?? appUtils.DASHBOARD_PATH
    return <router.Navigate to={redirectTo} replace />
    // User is in the process of registration, allow them to complete the registration.
  } else {
    return <router.Outlet context={session} />
  }
}

// ===================
// === GuestLayout ===
// ===================

/**
 * A React Router layout route containing routes only accessible by users that are
 * not logged in.
 */
export function GuestLayout() {
  const { session } = useAuth()
  const { localStorage } = localStorageProvider.useLocalStorage()

  if (session?.type === UserSessionType.partial) {
    return <router.Navigate to={appUtils.SETUP_PATH} />
  } else if (session?.type === UserSessionType.full) {
    const redirectTo = localStorage.get('loginRedirect')
    if (redirectTo != null) {
      localStorage.delete('loginRedirect')
      location.href = redirectTo
      return
    } else {
      return <router.Navigate to={appUtils.DASHBOARD_PATH} />
    }
  } else {
    return (
      <>
        {/* This div is used as a flag to indicate that the user is not logged in. */}
        {/* also it guarantees that the top-level suspense boundary is already resolved */}
        <div data-testid="before-auth-layout" aria-hidden />
        <router.Outlet />
      </>
    )
  }
}

/** A React Router layout route containing routes only accessible by users that are not deleted. */
export function NotDeletedUserLayout() {
  const { session, isUserMarkedForDeletion } = useAuth()

  if (isUserMarkedForDeletion()) {
    return <router.Navigate to={appUtils.RESTORE_USER_PATH} />
  } else {
    return <router.Outlet context={session} />
  }
}

/** A React Router layout route containing routes only accessible by users that are deleted softly. */
export function SoftDeletedUserLayout() {
  const { session, isUserMarkedForDeletion, isUserDeleted, isUserSoftDeleted } = useAuth()

  if (isUserMarkedForDeletion()) {
    const isSoftDeleted = isUserSoftDeleted()
    const isDeleted = isUserDeleted()
    if (isSoftDeleted) {
      return <router.Outlet context={session} />
    } else if (isDeleted) {
      return <router.Navigate to={appUtils.LOGIN_PATH} />
    } else {
      return <router.Navigate to={appUtils.DASHBOARD_PATH} />
    }
  }
}

// =============================
// === usePartialUserSession ===
// =============================

/**
 * A React context hook returning the user session
 * for a user that has not yet completed registration.
 */
export function usePartialUserSession() {
  const { session } = useAuth()

  invariant(session?.type === UserSessionType.partial, 'Expected a partial user session.')

  return session
}

// ======================
// === useUserSession ===
// ======================

/** A React context hook returning the user session for a user that may or may not be logged in. */
export function useUserSession() {
  return useAuth().session
}

// ==========================
// === useFullUserSession ===
// ==========================

/** A React context hook returning the user session for a user that is fully logged in. */
export function useFullUserSession(): FullUserSession {
  const { session } = useAuth()

  invariant(session?.type === UserSessionType.full, 'Expected a full user session.')

  return session
}

/** A React context hook returning the user session for a user that is fully logged in. */
export function useUser() {
  const { user } = useFullUserSession()

  return user
}
