/**
 * @file Module for authenticating users with AWS Cognito.
 *
 * Provides an `AuthProvider` component that wraps the entire application, and a `useAuth` hook that
 * can be used from any React component to access the currently logged-in user's session data. The
 * hook also provides methods for registering a user, logging in, logging out, etc.
 */
import {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useId,
  useState,
  type ReactNode,
} from 'react'

import { setUser as sentrySetUser } from '@sentry/react'
import {
  queryOptions,
  useMutation,
  useQueryClient,
  useSuspenseQuery,
  type QueryKey,
  type QueryObserverResult,
  type RefetchOptions,
} from '@tanstack/react-query'
import { Navigate, Outlet } from 'react-router-dom'
import { toast } from 'react-toastify'
import invariant from 'tiny-invariant'

import { architecture, platform } from 'enso-common/src/detect'

import {
  DASHBOARD_PATH,
  LOGIN_PATH,
  OPEN_IDE_DEEPLINK,
  RESTORE_USER_PATH,
  SETUP_PATH,
} from '#/appUtils'
import { type UserSession as CognitoUserSession } from '#/authentication/cognito'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { event, gtag, gtagOpenCloseCallback } from '#/hooks/gtagHooks'
import { useLoginRedirect } from '#/pages/authentication/Registration/registrationLocalStorage'
import { useRemoteBackend } from '#/providers/BackendProvider'
import { useLocalStorage } from '#/providers/LocalStorageProvider'
import { useSession } from '#/providers/SessionProvider'
import { useText } from '#/providers/TextProvider'
import {
  EmailAddress,
  isOrganizationId,
  type CreateUserRequestBody,
  type default as RemoteBackend,
  type UpdateUserRequestBody,
  type User,
} from '#/services/Backend'
import { Suspense } from 'react'
import { ErrorBoundary } from 'react-error-boundary'
import { Button, Text } from '../components/AriaComponents'
import { EnsoDevtools } from '../components/Devtools'
import Page from '../components/Page'
import { Result } from '../components/Result'
import { useTimeoutCallback } from '../hooks/timeoutHooks'
import { download } from '../utilities/download'
import { getDownloadUrl } from '../utilities/github'
import { unsafeWriteValue } from '../utilities/write'
import {
  featureFlagsForInternalTesting,
  useFeatureFlag,
  useSetFeatureFlags,
} from './FeatureFlagsProvider'

/** Possible types of {@link BaseUserSession}. */
export enum UserSessionType {
  offline = 'offline',
  partial = 'partial',
  full = 'full',
}

/** Properties common to all {@link UserSession}s. */
interface BaseUserSession extends CognitoUserSession {
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
  readonly user: User
}

/**
 * A user session for a user that may be either fully registered,
 * or in the process of registering.
 */
export type UserSession = FullUserSession | PartialUserSession

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
  readonly authQueryKey: QueryKey
  readonly setUsername: (username: string) => Promise<boolean>
  /** @deprecated Never use this function. Prefer particular functions like `setUsername` or `deleteUser`. */
  readonly setUser: (user: Partial<User>) => void
  readonly deleteUser: () => Promise<boolean>
  readonly restoreUser: () => Promise<boolean>
  readonly refetchSession: (
    options?: RefetchOptions,
  ) => Promise<QueryObserverResult<UserSession | null>>
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

const AuthContext = createContext<AuthContextType | null>(null)

// ====================
// === AuthProvider ===
// ====================

/** Query to fetch the user's session data from the backend. */
function createUsersMeQuery(session: CognitoUserSession | null, remoteBackend: RemoteBackend) {
  return queryOptions({
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
  readonly children: ReactNode
}

/** A React provider for the Cognito API. */
export default function AuthProvider(props: AuthProviderProps) {
  const { onAuthenticated, children } = props

  const remoteBackend = useRemoteBackend()
  const setFeatureFlags = useSetFeatureFlags()

  const { session, organizationId, signOut } = useSession()
  const { getText } = useText()
  const toastId = useId()

  const queryClient = useQueryClient()

  // This component cannot use `useGtagEvent` because `useGtagEvent` depends on the React Context
  // defined by this component.
  const gtagEvent = useCallback((name: string, params?: object) => {
    event(name, params)
  }, [])

  const usersMeQueryOptions = createUsersMeQuery(session, remoteBackend)

  const usersMeQuery = useSuspenseQuery(usersMeQueryOptions)
  const userData = usersMeQuery.data

  const createUserMutation = useMutation({
    mutationFn: (user: CreateUserRequestBody) => remoteBackend.createUser(user),
    meta: { invalidates: [usersMeQueryOptions.queryKey], awaitInvalidates: true },
  })

  const deleteUserMutation = useMutation({
    mutationFn: () => remoteBackend.deleteUser(),
    meta: { invalidates: [usersMeQueryOptions.queryKey], awaitInvalidates: true },
  })

  const restoreUserMutation = useMutation({
    mutationFn: () => remoteBackend.restoreUser(),
    meta: { invalidates: [usersMeQueryOptions.queryKey], awaitInvalidates: true },
  })

  const updateUserMutation = useMutation({
    mutationFn: (user: UpdateUserRequestBody) => remoteBackend.updateUser(user),
    meta: { invalidates: [usersMeQueryOptions.queryKey], awaitInvalidates: true },
  })

  const toastSuccess = (message: string) => {
    toast.update(toastId, {
      isLoading: null,
      autoClose: null,
      closeOnClick: null,
      closeButton: null,
      draggable: null,
      type: toast.TYPE.SUCCESS,
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
        userEmail: EmailAddress(email),
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
  const setUser = useEventCallback((user: Partial<User>) => {
    const currentUser = queryClient.getQueryData(usersMeQueryOptions.queryKey)

    if (currentUser != null && currentUser.type === UserSessionType.full) {
      const currentUserData = currentUser.user
      const nextUserData: User = Object.assign(currentUserData, user)

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

  useEffect(() => {
    if (userData?.type === UserSessionType.full) {
      sentrySetUser({
        id: userData.user.userId,
        email: userData.email,
        username: userData.user.name,
        // eslint-disable-next-line @typescript-eslint/naming-convention, camelcase
        ip_address: '{{auto}}',
      })
    }
  }, [userData])

  useEffect(() => {
    if (userData?.type === UserSessionType.partial) {
      sentrySetUser({ email: userData.email })
    }
  }, [userData])

  useEffect(() => {
    gtag('set', { platform: platform(), architecture: architecture() })
    return gtagOpenCloseCallback(gtagEvent, 'open_app', 'close_app')
  }, [gtagEvent])

  useEffect(() => {
    if (userData?.type === UserSessionType.full) {
      onAuthenticated(userData.accessToken)
    }
  }, [userData, onAuthenticated])

  useEffect(() => {
    if (userData?.type === UserSessionType.full && userData.user.isEnsoTeamMember) {
      setFeatureFlags(featureFlagsForInternalTesting())
    }
  }, [userData, setFeatureFlags])

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
  const context = useContext(AuthContext)

  invariant(context != null, '`useAuth` must be used within an `<AuthProvider />`.')

  return context
}

/** A React Router layout route containing routes only accessible by users that are logged in. */
export function AnyLoggedInUserLayout() {
  const { session } = useAuth()

  if (session == null) {
    return <Navigate to={LOGIN_PATH} />
  }

  return <Outlet context={session} />
}

/** A React Router layout route containing routes only accessible by users that are logged in. */
export function ProtectedLayout() {
  const { session } = useAuth()

  if (session == null) {
    return <Navigate to={LOGIN_PATH} />
  }

  if (session.type === UserSessionType.partial) {
    return <Navigate to={SETUP_PATH} />
  }

  return (
    <>
      {/* This div is used as a flag to indicate that the dashboard has been loaded and the user is authenticated. */}
      {/* also it guarantees that the top-level suspense boundary is already resolved */}
      <div data-testid="after-auth-layout" aria-hidden />

      <Outlet context={session} />

      <Suspense fallback={null}>
        <ErrorBoundary fallbackRender={() => null}>
          <EnsoDevtools />
        </ErrorBoundary>
      </Suspense>
    </>
  )
}

/**
 * A React Router layout route containing routes only accessible by users that are
 * in the process of registering.
 */
export function SemiProtectedLayout() {
  const { session } = useAuth()
  const { localStorage } = useLocalStorage()

  // The user is not logged in - redirect to the login page.
  if (session == null) {
    return <Navigate to={LOGIN_PATH} replace />
  }

  // User is registered, redirect to dashboard or to the redirect path specified during the registration / login.
  if (session.type === UserSessionType.full) {
    return <Navigate to={localStorage.consume('loginRedirect') ?? DASHBOARD_PATH} replace />
  }

  // User is in the process of registration, allow them to complete the registration.
  return <Outlet context={session} />
}

/**
 * A React Router layout route containing routes only accessible by users that are
 * not logged in.
 */
export function GuestLayout() {
  const { session } = useAuth()
  const loginRedirect = useLoginRedirect()

  if (session?.type === UserSessionType.partial) {
    return <Navigate to={SETUP_PATH} />
  } else if (session?.type === UserSessionType.full) {
    const redirectTo = loginRedirect.get()
    if (redirectTo != null) {
      loginRedirect.delete()
      location.href = redirectTo
      return
    } else {
      return <Navigate to={DASHBOARD_PATH} />
    }
  } else {
    return (
      <>
        {/* This div is used as a flag to indicate that the user is not logged in. */}
        {/* also it guarantees that the top-level suspense boundary is already resolved */}
        <div data-testid="before-auth-layout" aria-hidden />
        <Outlet />
      </>
    )
  }
}

/** A React Router layout route containing routes only accessible by users that are not deleted. */
export function NotDeletedUserLayout() {
  const { session, isUserMarkedForDeletion } = useAuth()

  if (isUserMarkedForDeletion()) {
    return <Navigate to={RESTORE_USER_PATH} />
  } else {
    return <Outlet context={session} />
  }
}

/** A React Router layout route containing routes only accessible by users that are deleted softly. */
export function SoftDeletedUserLayout() {
  const { session, isUserMarkedForDeletion, isUserDeleted, isUserSoftDeleted } = useAuth()

  if (isUserMarkedForDeletion()) {
    const isSoftDeleted = isUserSoftDeleted()
    const isDeleted = isUserDeleted()
    if (isSoftDeleted) {
      return <Outlet context={session} />
    } else if (isDeleted) {
      return <Navigate to={LOGIN_PATH} />
    } else {
      return <Navigate to={DASHBOARD_PATH} />
    }
  }
}

const DEFAULT_REDIRECT_DELAY_MS = 3_000

/** Props for a {@link CloudBrowserDisabledLayout}. */
export interface CloudBrowserDisabledLayoutProps {
  /** The delay in milliseconds before redirecting to the desktop edition. */
  readonly redirectDelayMs?: number
  /** The path to redirect to if the user is not a full user. */
  readonly redirectPath?: string
}

/** Layout that disables the dashboard if the cloud is disabled. */
export function CloudBrowserDisabledLayout(props: CloudBrowserDisabledLayoutProps) {
  const { redirectDelayMs = DEFAULT_REDIRECT_DELAY_MS, redirectPath = '' } = props
  const { session } = useAuth()
  const { getText } = useText()
  const isCloudExecutionEnabled = useFeatureFlag('enableCloudExecution')
  const [isRedirecting, setIsRedirecting] = useState(true)

  const normalizedRedirectPath = redirectPath.startsWith('/') ? redirectPath.slice(1) : redirectPath

  const path = OPEN_IDE_DEEPLINK + normalizedRedirectPath

  useTimeoutCallback({
    callback: () => {
      unsafeWriteValue(window.location, 'href', path)
      setIsRedirecting(false)
    },
    ms: redirectDelayMs,
    isDisabled: isCloudExecutionEnabled,
  })

  if (isCloudExecutionEnabled) {
    return <Outlet context={session} />
  }

  return (
    <Page>
      <Result
        status={isRedirecting ? 'loading' : 'info'}
        title={getText('cloudBrowserDisabledTitle')}
        subtitle={getText('cloudBrowserDisabledSubtitle')}
      >
        <Button.Group align="center" verticalAlign="center">
          <Button variant="primary" href={path}>
            {getText('openInDesktop')}
          </Button>

          <Text>{getText('or')}</Text>

          <Button
            variant="outline"
            onPress={async () => {
              const downloadUrl = await getDownloadUrl()

              if (downloadUrl != null) {
                download(downloadUrl)
              }
            }}
          >
            {getText('downloadIDE')}
          </Button>
        </Button.Group>
      </Result>
    </Page>
  )
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

/** A React context hook returning the user session for a user that may or may not be logged in. */
export function useUserSession() {
  return useAuth().session
}

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
