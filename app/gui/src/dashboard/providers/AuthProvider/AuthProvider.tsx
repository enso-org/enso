/**
 * @file Module for authenticating users with AWS Cognito.
 *
 * Provides an `AuthProvider` component that wraps the entire application, and a `useAuth` hook that
 * can be used from any React component to access the currently logged-in user's session data. The
 * hook also provides methods for registering a user, logging in, logging out, etc.
 */
import * as React from 'react'

import * as sentry from '@sentry/vue'
import * as reactQuery from '@tanstack/react-query'
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
import { Button, Text } from '#/components/AriaComponents'
import { EnsoDevtools } from '#/components/Devtools/EnsoDevtools'
import Page from '#/components/Page'
import { Result } from '#/components/Result'
import { useTimeoutCallback } from '#/hooks/timeoutHooks'
import {
  featureFlagsForInternalTesting,
  useFeatureFlag,
  useSetFeatureFlags,
} from '#/providers/FeatureFlagsProvider'
import { isOrganizationId } from '#/services/RemoteBackend'
import { download } from '#/utilities/download'
import { getDownloadUrl } from '#/utilities/github'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { unsafeWriteValue } from '#/utilities/write'
import { useRouterInReact } from '$/providers/react'
import { Suspense } from 'react'
import { ErrorBoundary } from 'react-error-boundary'
import { AuthContext, useAuth } from './hooks'
import type { AuthContextType } from './types'
import { UserSessionType, type FullUserSession, type PartialUserSession } from './types'

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
export function AuthProvider(props: AuthProviderProps) {
  const { onAuthenticated, children } = props

  const remoteBackend = backendProvider.useRemoteBackend()
  const setFeatureFlags = useSetFeatureFlags()

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

  const createUserMutation = useMutationCallback({
    mutationFn: (user: backendModule.CreateUserRequestBody) => remoteBackend.createUser(user),
    meta: { invalidates: [usersMeQueryOptions.queryKey], awaitInvalidates: true },
  })

  const deleteUserMutation = useMutationCallback({
    mutationFn: () => remoteBackend.deleteUser(),
    meta: { invalidates: [usersMeQueryOptions.queryKey], awaitInvalidates: true },
  })

  const restoreUserMutation = useMutationCallback({
    mutationFn: () => remoteBackend.restoreUser(),
    meta: { invalidates: [usersMeQueryOptions.queryKey], awaitInvalidates: true },
  })

  const updateUserMutation = useMutationCallback({
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
      await updateUserMutation({ username })
    } else {
      const orgId = await organizationId()
      const email = session?.email ?? ''

      invariant(orgId == null || isOrganizationId(orgId), 'Invalid organization ID')

      await createUserMutation({
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
    await deleteUserMutation()
    await signOut()

    toastSuccess(getText('deleteUserSuccess'))

    return true
  })

  const restoreUser = useEventCallback(async () => {
    await restoreUserMutation()

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

  React.useEffect(() => {
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

/**
 * A React Router layout route containing routes only accessible by users that are logged in.
 */
export function AnyLoggedInUserLayout({ children }: React.PropsWithChildren) {
  const { session } = useAuth()
  const { router } = useRouterInReact()

  if (session == null) {
    void router.push(appUtils.LOGIN_PATH)
    return
  }

  return <>{children}</>
}

/** A React Router layout route containing routes only accessible by users that are logged in. */
export function ProtectedLayout({ children }: React.PropsWithChildren<object>) {
  const { session } = useAuth()
  const { router } = useRouterInReact()

  if (session == null) {
    void router.push(appUtils.LOGIN_PATH)
    return
  }

  if (session.type === UserSessionType.partial) {
    void router.push(appUtils.SETUP_PATH)
    return
  }

  return (
    <>
      {/* This div is used as a flag to indicate that the dashboard has been loaded and the user is authenticated. */}
      {/* also it guarantees that the top-level suspense boundary is already resolved */}
      <div data-testid="after-auth-layout" aria-hidden />

      {children}

      <Suspense fallback={null}>
        <ErrorBoundary fallbackRender={() => null}>
          <EnsoDevtools />
        </ErrorBoundary>
      </Suspense>
    </>
  )
}

/**
 * A React Router layout route containing routes only accessible by users that are logged in.
 */
/**
 * A React Router layout route containing routes only accessible by users that are
 * in the process of registering.
 */
export function SemiProtectedLayout({ children }: React.PropsWithChildren) {
  const { session } = useAuth()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { router } = useRouterInReact()

  // The user is not logged in - redirect to the login page.
  if (session == null) {
    void router.replace(appUtils.LOGIN_PATH)
    return
  }

  // User is registered, redirect to dashboard or to the redirect path specified during the registration / login.
  if (session.type === UserSessionType.full) {
    void router.replace(localStorage.consume('loginRedirect') ?? appUtils.DASHBOARD_PATH)
    return
  }

  // User is in the process of registration, allow them to complete the registration.
  return <>{children}</>
}

/**
 * A React Router layout route containing routes only accessible by users that are
 * not logged in.
 */
export function GuestLayout({ children }: React.PropsWithChildren) {
  const { session } = useAuth()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { router } = useRouterInReact()

  if (session?.type === UserSessionType.partial) {
    void router.push(appUtils.SETUP_PATH)
    return
  } else if (session?.type === UserSessionType.full) {
    const redirectTo = localStorage.get('loginRedirect')
    if (redirectTo != null) {
      localStorage.delete('loginRedirect')
      void router.push(redirectTo)
      return
    } else {
      void router.push(appUtils.DASHBOARD_PATH)
      return
    }
  } else {
    return (
      <>
        {/* This div is used as a flag to indicate that the user is not logged in. */}
        {/* also it guarantees that the top-level suspense boundary is already resolved */}
        <div data-testid="before-auth-layout" aria-hidden />
        {children}
      </>
    )
  }
}

/** A React Router layout route containing routes only accessible by users that are not deleted. */
export function NotDeletedUserLayout({ children }: React.PropsWithChildren) {
  const { isUserMarkedForDeletion } = useAuth()
  const { router } = useRouterInReact()

  if (isUserMarkedForDeletion()) {
    void router.push(appUtils.RESTORE_USER_PATH)
  } else {
    return <>{children}</>
  }
}

/** A React Router layout route containing routes only accessible by users that are deleted softly. */
export function SoftDeletedUserLayout({ children }: React.PropsWithChildren) {
  const { isUserMarkedForDeletion, isUserDeleted, isUserSoftDeleted } = useAuth()
  const { router } = useRouterInReact()

  if (isUserMarkedForDeletion()) {
    const isSoftDeleted = isUserSoftDeleted()
    const isDeleted = isUserDeleted()
    if (isSoftDeleted) {
      return <>{children}</>
    } else if (isDeleted) {
      void router.push(appUtils.LOGIN_PATH)
      return
    } else {
      void router.push(appUtils.DASHBOARD_PATH)
      return
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

/**
 * Layout that disables the dashboard if the cloud is disabled.
 */
export function CloudBrowserDisabledLayout(
  props: React.PropsWithChildren<CloudBrowserDisabledLayoutProps>,
) {
  const { children, redirectDelayMs = DEFAULT_REDIRECT_DELAY_MS, redirectPath = '' } = props
  const { getText } = textProvider.useText()
  const isCloudExecutionEnabled = useFeatureFlag('enableCloudExecution')
  const [isRedirecting, setIsRedirecting] = React.useState(true)

  const normalizedRedirectPath = redirectPath.startsWith('/') ? redirectPath.slice(1) : redirectPath

  const path = appUtils.OPEN_IDE_DEEPLINK + normalizedRedirectPath

  useTimeoutCallback({
    callback: () => {
      unsafeWriteValue(window.location, 'href', path)
      setIsRedirecting(false)
    },
    ms: redirectDelayMs,
    isDisabled: isCloudExecutionEnabled,
  })

  if (isCloudExecutionEnabled) {
    return <>{children}</>
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
                void download({ url: downloadUrl })
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
