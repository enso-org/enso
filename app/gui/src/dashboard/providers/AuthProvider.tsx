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
import * as gtag from 'enso-common/src/gtag'

import * as appUtils from '#/appUtils'

import { useEventCallback } from '#/hooks/eventCallbackHooks'
import * as gtagHooks from '#/hooks/gtagHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as sessionProvider from '#/providers/SessionProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import * as resultComponent from '#/components/Result'

import * as backendModule from '#/services/Backend'
import type RemoteBackend from '#/services/RemoteBackend'

import * as errorModule from '#/utilities/error'

import * as cognitoModule from '#/authentication/cognito'
import type * as authServiceModule from '#/authentication/service'

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
  readonly signUp: (email: string, password: string, organizationId: string | null) => Promise<void>
  readonly authQueryKey: reactQuery.QueryKey
  readonly confirmSignUp: (email: string, code: string) => Promise<void>
  readonly setUsername: (username: string) => Promise<boolean>
  readonly signInWithGoogle: () => Promise<boolean>
  readonly signInWithGitHub: () => Promise<boolean>
  readonly signInWithPassword: (
    email: string,
    password: string,
  ) => Promise<{
    readonly challenge: cognitoModule.UserSessionChallenge
    readonly user: cognitoModule.CognitoUser
  }>
  readonly forgotPassword: (email: string) => Promise<void>
  readonly changePassword: (oldPassword: string, newPassword: string) => Promise<boolean>
  readonly resetPassword: (email: string, code: string, password: string) => Promise<void>
  readonly signOut: () => Promise<void>
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
  readonly cognito: cognitoModule.Cognito
}

const AuthContext = React.createContext<AuthContextType | null>(null)

// ====================
// === AuthProvider ===
// ====================

/** Query to fetch the user's session data from the backend. */
function createUsersMeQuery(
  session: cognitoModule.UserSession | null,
  remoteBackend: RemoteBackend,
  performLogout: () => Promise<void>,
) {
  return reactQuery.queryOptions({
    queryKey: [remoteBackend.type, 'usersMe', session?.clientId] as const,
    queryFn: async () => {
      if (session == null) {
        return Promise.resolve(null)
      }
      try {
        const user = await remoteBackend.usersMe()

        // if API returns null, user is not yet registered
        // but already authenticated with Cognito
        return user == null ?
            ({ type: UserSessionType.partial, ...session } satisfies PartialUserSession)
          : ({ type: UserSessionType.full, user, ...session } satisfies FullUserSession)
      } catch (error) {
        if (error instanceof backendModule.NotAuthorizedError) {
          await performLogout()
          return null
        } else {
          throw error
        }
      }
    },
  })
}

/** Props for an {@link AuthProvider}. */
export interface AuthProviderProps {
  readonly shouldStartInOfflineMode: boolean
  readonly authService: authServiceModule.AuthService
  /** Callback to execute once the user has authenticated successfully. */
  readonly onAuthenticated: (accessToken: string | null) => void
  readonly children: React.ReactNode
}

/** A React provider for the Cognito API. */
export default function AuthProvider(props: AuthProviderProps) {
  const { authService, onAuthenticated } = props
  const { children } = props
  const remoteBackend = backendProvider.useRemoteBackend()
  const { cognito } = authService
  const { session, sessionQueryKey } = sessionProvider.useSession()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { getText } = textProvider.useText()
  const { unsetModal } = modalProvider.useSetModal()
  const navigate = router.useNavigate()
  const toastId = React.useId()

  const queryClient = reactQuery.useQueryClient()

  // This component cannot use `useGtagEvent` because `useGtagEvent` depends on the React Context
  // defined by this component.
  const gtagEvent = React.useCallback((name: string, params?: object) => {
    gtag.event(name, params)
  }, [])

  const performLogout = async () => {
    await cognito.signOut()

    const parentDomain = location.hostname.replace(/^[^.]*\./, '')
    document.cookie = `logged_in=no;max-age=0;domain=${parentDomain}`
    gtagEvent('cloud_sign_out')
    cognito.saveAccessToken(null)
    localStorage.clearUserSpecificEntries()
    sentry.setUser(null)

    await queryClient.invalidateQueries({ queryKey: sessionQueryKey })
    await queryClient.clearWithPersister()

    return Promise.resolve()
  }

  const logoutMutation = reactQuery.useMutation({
    mutationKey: [remoteBackend.type, 'usersMe', 'logout', session?.clientId] as const,
    mutationFn: performLogout,
    // If the User Menu is still visible, it breaks when `userSession` is set to `null`.
    onMutate: unsetModal,
    onSuccess: () => toast.toast.success(getText('signOutSuccess')),
    onError: () => toast.toast.error(getText('signOutError')),
    meta: { invalidates: [sessionQueryKey], awaitInvalidates: true },
  })

  const usersMeQueryOptions = createUsersMeQuery(session, remoteBackend, async () => {
    await performLogout()
    toast.toast.info(getText('userNotAuthorizedError'))
  })

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

  /**
   * Wrap a function returning a {@link Promise} to display a loading toast notification
   * until the returned {@link Promise} finishes loading.
   */
  const withLoadingToast =
    <T extends unknown[], R>(action: (...args: T) => Promise<R>) =>
    async (...args: T) => {
      toast.toast.loading(getText('pleaseWait'), { toastId })
      return await action(...args)
    }

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

  const toastError = (message: string) => {
    toast.toast.update(toastId, {
      isLoading: null,
      autoClose: null,
      closeOnClick: null,
      closeButton: null,
      draggable: null,
      type: toast.toast.TYPE.ERROR,
      render: message,
    })
  }

  const signUp = useEventCallback(
    async (username: string, password: string, organizationId: string | null) => {
      gtagEvent('cloud_sign_up')
      const result = await cognito.signUp(username, password, organizationId)

      if (result.err) {
        throw new Error(result.val.message)
      } else {
        return
      }
    },
  )

  const confirmSignUp = useEventCallback(async (email: string, code: string) => {
    gtagEvent('cloud_confirm_sign_up')
    const result = await cognito.confirmSignUp(email, code)

    if (result.err) {
      switch (result.val.type) {
        case cognitoModule.CognitoErrorType.userAlreadyConfirmed:
        case cognitoModule.CognitoErrorType.userNotFound: {
          return
        }
        default: {
          throw new errorModule.UnreachableCaseError(result.val.type)
        }
      }
    }
  })

  const signInWithPassword = useEventCallback(async (email: string, password: string) => {
    gtagEvent('cloud_sign_in', { provider: 'Email' })

    const result = await cognito.signInWithPassword(email, password)

    if (result.ok) {
      const user = result.unwrap()

      const challenge = user.challengeName ?? 'NO_CHALLENGE'

      if (['SMS_MFA', 'SOFTWARE_TOKEN_MFA'].includes(challenge)) {
        return { challenge, user } as const
      }

      return queryClient
        .invalidateQueries({ queryKey: sessionQueryKey })
        .then(() => ({ challenge, user }) as const)
    } else {
      throw new Error(result.val.message)
    }
  })

  const refetchSession = usersMeQuery.refetch

  const setUsername = useEventCallback(async (username: string) => {
    gtagEvent('cloud_user_created')

    if (userData?.type === UserSessionType.full) {
      await updateUserMutation.mutateAsync({ username })
    } else {
      const organizationId = await cognito.organizationId()
      const email = session?.email ?? ''

      await createUserMutation.mutateAsync({
        userName: username,
        userEmail: backendModule.EmailAddress(email),
        organizationId:
          organizationId != null ? backendModule.OrganizationId(organizationId) : null,
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

  const forgotPassword = useEventCallback(async (email: string) => {
    const result = await cognito.forgotPassword(email)
    if (result.ok) {
      navigate(appUtils.LOGIN_PATH)
      return
    } else {
      throw new Error(result.val.message)
    }
  })

  const resetPassword = useEventCallback(async (email: string, code: string, password: string) => {
    const result = await cognito.forgotPasswordSubmit(email, code, password)

    if (result.ok) {
      navigate(appUtils.LOGIN_PATH)
      return
    } else {
      throw new Error(result.val.message)
    }
  })

  const changePassword = useEventCallback(async (oldPassword: string, newPassword: string) => {
    const result = await cognito.changePassword(oldPassword, newPassword)

    if (result.ok) {
      toastSuccess(getText('changePasswordSuccess'))
    } else {
      toastError(result.val.message)
    }

    return result.ok
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
    gtag.gtag('set', { platform: detect.platform(), architecture: detect.architecture() })
    return gtagHooks.gtagOpenCloseCallback(gtagEvent, 'open_app', 'close_app')
  }, [gtagEvent])

  React.useEffect(() => {
    if (userData?.type === UserSessionType.full) {
      onAuthenticated(userData.accessToken)
    }
  }, [userData, onAuthenticated])

  const value: AuthContextType = {
    signUp,
    confirmSignUp,
    setUsername,
    isUserMarkedForDeletion,
    isUserDeleted,
    isUserSoftDeleted,
    restoreUser,
    deleteUser,
    cognito,
    signInWithGoogle: useEventCallback(() => {
      gtagEvent('cloud_sign_in', { provider: 'Google' })

      return cognito
        .signInWithGoogle()
        .then(() => queryClient.invalidateQueries({ queryKey: sessionQueryKey }))
        .then(
          () => true,
          () => false,
        )
    }),
    signInWithGitHub: useEventCallback(() => {
      gtagEvent('cloud_sign_in', { provider: 'GitHub' })

      return cognito
        .signInWithGitHub()
        .then(() => queryClient.invalidateQueries({ queryKey: sessionQueryKey }))
        .then(
          () => true,
          () => false,
        )
    }),
    signInWithPassword,
    forgotPassword,
    resetPassword,
    changePassword: withLoadingToast(changePassword),
    refetchSession,
    session: userData,
    signOut: logoutMutation.mutateAsync,
    setUser,
    authQueryKey: usersMeQueryOptions.queryKey,
  }

  return (
    <AuthContext.Provider value={value}>
      {children}

      <ariaComponents.Dialog
        aria-label={getText('loggingOut')}
        isDismissable={false}
        isKeyboardDismissDisabled
        hideCloseButton
        modalProps={{ isOpen: logoutMutation.isPending }}
      >
        <resultComponent.Result status="loading" title={getText('loggingOut')} />
      </ariaComponents.Dialog>
    </AuthContext.Provider>
  )
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
    return <router.Outlet context={session} />
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
    return <router.Outlet />
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
