/** @file Module for authenticating users with AWS Cognito.
 *
 * Provides an `AuthProvider` component that wraps the entire application, and a `useAuth` hook that
 * can be used from any React component to access the currently logged-in user's session data. The
 * hook also provides methods for registering a user, logging in, logging out, etc. */
import * as React from 'react'

import * as sentry from '@sentry/react'
import * as reactQuery from '@tanstack/react-query'
import * as router from 'react-router-dom'
import invariant from 'tiny-invariant'

import * as detect from 'enso-common/src/detect'
import * as gtag from 'enso-common/src/gtag'

import * as appUtils from '#/appUtils'

import * as gtagHooks from '#/hooks/gtagHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as sessionProvider from '#/providers/SessionProvider'
import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'
import * as resultComponent from '#/components/Result'
import * as toast from '#/components/Toast'

import type Backend from '#/services/Backend'
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
interface BaseUserSession {
  /** A discriminator for TypeScript to be able to disambiguate between `UserSession` variants. */
  readonly type: UserSessionType
  /** User's JSON Web Token (JWT), used for authenticating and authorizing requests to the API. */
  readonly accessToken: string
  /** User's email address. */
  readonly email: string
}

/** Object containing the currently signed-in user's session data, if the user has not yet set their
 * username.
 *
 * If a user has not yet set their username, they do not yet have an organization associated with
 * their account. Otherwise, this type is identical to the `Session` type. This type should ONLY be
 * used by the `SetUsername` component. */
export interface PartialUserSession extends BaseUserSession {
  readonly type: UserSessionType.partial
}

/** Object containing the currently signed-in user's session data. */
export interface FullUserSession extends BaseUserSession {
  /** User's organization information. */
  readonly type: UserSessionType.full
  readonly user: backendModule.User
}

/** A user session for a user that may be either fully registered,
 * or in the process of registering. */
export type UserSession = FullUserSession | PartialUserSession

// ===================
// === AuthContext ===
// ===================

/** Interface returned by the `useAuth` hook.
 *
 * Contains the currently authenticated user's session data, as well as methods for signing in,
 * signing out, etc. All interactions with the authentication API should be done through this
 * interface.
 *
 * See `Cognito` for details on each of the authentication functions. */
interface AuthContextType {
  readonly signUp: (
    email: string,
    password: string,
    organizationId: string | null
  ) => Promise<boolean>
  readonly authQueryKey: reactQuery.QueryKey
  readonly confirmSignUp: (email: string, code: string) => Promise<boolean>
  readonly setUsername: (backend: Backend, username: string, email: string) => Promise<boolean>
  readonly signInWithGoogle: () => Promise<boolean>
  readonly signInWithGitHub: () => Promise<boolean>
  readonly signInWithPassword: (email: string, password: string) => Promise<boolean>
  readonly forgotPassword: (email: string) => Promise<boolean>
  readonly changePassword: (oldPassword: string, newPassword: string) => Promise<boolean>
  readonly resetPassword: (email: string, code: string, password: string) => Promise<boolean>
  readonly signOut: () => Promise<void>
  /**
   * @deprecated Never use this function. Prefer particular functions like `setUsername` or `deleteUser`.
   */
  readonly setUser: (user: Partial<backendModule.User>) => void
  readonly deleteUser: () => Promise<boolean>
  readonly restoreUser: () => Promise<boolean>
  /** Session containing the currently authenticated user's authentication information.
   *
   * If the user has not signed in, the session will be `null`. */
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

/**
 * Query to fetch the user's session data from the backend.
 */
function createUsersMeQuery(
  session: cognitoModule.UserSession | null,
  remoteBackend: RemoteBackend
) {
  return reactQuery.queryOptions({
    queryKey: ['usersMe', session?.clientId] as const,
    queryFn: async () => {
      if (session == null) {
        // eslint-disable-next-line no-restricted-syntax
        return null
      }

      const user = await remoteBackend.usersMe()

      // if API returns null, user is not yet registered
      // but already authenticated with Cognito
      return user == null
        ? ({ type: UserSessionType.partial, ...session } satisfies PartialUserSession)
        : ({ type: UserSessionType.full, user, ...session } satisfies FullUserSession)
    },
  })
}

/** Props for an {@link AuthProvider}. */
export interface AuthProviderProps {
  readonly shouldStartInOfflineMode: boolean
  readonly authService: authServiceModule.AuthService | null
  /** Callback to execute once the user has authenticated successfully. */
  readonly onAuthenticated: (accessToken: string | null) => void
  readonly children: React.ReactNode
}

/** A React provider for the Cognito API. */
export default function AuthProvider(props: AuthProviderProps) {
  const { authService, onAuthenticated } = props
  const { children } = props
  const remoteBackend = backendProvider.useRemoteBackendStrict()
  const { cognito } = authService ?? {}
  const { session, sessionQueryKey } = sessionProvider.useSession()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { getText } = textProvider.useText()
  const { unsetModal } = modalProvider.useSetModal()
  // This must not be `hooks.useNavigate` as `goOffline` would be inaccessible,
  // and the function call would error.
  // eslint-disable-next-line no-restricted-properties
  const navigate = router.useNavigate()
  const toastId = React.useId()

  const queryClient = reactQuery.useQueryClient()

  const usersMeQuery = createUsersMeQuery(session, remoteBackend)

  const { data: userData } = reactQuery.useSuspenseQuery(usersMeQuery)

  const createUserMutation = reactQuery.useMutation({
    mutationFn: (user: backendModule.CreateUserRequestBody) => remoteBackend.createUser(user),
    meta: { invalidates: [usersMeQuery.queryKey], awaitInvalidates: true },
  })

  const deleteUserMutation = reactQuery.useMutation({
    mutationFn: () => remoteBackend.deleteUser(),
    meta: { invalidates: [usersMeQuery.queryKey], awaitInvalidates: true },
  })

  const restoreUserMutation = reactQuery.useMutation({
    mutationFn: () => remoteBackend.restoreUser(),
    meta: { invalidates: [usersMeQuery.queryKey], awaitInvalidates: true },
  })

  const logoutMutation = reactQuery.useMutation({
    mutationFn: () => (cognito != null ? cognito.signOut() : Promise.reject()),
    onMutate: () => {
      // If the User Menu is still visible, it breaks when `userSession` is set to `null`.
      unsetModal()
    },
    onSuccess: async () => {
      const parentDomain = location.hostname.replace(/^[^.]*\./, '')
      document.cookie = `logged_in=no;max-age=0;domain=${parentDomain}`
      gtagEvent('cloud_sign_out')
      cognito?.saveAccessToken(null)
      localStorage.clearUserSpecificEntries()
      sentry.setUser(null)

      await queryClient.clearWithPersister()

      return toast.toast.success(getText('signOutSuccess'))
    },
    onError: () => toast.toast.error(getText('signOutError')),
    meta: { invalidates: [sessionQueryKey], awaitInvalidates: true },
  })

  // This component cannot use `useGtagEvent` because `useGtagEvent` depends on the React Context
  // defined by this component.
  const gtagEvent = React.useCallback((name: string, params?: object) => {
    gtag.event(name, params)
  }, [])

  /** Wrap a function returning a {@link Promise} to display a loading toast notification
   * until the returned {@link Promise} finishes loading. */
  const withLoadingToast =
    <T extends unknown[], R>(action: (...args: T) => Promise<R>) =>
    async (...args: T) => {
      toast.toast.loading(getText('pleaseWait'), { toastId })
      return await action(...args)
    }

  const toastSuccess = (message: string) => {
    toast.toast.success(message, {
      toastId,
      duration: Infinity,
      closeButton: false,
      dismissible: false,
    })
  }

  const toastError = (message: string) => {
    toast.toast.error(message, {
      toastId,
      duration: Infinity,
      closeButton: false,
      dismissible: false,
    })
  }

  const signUp = async (username: string, password: string, organizationId: string | null) => {
    if (cognito == null) {
      return false
    } else {
      gtagEvent('cloud_sign_up')
      const result = await cognito.signUp(username, password, organizationId)
      if (result.ok) {
        toastSuccess(getText('signUpSuccess'))
        navigate(appUtils.LOGIN_PATH)
      } else {
        toastError(result.val.message)
      }
      return result.ok
    }
  }

  const confirmSignUp = async (email: string, code: string) => {
    if (cognito == null) {
      return false
    } else {
      gtagEvent('cloud_confirm_sign_up')
      const result = await cognito.confirmSignUp(email, code)
      if (result.err) {
        switch (result.val.type) {
          case cognitoModule.CognitoErrorType.userAlreadyConfirmed: {
            break
          }
          case cognitoModule.CognitoErrorType.userNotFound: {
            toastError(getText('confirmSignUpError'))
            navigate(appUtils.LOGIN_PATH)
            return false
          }
          default: {
            throw new errorModule.UnreachableCaseError(result.val.type)
          }
        }
      }
      toastSuccess(getText('confirmSignUpSuccess'))
      navigate(appUtils.LOGIN_PATH)
      return result.ok
    }
  }

  const signInWithPassword = async (email: string, password: string) => {
    if (cognito == null) {
      return false
    } else {
      gtagEvent('cloud_sign_in', { provider: 'Email' })
      const result = await cognito.signInWithPassword(email, password)
      if (result.ok) {
        toastSuccess(getText('signInWithPasswordSuccess'))
        void queryClient.invalidateQueries({ queryKey: sessionQueryKey })
      } else {
        if (result.val.type === cognitoModule.CognitoErrorType.userNotFound) {
          // It may not be safe to pass the user's password in the URL.
          navigate(`${appUtils.REGISTRATION_PATH}?${new URLSearchParams({ email }).toString()}`)
        }
        toastError(result.val.message)
      }
      return result.ok
    }
  }

  const setUsername = async (backend: Backend, username: string, email: string) => {
    if (cognito == null) {
      return false
    } else if (backend.type === backendModule.BackendType.local) {
      toastError(getText('setUsernameLocalBackendError'))
      return false
    } else {
      gtagEvent('cloud_user_created')

      try {
        const organizationId = await cognito.organizationId()
        // This should not omit success and error toasts as it is not possible
        // to render this optimistically.
        toast.toast.promise(
          createUserMutation.mutateAsync({
            userName: username,
            userEmail: backendModule.EmailAddress(email),
            organizationId:
              organizationId != null ? backendModule.OrganizationId(organizationId) : null,
          }),
          {
            success: getText('setUsernameSuccess'),
            error: getText('setUsernameError'),
            loading: getText('settingUsername'),
          }
        )
        const redirectTo = localStorage.get('loginRedirect')
        if (redirectTo != null) {
          localStorage.delete('loginRedirect')
          location.href = redirectTo
        } else {
          navigate(appUtils.DASHBOARD_PATH)
        }
        return true
      } catch {
        return false
      }
    }
  }

  const deleteUser = async () => {
    if (cognito == null) {
      return false
    } else {
      await deleteUserMutation.mutateAsync()

      toastSuccess(getText('deleteUserSuccess'))

      return true
    }
  }

  const restoreUser = async () => {
    if (cognito == null) {
      return false
    } else {
      await restoreUserMutation.mutateAsync()

      toastSuccess(getText('restoreUserSuccess'))

      return true
    }
  }

  /**
   * Update the user session data in the React Query cache.
   * This only works for full user sessions.
   * @deprecated Never use this function. Prefer particular functions like `setUsername` or `deleteUser`.
   */
  const setUser = (user: Partial<backendModule.User>) => {
    const currentUser = queryClient.getQueryData(usersMeQuery.queryKey)

    if (currentUser != null && currentUser.type === UserSessionType.full) {
      const currentUserData = currentUser.user
      const nextUserData: backendModule.User = Object.assign(currentUserData, user)

      queryClient.setQueryData(usersMeQuery.queryKey, { ...currentUser, user: nextUserData })

      void queryClient.invalidateQueries({ queryKey: usersMeQuery.queryKey })
    }
  }

  const forgotPassword = async (email: string) => {
    if (cognito == null) {
      return false
    } else {
      const result = await cognito.forgotPassword(email)
      if (result.ok) {
        toastSuccess(getText('forgotPasswordSuccess'))
        navigate(appUtils.LOGIN_PATH)
      } else {
        toastError(result.val.message)
      }
      return result.ok
    }
  }

  const resetPassword = async (email: string, code: string, password: string) => {
    if (cognito == null) {
      return false
    } else {
      const result = await cognito.forgotPasswordSubmit(email, code, password)
      if (result.ok) {
        toastSuccess(getText('resetPasswordSuccess'))
        navigate(appUtils.LOGIN_PATH)
      } else {
        toastError(result.val.message)
      }
      return result.ok
    }
  }

  const changePassword = async (oldPassword: string, newPassword: string) => {
    if (cognito == null) {
      return false
    } else {
      const result = await cognito.changePassword(oldPassword, newPassword)
      if (result.ok) {
        toastSuccess(getText('changePasswordSuccess'))
      } else {
        toastError(result.val.message)
      }
      return result.ok
    }
  }

  const isUserMarkedForDeletion = () => !!(userData && 'user' in userData && userData.user.removeAt)

  const isUserDeleted = () => {
    if (userData && 'user' in userData && userData.user.removeAt) {
      const removeAtDate = new Date(userData.user.removeAt)
      const now = new Date()

      return removeAtDate <= now
    } else {
      return false
    }
  }

  const isUserSoftDeleted = () => {
    if (userData && 'user' in userData && userData.user.removeAt) {
      const removeAtDate = new Date(userData.user.removeAt)
      const now = new Date()

      return removeAtDate > now
    } else {
      return false
    }
  }

  React.useEffect(() => {
    if (userData?.type === UserSessionType.full) {
      sentry.setUser({
        id: userData.user.userId,
        email: userData.email,
        username: userData.user.name,
        // eslint-disable-next-line @typescript-eslint/naming-convention
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
    return gtagHooks.gtagOpenCloseCallback({ current: gtagEvent }, 'open_app', 'close_app')
  }, [gtagEvent])

  React.useEffect(() => {
    if (userData?.type === UserSessionType.full) {
      onAuthenticated(userData.accessToken)
    }
  }, [userData, onAuthenticated])

  const value = {
    signUp: withLoadingToast(signUp),
    confirmSignUp: withLoadingToast(confirmSignUp),
    setUsername,
    isUserMarkedForDeletion,
    isUserDeleted,
    isUserSoftDeleted,
    restoreUser,
    deleteUser,
    signInWithGoogle: () => {
      if (cognito == null) {
        return Promise.resolve(false)
      } else {
        gtagEvent('cloud_sign_in', { provider: 'Google' })
        return cognito
          .signInWithGoogle()
          .then(() => queryClient.invalidateQueries({ queryKey: sessionQueryKey }))
          .then(
            () => true,
            () => false
          )
      }
    },
    signInWithGitHub: () => {
      if (cognito == null) {
        return Promise.resolve(false)
      } else {
        gtagEvent('cloud_sign_in', { provider: 'GitHub' })
        return cognito
          .signInWithGitHub()
          .then(() => queryClient.invalidateQueries({ queryKey: sessionQueryKey }))
          .then(
            () => true,
            () => false
          )
      }
    },
    signInWithPassword: signInWithPassword,
    forgotPassword: withLoadingToast(forgotPassword),
    resetPassword: withLoadingToast(resetPassword),
    changePassword: withLoadingToast(changePassword),
    session: userData,
    signOut: logoutMutation.mutateAsync,
    setUser,
    authQueryKey: usersMeQuery.queryKey,
  } satisfies AuthContextType

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

/** A React hook that provides access to the authentication context.
 *
 * Only the hook is exported, and not the context, because we only want to use the hook directly and
 * never the context component.
 * @throws {Error} when used outside a {@link AuthProvider}. */
export function useAuth() {
  const context = React.useContext(AuthContext)
  if (context == null) {
    throw new Error('`useAuth` can only be used inside an `<AuthProvider />`.')
  } else {
    return context
  }
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
    return <router.Navigate to={appUtils.SET_USERNAME_PATH} />
  } else {
    return <router.Outlet context={session} />
  }
}

// ===========================
// === SemiProtectedLayout ===
// ===========================

/** A React Router layout route containing routes only accessible by users that are
 * in the process of registering. */
export function SemiProtectedLayout() {
  const { session } = useAuth()
  const { localStorage } = localStorageProvider.useLocalStorage()

  if (session?.type === UserSessionType.full) {
    const redirectTo = localStorage.get('loginRedirect')
    if (redirectTo != null) {
      localStorage.delete('loginRedirect')
      location.href = redirectTo
      return
    } else {
      return <router.Navigate to={appUtils.DASHBOARD_PATH} />
    }
  } else {
    return <router.Outlet context={session} />
  }
}

// ===================
// === GuestLayout ===
// ===================

/** A React Router layout route containing routes only accessible by users that are
 * not logged in. */
export function GuestLayout() {
  const { session } = useAuth()
  const { localStorage } = localStorageProvider.useLocalStorage()

  if (session?.type === UserSessionType.partial) {
    return <router.Navigate to={appUtils.SET_USERNAME_PATH} />
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

/** A React context hook returning the user session
 * for a user that has not yet completed registration. */
export function usePartialUserSession() {
  return router.useOutletContext<PartialUserSession>()
}

// ================================
// === useNonPartialUserSession ===
// ================================

/** A React context hook returning the user session for a user that can perform actions. */
export function useNonPartialUserSession() {
  return router.useOutletContext<Exclude<UserSession, PartialUserSession>>()
}

// ======================
// === useUserSession ===
// ======================

/** A React context hook returning the user session for a user that may or may not be logged in. */
export function useUserSession() {
  return router.useOutletContext<UserSession | undefined>()
}

/**
 * A React context hook returning the user session for a user that is fully logged in.
 */
export function useFullUserSession(): FullUserSession {
  const session = router.useOutletContext<UserSession>()

  invariant(session.type === UserSessionType.full, 'Expected a full user session.')

  return session
}
