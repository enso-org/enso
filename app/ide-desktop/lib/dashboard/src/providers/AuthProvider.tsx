/** @file Module for authenticating users with AWS Cognito.
 *
 * Provides an `AuthProvider` component that wraps the entire application, and a `useAuth` hook that
 * can be used from any React component to access the currently logged-in user's session data. The
 * hook also provides methods for registering a user, logging in, logging out, etc. */
import * as React from 'react'

import * as sentry from '@sentry/react'
import isNetworkError from 'is-network-error'
import * as router from 'react-router-dom'
import * as toast from 'react-toastify'

import * as detect from 'enso-common/src/detect'
import * as gtag from 'enso-common/src/gtag'

import * as appUtils from '#/appUtils'

import * as gtagHooks from '#/hooks/gtagHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as localStorageProvider from '#/providers/LocalStorageProvider'
import * as loggerProvider from '#/providers/LoggerProvider'
import * as sessionProvider from '#/providers/SessionProvider'
import * as textProvider from '#/providers/TextProvider'

import LoadingScreen from '#/pages/authentication/LoadingScreen'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'
import LocalBackend from '#/services/LocalBackend'
import type * as projectManager from '#/services/ProjectManager'
import RemoteBackend from '#/services/RemoteBackend'

import * as errorModule from '#/utilities/error'
import HttpClient, * as httpClient from '#/utilities/HttpClient'
import * as object from '#/utilities/object'

import * as cognitoModule from '#/authentication/cognito'
import type * as authServiceModule from '#/authentication/service'

// =================
// === Constants ===
// =================

/** The minimum delay between two requests. */
const REQUEST_DELAY_MS = 200

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
interface BaseUserSession<Type extends UserSessionType> {
  /** A discriminator for TypeScript to be able to disambiguate between `UserSession` variants. */
  readonly type: Type
  /** User's JSON Web Token (JWT), used for authenticating and authorizing requests to the API. */
  readonly accessToken: string
  /** User's email address. */
  readonly email: string
}

// Extends `BaseUserSession` in order to inherit the documentation.
/** Empty object of an offline user session.
 * Contains some fields from {@link FullUserSession} to allow destructuring. */
export interface OfflineUserSession extends Pick<BaseUserSession<UserSessionType.offline>, 'type'> {
  readonly accessToken: null
  readonly user: null
}

/** The singleton instance of {@link OfflineUserSession}. Minimizes React re-renders. */
const OFFLINE_USER_SESSION: Readonly<OfflineUserSession> = {
  type: UserSessionType.offline,
  accessToken: null,
  user: null,
}

/** Object containing the currently signed-in user's session data, if the user has not yet set their
 * username.
 *
 * If a user has not yet set their username, they do not yet have an organization associated with
 * their account. Otherwise, this type is identical to the `Session` type. This type should ONLY be
 * used by the `SetUsername` component. */
export interface PartialUserSession extends BaseUserSession<UserSessionType.partial> {}

/** Object containing the currently signed-in user's session data. */
export interface FullUserSession extends BaseUserSession<UserSessionType.full> {
  /** User's organization information. */
  readonly user: backendModule.User
}

/** A user session for a user that may be either fully registered,
 * or in the process of registering. */
export type UserSession = FullUserSession | OfflineUserSession | PartialUserSession

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
  readonly goOffline: (shouldShowToast?: boolean) => Promise<boolean>
  readonly signUp: (
    email: string,
    password: string,
    organizationId: string | null
  ) => Promise<boolean>
  readonly confirmSignUp: (email: string, code: string) => Promise<boolean>
  readonly setUsername: (backend: Backend, username: string, email: string) => Promise<boolean>
  readonly signInWithGoogle: () => Promise<boolean>
  readonly signInWithGitHub: () => Promise<boolean>
  readonly signInWithPassword: (email: string, password: string) => Promise<boolean>
  readonly forgotPassword: (email: string) => Promise<boolean>
  readonly changePassword: (oldPassword: string, newPassword: string) => Promise<boolean>
  readonly resetPassword: (email: string, code: string, password: string) => Promise<boolean>
  readonly signOut: () => Promise<boolean>
  readonly restoreUser: (backend: Backend) => Promise<boolean>
  /** Session containing the currently authenticated user's authentication information.
   *
   * If the user has not signed in, the session will be `null`. */
  readonly session: UserSession | null
  readonly setUser: React.Dispatch<React.SetStateAction<backendModule.User>>
  /**
   * Return `true` if the user is marked for deletion.
   */
  readonly isUserMarkedForDeletion: () => boolean
  /**
   * Return `true` if the user is deleted completely.
   */
  readonly isUserDeleted: () => boolean
  /**
   * Return `true` if the user is soft deleted.
   */
  readonly isUserSoftDeleted: () => boolean
}

const AuthContext = React.createContext<AuthContextType | null>(null)

// ====================
// === AuthProvider ===
// ====================

/** Props for an {@link AuthProvider}. */
export interface AuthProviderProps {
  readonly shouldStartInOfflineMode: boolean
  readonly supportsLocalBackend: boolean
  readonly authService: authServiceModule.AuthService | null
  /** Callback to execute once the user has authenticated successfully. */
  readonly onAuthenticated: (accessToken: string | null) => void
  readonly children: React.ReactNode
  readonly projectManagerUrl: string | null
  readonly projectManagerRootDirectory: projectManager.Path | null
}

/** A React provider for the Cognito API. */
export default function AuthProvider(props: AuthProviderProps) {
  const { shouldStartInOfflineMode, supportsLocalBackend, authService, onAuthenticated } = props
  const { children, projectManagerUrl, projectManagerRootDirectory } = props
  const logger = loggerProvider.useLogger()
  const { cognito } = authService ?? {}
  const { session, deinitializeSession, onSessionError } = sessionProvider.useSession()
  const { setBackendWithoutSavingType } = backendProvider.useSetBackend()
  const { localStorage } = localStorageProvider.useLocalStorage()
  const { getText } = textProvider.useText()
  // This must not be `hooks.useNavigate` as `goOffline` would be inaccessible,
  // and the function call would error.
  // eslint-disable-next-line no-restricted-properties
  const navigate = router.useNavigate()
  const [forceOfflineMode, setForceOfflineMode] = React.useState(shouldStartInOfflineMode)
  const [initialized, setInitialized] = React.useState(false)
  const [userSession, setUserSession] = React.useState<UserSession | null>(null)
  const toastId = React.useId()

  const setUser = React.useCallback((valueOrUpdater: React.SetStateAction<backendModule.User>) => {
    setUserSession(oldUserSession => {
      if (oldUserSession == null || !('user' in oldUserSession) || oldUserSession.user == null) {
        return oldUserSession
      } else {
        return object.merge(oldUserSession, {
          user:
            typeof valueOrUpdater !== 'function'
              ? valueOrUpdater
              : valueOrUpdater(oldUserSession.user),
        })
      }
    })
  }, [])

  const goOfflineInternal = React.useCallback(() => {
    setInitialized(true)
    sentry.setUser(null)
    setUserSession(OFFLINE_USER_SESSION)
    if (supportsLocalBackend && projectManagerUrl != null && projectManagerRootDirectory != null) {
      setBackendWithoutSavingType(new LocalBackend(projectManagerUrl, projectManagerRootDirectory))
    } else {
      // Provide dummy headers to avoid errors. This `Backend` will never be called as
      // the entire UI will be disabled.
      const client = new HttpClient([['Authorization', '']])
      setBackendWithoutSavingType(new RemoteBackend(client, logger, getText))
    }
  }, [
    getText,
    /* should never change */ projectManagerUrl,
    /* should never change */ projectManagerRootDirectory,
    /* should never change */ supportsLocalBackend,
    /* should never change */ logger,
    /* should never change */ setBackendWithoutSavingType,
  ])

  const goOffline = React.useCallback(
    (shouldShowToast = true) => {
      if (shouldShowToast) {
        toast.toast.error('You are offline, switching to offline mode.')
      }
      goOfflineInternal()
      navigate(appUtils.DASHBOARD_PATH)
      return Promise.resolve(true)
    },
    [goOfflineInternal, /* should never change */ navigate]
  )

  // This component cannot use `useGtagEvent` because `useGtagEvent` depends on the React Context
  // defined by this component.
  const gtagEvent = React.useCallback(
    (name: string, params?: object) => {
      if (userSession?.type !== UserSessionType.offline) {
        gtag.event(name, params)
      }
    },
    [userSession?.type]
  )
  const gtagEventRef = React.useRef(gtagEvent)
  gtagEventRef.current = gtagEvent

  React.useEffect(() => {
    gtag.gtag('set', {
      platform: detect.platform(),
      architecture: detect.architecture(),
    })
    return gtagHooks.gtagOpenCloseCallback(gtagEventRef, 'open_app', 'close_app')
  }, [])

  // This is identical to `hooks.useOnlineCheck`, however it is inline here to avoid any possible
  // circular dependency.
  React.useEffect(() => {
    if (!navigator.onLine) {
      void goOffline()
    }
  }, [/* should never change */ goOffline])

  React.useEffect(() => {
    if (authService == null) {
      // The authentication client secrets and endpoint URLs are not set.
      goOfflineInternal()
      navigate(appUtils.DASHBOARD_PATH)
    }
  }, [authService, navigate, /* should never change */ goOfflineInternal])

  React.useEffect(
    () =>
      onSessionError(error => {
        if (isNetworkError(error)) {
          void goOffline()
        }
      }),
    [onSessionError, /* should never change */ goOffline]
  )

  React.useEffect(() => {
    const onFetchError = () => {
      void goOffline()
    }
    document.addEventListener(httpClient.FETCH_ERROR_EVENT_NAME, onFetchError)
    return () => {
      document.removeEventListener(httpClient.FETCH_ERROR_EVENT_NAME, onFetchError)
    }
  }, [/* should never change */ goOffline])

  /** Fetch the JWT access token from the session via the AWS Amplify library.
   *
   * When invoked, retrieves the access token (if available) from the storage method chosen when
   * Amplify was configured (e.g. local storage). If the token is not available, return `undefined`.
   * If the token has expired, automatically refreshes the token and returns the new token. */
  React.useEffect(() => {
    const fetchSession = async () => {
      if (!navigator.onLine || forceOfflineMode) {
        goOfflineInternal()
        setForceOfflineMode(false)
      } else if (session == null) {
        setInitialized(true)
        if (!initialized) {
          sentry.setUser(null)
          setUserSession(null)
        }
      } else {
        const client = new HttpClient([['Authorization', `Bearer ${session.accessToken}`]])
        const backend = new RemoteBackend(client, logger, getText)
        // The backend MUST be the remote backend before login is finished.
        // This is because the "set username" flow requires the remote backend.
        if (!initialized || userSession == null || userSession.type === UserSessionType.offline) {
          setBackendWithoutSavingType(backend)
        }
        gtagEvent('cloud_open')
        let user: backendModule.User | null
        while (true) {
          try {
            user = await backend.usersMe()
            break
          } catch (error) {
            // The value may have changed after the `await`.
            // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
            if (!navigator.onLine || isNetworkError(error)) {
              void goOffline()
              // eslint-disable-next-line no-restricted-syntax
              return
            }
            // This prevents a busy loop when request blocking is enabled in DevTools.
            // The UI will be blank indefinitely. This is intentional, since for real
            // network outages, `navigator.onLine` will be false.
            await new Promise<void>(resolve => {
              window.setTimeout(resolve, REQUEST_DELAY_MS)
            })
          }
        }
        const url = new URL(location.href)
        if (url.searchParams.get('authentication') === 'false') {
          url.searchParams.delete('authentication')
          history.replaceState(null, '', url.toString())
        }
        let newUserSession: UserSession
        if (user == null) {
          sentry.setUser({ email: session.email })
          newUserSession = {
            type: UserSessionType.partial,
            ...session,
          }
        } else {
          sentry.setUser({
            id: user.userId,
            email: user.email,
            username: user.name,
            // eslint-disable-next-line @typescript-eslint/naming-convention
            ip_address: '{{auto}}',
          })
          newUserSession = {
            type: UserSessionType.full,
            ...session,
            user,
          }

          // 34560000 is the recommended max cookie age.
          const parentDomain = location.hostname.replace(/^[^.]*\./, '')
          document.cookie = `logged_in=yes;max-age=34560000;domain=${parentDomain};samesite=strict;secure`

          // Save access token so can it be reused by the backend.
          cognito?.saveAccessToken({
            accessToken: session.accessToken,
            clientId: session.clientId,
            expireAt: session.expireAt,
            refreshToken: session.refreshToken,
            refreshUrl: session.refreshUrl,
          })

          // Execute the callback that should inform the Electron app that the user has logged in.
          // This is done to transition the app from the authentication/dashboard view to the IDE.
          onAuthenticated(session.accessToken)
        }

        setUserSession(newUserSession)
        setInitialized(true)
      }
    }

    fetchSession().catch(error => {
      if (isUserFacingError(error)) {
        toast.toast.error(error.message)
        logger.error(error.message)
      } else {
        logger.error(error)
      }
    })
    // `userSession` MUST NOT be a dependency as `setUserSession` is called every time
    // by this effect. Because it is an object literal, it will never be equal to the previous
    // value.
    // `initialized` MUST NOT be a dependency as it breaks offline mode.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [
    cognito,
    logger,
    onAuthenticated,
    session,
    /* should never change */ goOfflineInternal,
    /* should never change */ setBackendWithoutSavingType,
  ])

  /** Wrap a function returning a {@link Promise} to display a loading toast notification
   * until the returned {@link Promise} finishes loading. */
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
      toastError(getText('setUsernameLocalBackend'))
      return false
    } else {
      gtagEvent('cloud_user_created')
      try {
        const organizationId = await cognito.organizationId()
        // This should not omit success and error toasts as it is not possible
        // to render this optimistically.
        await toast.toast.promise(
          backend.createUser({
            userName: username,
            userEmail: backendModule.EmailAddress(email),
            organizationId:
              organizationId != null ? backendModule.OrganizationId(organizationId) : null,
          }),
          {
            success: getText('setUsernameSuccess'),
            error: getText('setUsernameError'),
            pending: getText('settingUsername'),
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

  const restoreUser = async (backend: Backend) => {
    if (cognito == null) {
      return false
    } else {
      if (backend.type === backendModule.BackendType.local) {
        toastError(getText('restoreUserLocalBackendError'))
        return false
      } else {
        await backend.restoreUser()
        setUser(object.merger({ removeAt: null }))

        toastSuccess(getText('restoreUserSuccess'))
        navigate(appUtils.DASHBOARD_PATH)

        return true
      }
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

  const signOut = async () => {
    if (cognito == null) {
      return false
    } else {
      const parentDomain = location.hostname.replace(/^[^.]*\./, '')
      document.cookie = `logged_in=no;max-age=0;domain=${parentDomain}`
      gtagEvent('cloud_sign_out')
      cognito.saveAccessToken(null)
      localStorage.clearUserSpecificEntries()
      deinitializeSession()
      setInitialized(false)
      sentry.setUser(null)
      setUserSession(null)
      // This should not omit success and error toasts as it is not possible
      // to render this optimistically.
      await toast.toast.promise(cognito.signOut(), {
        success: getText('signOutSuccess'),
        error: getText('signOutError'),
        pending: getText('loggingOut'),
      })
      return true
    }
  }

  const isUserMarkedForDeletion = () =>
    !!(userSession && 'user' in userSession && userSession.user?.removeAt)

  const isUserDeleted = () => {
    if (userSession && 'user' in userSession && userSession.user?.removeAt) {
      const removeAtDate = new Date(userSession.user.removeAt)
      const now = new Date()

      return removeAtDate <= now
    } else {
      return false
    }
  }

  const isUserSoftDeleted = () => {
    if (userSession && 'user' in userSession && userSession.user?.removeAt) {
      const removeAtDate = new Date(userSession.user.removeAt)
      const now = new Date()

      return removeAtDate > now
    } else {
      return false
    }
  }

  const value = {
    goOffline: goOffline,
    signUp: withLoadingToast(signUp),
    confirmSignUp: withLoadingToast(confirmSignUp),
    setUsername,
    isUserMarkedForDeletion,
    isUserDeleted,
    isUserSoftDeleted,
    restoreUser,
    signInWithGoogle: () => {
      if (cognito == null) {
        return Promise.resolve(false)
      } else {
        gtagEvent('cloud_sign_in', { provider: 'Google' })
        return cognito.signInWithGoogle().then(
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
        return cognito.signInWithGitHub().then(
          () => true,
          () => false
        )
      }
    },
    signInWithPassword: withLoadingToast(signInWithPassword),
    forgotPassword: withLoadingToast(forgotPassword),
    resetPassword: withLoadingToast(resetPassword),
    changePassword: withLoadingToast(changePassword),
    signOut,
    session: userSession,
    setUser,
  }

  return (
    <AuthContext.Provider value={value}>
      {/* Only render the underlying app after we assert for the presence of a current user. */}
      {initialized ? children : <LoadingScreen />}
    </AuthContext.Provider>
  )
}

/** Type of an error containing a `string`-typed `message` field.
 *
 * Many types of errors fall into this category. We use this type to check if an error can be safely
 * displayed to the user. */
interface UserFacingError {
  /** The user-facing error message. */
  readonly message: string
}

/** Return `true` if the value is a {@link UserFacingError}. */
function isUserFacingError(value: unknown): value is UserFacingError {
  return typeof value === 'object' && value != null && 'message' in value
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

// ===============================
// === shouldPreventNavigation ===
// ===============================

/** True if navigation should be prevented, for debugging purposes. */
function getShouldPreventNavigation() {
  const location = router.useLocation()
  return new URLSearchParams(location.search).get('prevent-navigation') === 'true'
}

// =======================
// === ProtectedLayout ===
// =======================

/** A React Router layout route containing routes only accessible by users that are logged in. */
export function ProtectedLayout() {
  const { session } = useAuth()
  const shouldPreventNavigation = getShouldPreventNavigation()

  if (!shouldPreventNavigation && session == null) {
    return <router.Navigate to={appUtils.LOGIN_PATH} />
  } else if (!shouldPreventNavigation && session?.type === UserSessionType.partial) {
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
  const shouldPreventNavigation = getShouldPreventNavigation()

  if (!shouldPreventNavigation && session?.type === UserSessionType.full) {
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
  const shouldPreventNavigation = getShouldPreventNavigation()

  if (!shouldPreventNavigation && session?.type === UserSessionType.partial) {
    return <router.Navigate to={appUtils.SET_USERNAME_PATH} />
  } else if (!shouldPreventNavigation && session?.type === UserSessionType.full) {
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

/**
 * A React Router layout route containing routes only accessible by users that are not deleted.
 */
export function NotDeletedUserLayout() {
  const { session, isUserMarkedForDeletion } = useAuth()
  const shouldPreventNavigation = getShouldPreventNavigation()

  if (shouldPreventNavigation) {
    return <router.Outlet context={session} />
  } else {
    if (isUserMarkedForDeletion()) {
      return <router.Navigate to={appUtils.RESTORE_USER_PATH} />
    } else {
      return <router.Outlet context={session} />
    }
  }
}

/**
 * A React Router layout route containing routes only accessible by users that are deleted softly
 */
export function SoftDeletedUserLayout() {
  const { session, isUserMarkedForDeletion, isUserDeleted, isUserSoftDeleted } = useAuth()
  const shouldPreventNavigation = getShouldPreventNavigation()

  if (shouldPreventNavigation) {
    return <router.Outlet context={session} />
  } else if (isUserMarkedForDeletion()) {
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
