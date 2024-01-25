/** @file File containing the {@link App} React component, which is the entrypoint into our React
 * application.
 *
 * # Providers
 *
 * The {@link App} component is responsible for defining the global context used by child
 * components. For example, it defines a {@link toastify.ToastContainer}, which is used to display temporary
 * notifications to the user. These global components are defined at the top of the {@link App} so
 * that they are available to all of the child components.
 *
 * The {@link App} also defines various providers (e.g., {@link authProvider.AuthProvider}).
 * Providers are a React-specific concept that allows components to access global state without
 * having to pass it down through the component tree. For example, the
 * {@link authProvider.AuthProvider} wraps the entire application, and provides the context
 * necessary for child components to use the {@link authProvider.useAuth} hook. The
 * {@link authProvider.useAuth} hook lets child components access the user's authentication session
 * (i.e., email, username, etc.) and it also provides methods for signing the user in, etc.
 *
 * Providers consist of a provider component that wraps the application, a context object defined
 * by the provider component, and a hook that can be used by child components to access the context.
 * All of the providers are initialized here, at the {@link App} component to ensure that they are
 * available to all of the child components.
 *
 * # Routes and Authentication
 *
 * The {@link AppRouter} component defines the layout of the application, in terms of navigation. It
 * consists of a list of {@link router.Route}s, as well as the HTTP pathnames that the
 * {@link router.Route}s can be accessed by.
 *
 * The {@link router.Route}s are grouped by authorization level. Some routes are
 * accessed by unauthenticated (i.e., not signed in) users. Some routes are accessed by partially
 * authenticated users (c.f. {@link authProvider.PartialUserSession}). That is, users who have
 * signed up but who have not completed email verification or set a username. The remaining
 * {@link router.Route}s require fully authenticated users (c.f.
 * {@link authProvider.FullUserSession}). */
import * as React from 'react'

import * as router from 'react-router-dom'
import * as toastify from 'react-toastify'

import * as detect from 'enso-common/src/detect'

import * as appUtils from '#/appUtils'
import * as authServiceModule from '#/authentication/service'
import * as navigateHooks from '#/hooks/navigateHooks'
import ConfirmRegistration from '#/pages/authentication/ConfirmRegistration'
import EnterOfflineMode from '#/pages/authentication/EnterOfflineMode'
import ForgotPassword from '#/pages/authentication/ForgotPassword'
import Login from '#/pages/authentication/Login'
import Registration from '#/pages/authentication/Registration'
import ResetPassword from '#/pages/authentication/ResetPassword'
import SetUsername from '#/pages/authentication/SetUsername'
import Dashboard from '#/pages/dashboard/Dashboard'
import AuthProvider, * as authProvider from '#/providers/AuthProvider'
import BackendProvider from '#/providers/BackendProvider'
import LocalStorageProvider from '#/providers/LocalStorageProvider'
import LoggerProvider from '#/providers/LoggerProvider'
import type * as loggerProvider from '#/providers/LoggerProvider'
import ModalProvider from '#/providers/ModalProvider'
import SessionProvider from '#/providers/SessionProvider'
import ShortcutsProvider from '#/providers/ShortcutsProvider'
import type * as backend from '#/services/backend'
import * as localBackend from '#/services/localBackend'
import * as shortcutsModule from '#/utilities/shortcuts'

// ======================
// === getMainPageUrl ===
// ======================

/** Returns the URL to the main page. This is the current URL, with the current route removed. */
function getMainPageUrl() {
  const mainPageUrl = new URL(window.location.href)
  mainPageUrl.pathname = mainPageUrl.pathname.replace(appUtils.ALL_PATHS_REGEX, '')
  return mainPageUrl
}

// ===========
// === App ===
// ===========

/** Global configuration for the `App` component. */
export interface AppProps {
  logger: loggerProvider.Logger
  /** Whether the application may have the local backend running. */
  supportsLocalBackend: boolean
  /** If true, the app can only be used in offline mode. */
  isAuthenticationDisabled: boolean
  /** Whether the application supports deep links. This is only true when using
   * the installed app on macOS and Windows. */
  supportsDeepLinks: boolean
  /** Whether the dashboard should be rendered. */
  shouldShowDashboard: boolean
  /** The name of the project to open on startup, if any. */
  initialProjectName: string | null
  onAuthenticated: (accessToken: string | null) => void
  projectManagerUrl: string | null
  appRunner: AppRunner
}

/** Component called by the parent module, returning the root React component for this
 * package.
 *
 * This component handles all the initialization and rendering of the app, and manages the app's
 * routes. It also initializes an `AuthProvider` that will be used by the rest of the app. */
export default function App(props: AppProps) {
  // This is a React component even though it does not contain JSX.
  // eslint-disable-next-line no-restricted-syntax
  const Router = detect.isOnElectron() ? router.MemoryRouter : router.BrowserRouter
  /** Note that the `Router` must be the parent of the `AuthProvider`, because the `AuthProvider`
   * will redirect the user between the login/register pages and the dashboard. */
  return (
    <>
      <toastify.ToastContainer
        position="top-center"
        theme="light"
        closeOnClick={false}
        draggable={false}
        toastClassName="text-sm leading-170 bg-frame-selected rounded-2xl backdrop-blur-3xl"
        transition={toastify.Zoom}
        limit={3}
      />
      <Router basename={getMainPageUrl().pathname}>
        <AppRouter {...props} />
      </Router>
    </>
  )
}

// =================
// === AppRouter ===
// =================

/** Router definition for the app.
 *
 * The only reason the {@link AppRouter} component is separate from the {@link App} component is
 * because the {@link AppRouter} relies on React hooks, which can't be used in the same React
 * component as the component that defines the provider. */
function AppRouter(props: AppProps) {
  const { logger, supportsLocalBackend, isAuthenticationDisabled, shouldShowDashboard } = props
  const { onAuthenticated, projectManagerUrl } = props
  const navigate = navigateHooks.useNavigate()
  if (detect.IS_DEV_MODE) {
    // @ts-expect-error This is used exclusively for debugging.
    window.navigate = navigate
  }
  const [shortcuts] = React.useState(() => shortcutsModule.ShortcutRegistry.createWithDefaults())
  React.useEffect(() => {
    const onKeyDown = (event: KeyboardEvent) => {
      const isTargetEditable =
        event.target instanceof HTMLInputElement ||
        (event.target instanceof HTMLElement && event.target.isContentEditable)
      const shouldHandleEvent = isTargetEditable ? !shortcutsModule.isTextInputEvent(event) : true
      if (shouldHandleEvent && shortcuts.handleKeyboardEvent(event)) {
        event.preventDefault()
        // This is required to prevent the event from propagating to the event handler
        // that focuses the search input.
        event.stopImmediatePropagation()
      }
    }
    document.body.addEventListener('keydown', onKeyDown)
    return () => {
      document.body.removeEventListener('keydown', onKeyDown)
    }
  }, [shortcuts])
  const mainPageUrl = getMainPageUrl()
  const authService = React.useMemo(() => {
    const authConfig = { navigate, ...props }
    return authServiceModule.initAuthService(authConfig)
  }, [props, /* should never change */ navigate])
  const userSession = authService.cognito.userSession.bind(authService.cognito)
  const registerAuthEventListener = authService.registerAuthEventListener
  const initialBackend: backend.Backend = isAuthenticationDisabled
    ? new localBackend.LocalBackend(projectManagerUrl)
    : // This is safe, because the backend is always set by the authentication flow.
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      null!
  const routes = (
    <router.Routes>
      <React.Fragment>
        {/* Login & registration pages are visible to unauthenticated users. */}
        <router.Route element={<authProvider.GuestLayout />}>
          <router.Route path={appUtils.REGISTRATION_PATH} element={<Registration />} />
          <router.Route
            path={appUtils.LOGIN_PATH}
            element={<Login supportsLocalBackend={supportsLocalBackend} />}
          />
        </router.Route>
        {/* Protected pages are visible to authenticated users. */}
        <router.Route element={<authProvider.ProtectedLayout />}>
          <router.Route
            path={appUtils.DASHBOARD_PATH}
            element={shouldShowDashboard && <Dashboard {...props} />}
          />
        </router.Route>
        {/* Semi-protected pages are visible to users currently registering. */}
        <router.Route element={<authProvider.SemiProtectedLayout />}>
          <router.Route path={appUtils.SET_USERNAME_PATH} element={<SetUsername />} />
        </router.Route>
        {/* Other pages are visible to unauthenticated and authenticated users. */}
        <router.Route path={appUtils.CONFIRM_REGISTRATION_PATH} element={<ConfirmRegistration />} />
        <router.Route path={appUtils.FORGOT_PASSWORD_PATH} element={<ForgotPassword />} />
        <router.Route path={appUtils.RESET_PASSWORD_PATH} element={<ResetPassword />} />
        <router.Route path={appUtils.ENTER_OFFLINE_MODE_PATH} element={<EnterOfflineMode />} />
      </React.Fragment>
    </router.Routes>
  )
  let result = routes
  result = <ShortcutsProvider shortcuts={shortcuts}>{result}</ShortcutsProvider>
  result = <ModalProvider>{result}</ModalProvider>
  result = (
    <AuthProvider
      shouldStartInOfflineMode={isAuthenticationDisabled}
      supportsLocalBackend={supportsLocalBackend}
      authService={authService}
      onAuthenticated={onAuthenticated}
      projectManagerUrl={projectManagerUrl}
    >
      {result}
    </AuthProvider>
  )
  result = <BackendProvider initialBackend={initialBackend}>{result}</BackendProvider>
  /** {@link BackendProvider} depends on {@link LocalStorageProvider}. */
  result = <LocalStorageProvider>{result}</LocalStorageProvider>
  result = (
    <SessionProvider
      mainPageUrl={mainPageUrl}
      userSession={userSession}
      registerAuthEventListener={registerAuthEventListener}
    >
      {result}
    </SessionProvider>
  )
  result = <LoggerProvider logger={logger}>{result}</LoggerProvider>
  return result
}
