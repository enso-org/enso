/** @file File containing the {@link App} React component, which is the entrypoint into our React
 * application.
 *
 * # Providers
 *
 * The {@link App} component is responsible for defining the global context used by child
 * components. For example, it defines a {@link toast.Toaster}, which is used to display temporary
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

import * as react from 'react'
import * as router from 'react-router-dom'
import * as toast from 'react-hot-toast'

import * as authService from '../authentication/service'
import * as detect from '../detect'

import * as authProvider from '../authentication/providers/auth'
import * as backendProvider from '../providers/backend'
import * as loggerProvider from '../providers/logger'
import * as modalProvider from '../providers/modal'
import * as sessionProvider from '../authentication/providers/session'

import ConfirmRegistration from '../authentication/components/confirmRegistration'
import Dashboard from '../dashboard/components/dashboard'
import ForgotPassword from '../authentication/components/forgotPassword'
import Login from '../authentication/components/login'
import Registration from '../authentication/components/registration'
import ResetPassword from '../authentication/components/resetPassword'
import SetUsername from '../authentication/components/setUsername'

// =================
// === Constants ===
// =================

/** Path to the root of the app (i.e., the Cloud dashboard). */
export const DASHBOARD_PATH = '/'
/** Path to the login page. */
export const LOGIN_PATH = '/login'
/** Path to the registration page. */
export const REGISTRATION_PATH = '/registration'
/** Path to the confirm registration page. */
export const CONFIRM_REGISTRATION_PATH = '/confirmation'
/** Path to the forgot password page. */
export const FORGOT_PASSWORD_PATH = '/forgot-password'
/** Path to the reset password page. */
export const RESET_PASSWORD_PATH = '/password-reset'
/** Path to the set username page. */
export const SET_USERNAME_PATH = '/set-username'

// ===========
// === App ===
// ===========

/** Global configuration for the `App` component. */
export interface AppProps {
    logger: loggerProvider.Logger
    /** Whether the application may have the local backend running. */
    supportsLocalBackend: boolean
    /** Whether the application supports deep links. This is only true when using
     * the installed app on macOS and Windows. */
    supportsDeepLinks: boolean
    /** Whether the dashboard should be rendered. */
    showDashboard: boolean
    onAuthenticated: () => void
    appRunner: AppRunner
}

/** Component called by the parent module, returning the root React component for this
 * package.
 *
 * This component handles all the initialization and rendering of the app, and manages the app's
 * routes. It also initializes an `AuthProvider` that will be used by the rest of the app. */
function App(props: AppProps) {
    // This is a React component even though it does not contain JSX.
    // eslint-disable-next-line no-restricted-syntax
    const Router = detect.isRunningInElectron() ? router.MemoryRouter : router.BrowserRouter
    /** Note that the `Router` must be the parent of the `AuthProvider`, because the `AuthProvider`
     * will redirect the user between the login/register pages and the dashboard. */
    return (
        <>
            <toast.Toaster position="top-center" reverseOrder={false} />
            <Router>
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
    const { logger, showDashboard, onAuthenticated } = props
    const navigate = router.useNavigate()
    // FIXME[sb]: After platform detection for Electron is merged in, `IS_DEV_MODE` should be
    // set to true on `ide watch`.
    if (IS_DEV_MODE) {
        // @ts-expect-error This is used exclusively for debugging.
        window.navigate = navigate
    }
    const mainPageUrl = new URL(window.location.href)
    const memoizedAuthService = react.useMemo(() => {
        const authConfig = { navigate, ...props }
        return authService.initAuthService(authConfig)
    }, [navigate, props])
    const userSession = memoizedAuthService.cognito.userSession.bind(memoizedAuthService.cognito)
    const registerAuthEventListener = memoizedAuthService.registerAuthEventListener
    const routes = (
        <router.Routes>
            <react.Fragment>
                {/* Login & registration pages are visible to unauthenticated users. */}
                <router.Route element={<authProvider.GuestLayout />}>
                    <router.Route path={REGISTRATION_PATH} element={<Registration />} />
                    <router.Route path={LOGIN_PATH} element={<Login />} />
                </router.Route>
                {/* Protected pages are visible to authenticated users. */}
                <router.Route element={<authProvider.ProtectedLayout />}>
                    <router.Route
                        path={DASHBOARD_PATH}
                        element={showDashboard && <Dashboard {...props} />}
                    />
                </router.Route>
                {/* Semi-protected pages are visible to users currently registering. */}
                <router.Route element={<authProvider.SemiProtectedLayout />}>
                    <router.Route path={SET_USERNAME_PATH} element={<SetUsername />} />
                </router.Route>
                {/* Other pages are visible to unauthenticated and authenticated users. */}
                <router.Route path={CONFIRM_REGISTRATION_PATH} element={<ConfirmRegistration />} />
                <router.Route path={FORGOT_PASSWORD_PATH} element={<ForgotPassword />} />
                <router.Route path={RESET_PASSWORD_PATH} element={<ResetPassword />} />
            </react.Fragment>
        </router.Routes>
    )
    return (
        <loggerProvider.LoggerProvider logger={logger}>
            <sessionProvider.SessionProvider
                mainPageUrl={mainPageUrl}
                userSession={userSession}
                registerAuthEventListener={registerAuthEventListener}
            >
                {/* This is safe, because the backend is always set by the authentication flow. */}
                {/* eslint-disable-next-line @typescript-eslint/no-non-null-assertion */}
                <backendProvider.BackendProvider initialBackend={null!}>
                    <authProvider.AuthProvider
                        authService={memoizedAuthService}
                        onAuthenticated={onAuthenticated}
                    >
                        <modalProvider.ModalProvider>{routes}</modalProvider.ModalProvider>
                    </authProvider.AuthProvider>
                </backendProvider.BackendProvider>
            </sessionProvider.SessionProvider>
        </loggerProvider.LoggerProvider>
    )
}

export default App
