/** @file Main App module responsible for rendering virtual router. */

import * as react from 'react'
import * as toast from 'react-hot-toast';
import * as router from 'react-router-dom'
import * as projectManager from "enso-studio-content/src/project_manager";

import * as authProvider from '../authentication/providers/auth';
import DashboardContainer from "../dashboard/components/dashboard";
import ForgotPasswordContainer from "../authentication/components/forgotPassword";
import ResetPasswordContainer from "../authentication/components/resetPassword";
import LoginContainer from "../authentication/components/login";
import RegistrationContainer from "../authentication/components/registration";
import ConfirmRegistrationContainer from "../authentication/components/confirmRegistration";
import SetUsernameContainer from "../authentication/components/setUsername";
import * as authService from '../authentication/service';
import withRouter from '../navigation';
import * as loggerProvider from '../providers/logger';
import * as session from '../authentication/providers/session';



// =================
// === Constants ===
// =================

/** Path to the root of the app (i.e., the Cloud dashboard). */
export const DASHBOARD_PATH = "/";
/** Path to the login page. */
export const LOGIN_PATH = "/login";
/** Path to the registration page. */
export const REGISTRATION_PATH = "/registration";
/** Path to the confirm registration page. */
export const CONFIRM_REGISTRATION_PATH = "/confirmation";
/** Path to the forgot password page. */
export const FORGOT_PASSWORD_PATH = "/forgot-password";
/** Path to the reset password page. */
export const RESET_PASSWORD_PATH = "/password-reset";
/** Path to the set username page. */
export const SET_USERNAME_PATH = "/set-username";



// ===========
// === App ===
// ===========

/** Global configuration for the `App` component. */
export interface AppProps {
  /** Logger to use for logging. */
  logger: loggerProvider.Logger;
  /** Whether the application is running on a desktop (i.e., versus in the Cloud). */
  runningOnDesktop: boolean;
  onAuthenticated: () => void;
  projectManager?: projectManager.ProjectManager;
}

/** Functional component called by the parent module, returning the root React component for this package.
 * 
 * This component handles all the initialization and rendering of the app, and manages the app's
 * routes. It also initializes an `AuthProvider` that will be used by the rest of the app. */
// eslint-disable-next-line @typescript-eslint/naming-convention
const App = (props: AppProps) => {
  const { runningOnDesktop } = props;
  // eslint-disable-next-line @typescript-eslint/naming-convention
  const Router = runningOnDesktop ? router.MemoryRouter : router.BrowserRouter;

  // Note that the `Router` must be the parent of the `AuthProvider`, because the `AuthProvider`
  // will redirect the user between the login/register pages and the dashboard.
  return (
    <>
      <toast.Toaster position="top-center" reverseOrder={false} />
      <Router>
        <AppRouterWithHistory {...props} />
      </Router>
    </>
  );
}



// =================
// === AppRouter ===
// =================

/** Router definition for the app. */
// eslint-disable-next-line @typescript-eslint/naming-convention
const AppRouter = (props: AppProps) => {
  const { logger, onAuthenticated, runningOnDesktop, projectManager } = props;
  const navigate = router.useNavigate();
  const authConfig = { navigate, ...props }
  const memoizedAuthService = react.useMemo(() => authService.initAuthService(authConfig), []);

  const userSession = memoizedAuthService.cognito.userSession;
  const registerAuthEventListener = memoizedAuthService.registerAuthEventListener;

  return (
    <loggerProvider.LoggerProvider logger={logger}>
      <session.SessionProvider userSession={userSession} registerAuthEventListener={registerAuthEventListener}>
        <authProvider.AuthProvider authService={memoizedAuthService} onAuthenticated={onAuthenticated} >
          <router.Routes>
            <react.Fragment>
              {/* Login & registration pages are visible to unauthenticated users. */}
              <router.Route element={<authProvider.GuestLayout />}>
                <router.Route path={REGISTRATION_PATH} element={<RegistrationContainer />} /> 
                <router.Route path={LOGIN_PATH} element={<LoginContainer />} /> 
              </router.Route>
              {/* Protected pages are visible to authenticated users. */}
              <router.Route element={<authProvider.ProtectedLayout />}>
                <router.Route index element={<DashboardContainer runningOnDesktop={runningOnDesktop} projectManager={projectManager} />} />
                <router.Route path={DASHBOARD_PATH} element={<DashboardContainer runningOnDesktop={runningOnDesktop} projectManager={projectManager} />} />
                <router.Route path={SET_USERNAME_PATH} element={<SetUsernameContainer />} /> 
              </router.Route>
              {/* Other pages are visible to unauthenticated and authenticated users. */}
              <router.Route path={CONFIRM_REGISTRATION_PATH} element={<ConfirmRegistrationContainer />} />
              <router.Route path={FORGOT_PASSWORD_PATH} element={<ForgotPasswordContainer />} />
              <router.Route path={RESET_PASSWORD_PATH} element={<ResetPasswordContainer />} />
            </react.Fragment>
          </router.Routes>
        </authProvider.AuthProvider>
      </session.SessionProvider>
    </loggerProvider.LoggerProvider>
  )
}

// eslint-disable-next-line @typescript-eslint/naming-convention
const AppRouterWithHistory = withRouter(AppRouter);

export default App;

