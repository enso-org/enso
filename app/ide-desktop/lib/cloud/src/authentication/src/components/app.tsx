// FIXME [NP]: Address comments in https://github.com/enso-org/enso/pull/5716/files
/** @file Main App module responsible for rendering virtual router. */

import * as React from 'react'
import { Routes, Route, BrowserRouter, MemoryRouter, useNavigate } from 'react-router-dom'

import { AuthProvider, GuestLayout, ProtectedLayout } from '../authentication';
import DashboardContainer from "./dashboard";
import ForgotPasswordContainer from "./forgotPassword";
import ResetPasswordContainer from "./resetPassword";
import LoginContainer from "./login";
import RegistrationContainer from "./registration";
import ConfirmRegistrationContainer from "./confirmRegistration";
import SetUsernameContainer from "./setUsername";
import { Toaster } from 'react-hot-toast';
import { FC, Fragment, useMemo } from 'react';
import authApi from '../authentication/api';
import withRouter from '../navigation';
import {ProjectManager} from "enso-studio-content/src/project_manager";
import { Logger, LoggerProvider } from '../logger';
import { SessionProvider } from '../authentication/providers/session';



// =================
// === Constants ===
// =================

/// Path to the root of the app (i.e., the Cloud dashboard).
export const DASHBOARD_PATH = "/";
/// Path to the login page.
export const LOGIN_PATH = "/login";
/// Path to the registration page.
export const REGISTRATION_PATH = "/registration";
/// Path to the confirm registration page.
// FIXME [NP]: use a more specific path
export const CONFIRM_REGISTRATION_PATH = "/confirmation";
/// Path to the forgot password page.
export const FORGOT_PASSWORD_PATH = "/forgot-password";
/// Path to the reset password page.
export const RESET_PASSWORD_PATH = "/reset-password";
/// Path to the set username page.
export const SET_USERNAME_PATH = "/set-username";



// ===========
// === App ===
// ===========

/// Global configuration for the `App` component.
export interface AppProps {
  /**
   * Logger to use for logging.
   */
  logger: Logger;
  /**
   * Whether the application is running on a desktop (i.e., versus in the Cloud).
   */
  runningOnDesktop: boolean;
  onAuthenticated: () => void;
  projectManager: ProjectManager | undefined;
}

/**
 * Functional component called by the parent module, returning the root React component for this package.
 * 
 * This component handles all the initialization and rendering of the app, and manages the app's
 * routes. It also initializes an `AuthProvider` that will be used by the rest of the app.
 */
// eslint-disable-next-line @typescript-eslint/naming-convention
const App = (props: AppProps) => {
  const { runningOnDesktop } = props;
  // eslint-disable-next-line @typescript-eslint/naming-convention, @typescript-eslint/no-unnecessary-condition
  const Router = runningOnDesktop ? MemoryRouter : BrowserRouter;

  // Note that the `Router` must be the parent of the `AuthProvider`, because the `AuthProvider`
  // will redirect the user between the login/register pages and the dashboard.
  return (
    <>
      <Toaster position="top-center" reverseOrder={false} />
      <Router>
        <AppRouterWithHistory {...props} />
      </Router>
    </>
  );
}



// =================
// === AppRouter ===
// =================

/// Router definition for the app.
// FIXME [NP]: React components are expected to use PascalCase, but our linter is not configured to
//   allow that. Do we want to allow that, even if it would disable the lint for non-React code?
// eslint-disable-next-line @typescript-eslint/naming-convention
const AppRouter: FC<AppProps> = (props) => {
  const { logger, onAuthenticated, runningOnDesktop, projectManager } = props;
  const navigate = useNavigate();
  const authConfig = { navigate, ...props }
  const auth = useMemo(() => authApi(authConfig), []);

  return (
    <LoggerProvider logger={logger}>
      <SessionProvider userSession={auth.userSession}>
        <AuthProvider auth={auth} onAuthenticated={onAuthenticated} >
          <Routes>
            <Fragment>
              {/* Login & registration pages are visible to unauthenticated users. */}
              <Route element={<GuestLayout />}>
                <Route path={REGISTRATION_PATH} element={<RegistrationContainer />} /> 
                <Route path={LOGIN_PATH} element={<LoginContainer />} /> 
              </Route>
              {/* Protected pages are visible to authenticated users. */}
              <Route element={<ProtectedLayout />}>
                {/* FIXME [NP]: why do we need this extra one for electron to work? */}
                <Route index element={<DashboardContainer runningOnDesktop={runningOnDesktop} projectManager={projectManager} />} />
                <Route path={DASHBOARD_PATH} element={<DashboardContainer runningOnDesktop={runningOnDesktop} projectManager={projectManager} />} />
                <Route path={SET_USERNAME_PATH} element={<SetUsernameContainer />} /> 
              </Route>
              {/* Other pages are visible to unauthenticated and authenticated users. */}
              <Route path={CONFIRM_REGISTRATION_PATH} element={<ConfirmRegistrationContainer />} />
              <Route path={FORGOT_PASSWORD_PATH} element={<ForgotPasswordContainer />} />
              <Route path={RESET_PASSWORD_PATH} element={<ResetPasswordContainer />} />
            </Fragment>
          </Routes>
        </AuthProvider>
      </SessionProvider>
    </LoggerProvider>
  )
}

// eslint-disable-next-line @typescript-eslint/naming-convention
const AppRouterWithHistory = withRouter(AppRouter);

export default App;

