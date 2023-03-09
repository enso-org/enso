/** @file Main App module responsible for rendering virtual router. */

import * as React from "react";
import * as toast from "react-hot-toast";
import * as router from "react-router-dom";

import * as authProvider from "../authentication/providers/auth";
import DashboardContainer from "../dashboard/components/dashboard";
import * as authService from "../authentication/service";
import withRouter from "../navigation";
import * as loggerProvider from "../providers/logger";
import * as session from "../authentication/providers/session";



// =================
// === Constants ===
// =================

/** Path to the root of the app (i.e., the Cloud dashboard). */
export const DASHBOARD_PATH = "/";



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
};



// =================
// === AppRouter ===
// =================

/** Router definition for the app. */
// eslint-disable-next-line @typescript-eslint/naming-convention
const AppRouter = (props: AppProps) => {
    const { logger, onAuthenticated } = props;
    const navigate = router.useNavigate();
    const authConfig = { navigate, ...props };
    const memoizedAuthService = React.useMemo(
        () => authService.initAuthService(authConfig),
        []
    );

    const userSession = memoizedAuthService.cognito.userSession;

    return (
        <loggerProvider.LoggerProvider logger={logger}>
            <session.SessionProvider
                userSession={userSession}
            >
                <authProvider.AuthProvider
                    onAuthenticated={onAuthenticated}
                >
                    <router.Routes>
                        <React.Fragment>
                            {/* Protected pages are visible to authenticated users. */}
                            <router.Route element={<authProvider.ProtectedLayout />}>
                                <router.Route
                                    index
                                    element={
                                        <DashboardContainer />
                                    }
                                />
                                <router.Route
                                    path={DASHBOARD_PATH}
                                    element={
                                        <DashboardContainer />
                                    }
                                />
                            </router.Route>
                        </React.Fragment>
                    </router.Routes>
                </authProvider.AuthProvider>
            </session.SessionProvider>
        </loggerProvider.LoggerProvider>
    );
};

// eslint-disable-next-line @typescript-eslint/naming-convention
const AppRouterWithHistory = withRouter(AppRouter);

export default App;
