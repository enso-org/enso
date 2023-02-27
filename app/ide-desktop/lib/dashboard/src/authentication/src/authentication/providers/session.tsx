import { ReactNode, createContext, useContext, useState, useEffect } from "react";
import { None, Option } from "ts-results";

import { useAsyncEffect } from "../../hooks";
import { UserSession } from "../cognito";
import { ListenerCallback } from "../listen";



// =================
// === Constants ===
// =================

/**
 * URL that the Electron main page is hosted on.
 * 
 * This **must** be the actual page that the Electron app is hosted on, otherwise the OAuth flow
 * will not work and will redirect the user to a blank page. If this is the correct URL, no redirect
 * will occur (which is the desired behaviour).
 */
const MAIN_PAGE_URL = "http://localhost:8080";

/** Initial value of the session refresh counter. This value itself is meaningless. The only point
 * of the session refresh counter is to trigger a re-fetch of the user's credentials and a re-render
 * of the `AuthProvider` component when the counter is incremented. */
const INITIAL_REFRESH_COUNT = 0;



// ======================
// === SessionContext ===
// ======================

interface SessionContextType {
    session: Option<UserSession>;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
const SessionContext = createContext<SessionContextType>({} as SessionContextType)



// =======================
// === SessionProvider ===
// =======================

interface SessionProviderProps {
    registerAuthEventListener: (callback: ListenerCallback) => void;
    userSession: () => Promise<Option<UserSession>>;
    children: ReactNode;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
export const SessionProvider = (props: SessionProviderProps) => {
    const { children, userSession, registerAuthEventListener } = props;

    // Flag used to avoid rendering child components until we've fetched the user's session at least
    // once. Avoids flash of the login screen when the user is already logged in.
    const [initialized, setInitialized] = useState(false);

    // State that, when incremented, forces a refresh of the user session. This is useful when a
    // user has just logged in (so their cached credentials are out of date). Should be used via the
    // `refreshSession` function.
    const [refresh, setRefresh] = useState(INITIAL_REFRESH_COUNT);

    // Function that forces a refresh of the user session.
    //
    // Should be called after any operation that **will** (not **might**) change the user's session.
    // For example, this should be called after signing out. Calling this will result in a re-render
    // of the whole page, which is why it should only be done when necessary.
    const refreshSession = () => setRefresh((refresh) => refresh + 1);

    // Register an async effect that will fetch the user's session whenever the `refresh` state is
    // incremented. This is useful when a user has just logged in (as their cached credentials are
    // out of date, so this will update them).
    const [session] = useAsyncEffect(None, async () => {
      const session = await userSession();
      setInitialized(true);
      return session;
    }, [refresh, userSession])

    // Register an effect that will listen for authentication events. When the event occurs, we will
    // refresh or clear the user's session, forcing a re-render of the page with the new session.
    //
    // For example, if a user clicks the signout button, this will clear the user's session, which
    // means we want the login screen to render (which is a child of this provider).
    useEffect(() => {
      const listener: ListenerCallback = (event) => {
        if (event === "signIn") {
            refreshSession();
        } else if (event === "customOAuthState" || event === "cognitoHostedUI") {
            // AWS Amplify doesn't provide a way to set the redirect URL for the OAuth flow, so we
            // have to hack it by replacing the URL in the browser's history. This is done because
            // otherwise the user will be redirected to a URL like `enso://auth`, which will not
            // work.
            //
            // See: https://github.com/aws-amplify/amplify-js/issues/3391#issuecomment-756473970
            window.history.replaceState({}, "", MAIN_PAGE_URL)
            refreshSession();
        // Typescript tells us we don't need the final condition because this is an exhaustive
        // match, but we don't want to turn this into an `else` in case we add more event types we
        // care about in the future. If we did so, then TypeScript wouldn't notify us about missing
        // cases.
        // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
        } else if (event === "signOut") {
            refreshSession();
        }
      };
      
      const cancel = registerAuthEventListener(listener);
      // Return the `cancel` function from the `useEffect`, which ensures that the listener is cleaned
      // up between renders. This must be done because the `useEffect` will be called multiple times
      // during the lifetime of the component.
      return cancel
    }, [registerAuthEventListener]);

    const value = { session };

    return (
        <SessionContext.Provider value={value}>
            {initialized && children}
        </SessionContext.Provider>
    );
}



// ==================
// === useSession ===
// ==================

export const useSession = () => useContext(SessionContext);
