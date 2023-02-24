// FIXME [NP]: docs
import { ReactNode, createContext, useContext, useState } from "react";
import { useAsyncEffect } from "../../hooks";
import { useLogger } from "../../logger";
import { UserSession } from "../api";



// ======================
// === SessionContext ===
// ======================

interface SessionContextType {
    session: UserSession | null;
    refreshSession: () => void;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
const SessionContext = createContext<SessionContextType>({} as SessionContextType)



// =======================
// === SessionProvider ===
// =======================

interface SessionProviderProps {
    userSession: () => Promise<UserSession | null>;
    children: ReactNode;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
export const SessionProvider = (props: SessionProviderProps) => {
    const logger = useLogger();
    const { children, userSession } = props;

    // Flag used to avoid rendering child components until we've fetched the user's session at least
    // once. Avoids flash of the login screen when the user is already logged in.
    const [initialized, setInitialized] = useState(false);

    // State that, when incremented, forces a refresh of the user session. This is useful when a
    // user has just logged in (so their cached credentials are out of date). Should be used via the
    // `refreshSession` function.
    const [refresh, setRefresh] = useState(0);

    // Function that forces a refresh of the user session.
    //
    // Should be called after any operation that **will** (not **might**) change the user's session.
    // For example, this should be called after signing out. Calling this will result in a re-render
    // of the whole page, which is why it should only be done when necessary.
    // FIXME [NP]: remove
    const refreshSession = () => setRefresh((refresh) => refresh + 1);

    // Register an async effect that will fetch the user's session whenever the `refresh` state is
    // incremented. This is useful when a user has just logged in (as their cached credentials are
    // out of date, so this will update them).
    const [session] = useAsyncEffect(null, async () => {
      logger.log("FIXME [NP]: async userSession event")
      const session = await userSession();
      setInitialized(true);
      return session;
    }, [refresh, userSession])

    const value = { session, refreshSession };

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

