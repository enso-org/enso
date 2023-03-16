/** @file Provider for the {@link SessionContextType}, which contains information about the
 * currently authenticated user's session. */
import * as react from "react";

import * as results from "ts-results";

import * as cognito from "../cognito";
import * as hooks from "../../hooks";

// ======================
// === SessionContext ===
// ======================

interface SessionContextType {
  session: results.Option<cognito.UserSession>;
}

/** See {@link AuthContext} for safety details. */
const SessionContext = react.createContext<SessionContextType>(
  // eslint-disable-next-line no-restricted-syntax
  {} as SessionContextType
);

// =======================
// === SessionProvider ===
// =======================

interface SessionProviderProps {
  userSession: () => Promise<results.Option<cognito.UserSession>>;
  children: react.ReactNode;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
export function SessionProvider(props: SessionProviderProps) {
  const { children, userSession } = props;

  /** Flag used to avoid rendering child components until we've fetched the user's session at least
   * once. Avoids flash of the login screen when the user is already logged in. */
  const [initialized, setInitialized] = react.useState(false);

  /** Register an async effect that will fetch the user's session whenever the `refresh` state is
   * incremented. This is useful when a user has just logged in (as their cached credentials are
   * out of date, so this will update them). */
  const session = hooks.useAsyncEffect(
    results.None,
    async () => {
      const innerSession = await userSession();
      setInitialized(true);
      return innerSession;
    },
    [userSession]
  );

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

export function useSession() {
  return react.useContext(SessionContext);
}
