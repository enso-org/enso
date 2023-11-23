/** @file Provider for the {@link SessionContextType}, which contains information about the
 * currently authenticated user's session. */
import * as React from 'react'

import type * as cognito from '../cognito'
import * as error from '../../error'
import * as hooks from '../../hooks'
import * as listen from '../listen'
import * as useRefresh from '../../useRefresh'

// ======================
// === SessionContext ===
// ======================

/** State contained in a {@link SessionContext}. */
interface SessionContextType {
    session: cognito.UserSession | null
    /** Set `initialized` to false. Must be called when logging out. */
    deinitializeSession: () => void
}

/** See {@link AuthContext} for safety details. */
const SessionContext = React.createContext<SessionContextType>(
    // eslint-disable-next-line no-restricted-syntax
    {} as SessionContextType
)

// =======================
// === SessionProvider ===
// =======================

/** Props for a {@link SessionProvider}. */
export interface SessionProviderProps {
    /** URL that the content of the app is served at, by Electron.
     *
     * This **must** be the actual page that the content is served at, otherwise the OAuth flow will
     * not work and will redirect the user to a blank page. If this is the correct URL, no redirect
     * will occur (which is the desired behaviour).
     *
     * The URL includes a scheme, hostname, and port (e.g., `http://localhost:8080`). The port is not
     * known ahead of time, since the content may be served on any free port. Thus, the URL is
     * obtained by reading the window location at the time that authentication is instantiated. This
     * is guaranteed to be the correct location, since authentication is instantiated when the content
     * is initially served. */
    mainPageUrl: URL
    registerAuthEventListener: listen.ListenFunction
    userSession: () => Promise<cognito.UserSession | null>
    children: React.ReactNode
}

/** A React provider for the session of the authenticated user. */
export function SessionProvider(props: SessionProviderProps) {
    const { mainPageUrl, children, userSession, registerAuthEventListener } = props

    const [refresh, doRefresh] = useRefresh.useRefresh()

    /** Flag used to avoid rendering child components until we've fetched the user's session at least
     * once. Avoids flash of the login screen when the user is already logged in. */
    const [initialized, setInitialized] = React.useState(false)

    /** Register an async effect that will fetch the user's session whenever the `refresh` state is
     * set. This is useful when a user has just logged in (as their cached credentials are
     * out of date, so this will update them). */
    const session = hooks.useAsyncEffect(
        null,
        async () => {
            const innerSession = await userSession()
            setInitialized(true)
            return innerSession
        },
        [refresh]
    )

    /** Register an effect that will listen for authentication events. When the event occurs, we
     * will refresh or clear the user's session, forcing a re-render of the page with the new
     * session.
     *
     * For example, if a user clicks the signout button, this will clear the user's session, which
     * means we want the login screen to render (which is a child of this provider). */
    React.useEffect(() => {
        const listener: listen.ListenerCallback = event => {
            switch (event) {
                case listen.AuthEvent.signIn:
                case listen.AuthEvent.signOut: {
                    doRefresh()
                    break
                }
                case listen.AuthEvent.customOAuthState:
                case listen.AuthEvent.cognitoHostedUi: {
                    /** AWS Amplify doesn't provide a way to set the redirect URL for the OAuth flow, so
                     * we have to hack it by replacing the URL in the browser's history. This is done
                     * because otherwise the user will be redirected to a URL like `enso://auth`, which
                     * will not work.
                     *
                     * See:
                     * https://github.com/aws-amplify/amplify-js/issues/3391#issuecomment-756473970 */
                    history.replaceState({}, '', mainPageUrl)
                    doRefresh()
                    break
                }
                default: {
                    throw new error.UnreachableCaseError(event)
                }
            }
        }

        const cancel = registerAuthEventListener(listener)
        /** Return the `cancel` function from the `useEffect`, which ensures that the listener is
         * cleaned up between renders. This must be done because the `useEffect` will be called
         * multiple times during the lifetime of the component. */
        return cancel
    }, [doRefresh, registerAuthEventListener, mainPageUrl])

    const deinitializeSession = () => {
        setInitialized(false)
    }

    return (
        <SessionContext.Provider value={{ session, deinitializeSession }}>
            {initialized && children}
        </SessionContext.Provider>
    )
}

// ==================
// === useSession ===
// ==================

/** React context hook returning the session of the authenticated user. */
export function useSession() {
    return React.useContext(SessionContext)
}
