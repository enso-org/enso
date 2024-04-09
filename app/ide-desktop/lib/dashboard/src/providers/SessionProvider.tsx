/** @file Provider for the {@link SessionContextType}, which contains information about the
 * currently authenticated user's session. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as asyncEffectHooks from '#/hooks/asyncEffectHooks'
import * as refreshHooks from '#/hooks/refreshHooks'

import * as errorModule from '#/utilities/error'

import type * as cognito from '#/authentication/cognito'
import * as listen from '#/authentication/listen'

// ======================
// === SessionContext ===
// ======================

/** State contained in a {@link SessionContext}. */
interface SessionContextType {
  readonly session: cognito.UserSession | null
  /** Set `initialized` to false. Must be called when logging out. */
  readonly deinitializeSession: () => void
  readonly onSessionError: (callback: (error: Error) => void) => () => void
}

const SessionContext = React.createContext<SessionContextType | null>(null)

// =======================
// === SessionProvider ===
// =======================

/** Props for a {@link SessionProvider}. */
export interface SessionProviderProps {
  /** The URL that the content of the app is served at, by Electron.
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
  readonly mainPageUrl: URL
  readonly registerAuthEventListener: listen.ListenFunction | null
  readonly userSession: (() => Promise<cognito.UserSession | null>) | null
  readonly refreshUserSession: (() => Promise<void>) | null
  readonly children: React.ReactNode
}

const FIVE_MINUTES_MS = 300_000
const SIX_HOURS_MS = 21_600_000

/** A React provider for the session of the authenticated user. */
export default function SessionProvider(props: SessionProviderProps) {
  const { mainPageUrl, children, userSession, registerAuthEventListener, refreshUserSession } =
    props
  const [refresh, doRefresh] = refreshHooks.useRefresh()
  const [initialized, setInitialized] = React.useState(false)
  const errorCallbacks = React.useRef(new Set<(error: Error) => void>())

  /** Returns a function to unregister the listener. */
  const onSessionError = React.useCallback((callback: (error: Error) => void) => {
    errorCallbacks.current.add(callback)
    return () => {
      errorCallbacks.current.delete(callback)
    }
  }, [])

  // Register an async effect that will fetch the user's session whenever the `refresh` state is
  // set. This is useful when a user has just logged in (as their cached credentials are
  // out of date, so this will update them).
  const session = asyncEffectHooks.useAsyncEffect(
    null,
    async () => {
      if (userSession == null) {
        setInitialized(true)
        return null
      } else {
        try {
          const innerSession = await userSession()
          setInitialized(true)
          return innerSession
        } catch (error) {
          if (error instanceof Error) {
            for (const listener of errorCallbacks.current) {
              listener(error)
            }
          }
          throw error
        }
      }
    },
    [refresh]
  )

  const timeUntilRefresh = session
    ? // If the session has not expired, we should refresh it when it is 5 minutes from expiring.
      new Date(session.expireAt).getTime() - Date.now() - FIVE_MINUTES_MS
    : Infinity

  reactQuery.useQuery({
    queryKey: ['userSession'],
    queryFn: refreshUserSession ? () => refreshUserSession().then(doRefresh) : reactQuery.skipToken,
    refetchOnWindowFocus: true,
    refetchIntervalInBackground: true,
    refetchInterval: timeUntilRefresh < SIX_HOURS_MS ? timeUntilRefresh : SIX_HOURS_MS,
  })

  // Register an effect that will listen for authentication events. When the event occurs, we
  // will refresh or clear the user's session, forcing a re-render of the page with the new
  // session.
  //
  // For example, if a user clicks the "sign out" button, this will clear the user's session, which
  // means the login screen (which is a child of this provider) should render.
  React.useEffect(
    () =>
      registerAuthEventListener?.(event => {
        switch (event) {
          case listen.AuthEvent.signIn:
          case listen.AuthEvent.signOut: {
            doRefresh()
            break
          }
          case listen.AuthEvent.customOAuthState:
          case listen.AuthEvent.cognitoHostedUi: {
            // AWS Amplify doesn't provide a way to set the redirect URL for the OAuth flow, so
            // we have to hack it by replacing the URL in the browser's history. This is done
            // because otherwise the user will be redirected to a URL like `enso://auth`, which
            // will not work.
            // See https://github.com/aws-amplify/amplify-js/issues/3391#issuecomment-756473970
            history.replaceState({}, '', mainPageUrl)
            doRefresh()
            break
          }
          default: {
            throw new errorModule.UnreachableCaseError(event)
          }
        }
      }),
    [doRefresh, registerAuthEventListener, mainPageUrl]
  )

  const deinitializeSession = () => {
    setInitialized(false)
  }

  return (
    <SessionContext.Provider value={{ session, deinitializeSession, onSessionError }}>
      {initialized && children}
    </SessionContext.Provider>
  )
}

// ==================
// === useSession ===
// ==================

/** React context hook returning the session of the authenticated user.
 * @throws {Error} when used outside a {@link SessionProvider}. */
export function useSession() {
  const context = React.useContext(SessionContext)
  if (context == null) {
    throw new Error('`useSession` can only be used inside an `<SessionProvider />`.')
  } else {
    return context
  }
}
