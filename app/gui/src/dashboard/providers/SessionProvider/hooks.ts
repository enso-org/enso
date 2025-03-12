/** @file Hooks for `SessionProvider`. */
import { useContext } from 'react'
import invariant from 'tiny-invariant'
import { SessionContext, type SessionContextType } from './constants'

/**
 * React context hook returning the session of the authenticated user.
 * @throws {Error} when used outside a {@link SessionProvider}.
 */
export function useSession() {
  const context = useContext(SessionContext)

  invariant(context != null, '`useSession` can only be used inside an `<SessionProvider />`.')

  return context
}

/** Return API to work with a session. */
export function useSessionAPI(): Omit<SessionContextType, 'session'> {
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const { session, ...api } = useSession()

  return api
}

/**
 * React context hook returning the session of the authenticated user.
 * @throws {Error} if the session is not defined.
 */
export function useSessionStrict() {
  const { session } = useSession()
  invariant(session != null, 'Session must be defined')
  return { session } as const
}
