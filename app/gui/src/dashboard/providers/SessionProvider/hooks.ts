/**
 * @file
 *
 * Hooks for the session provider.
 */
import * as React from 'react'

import invariant from 'tiny-invariant'
import type { SessionContextType } from './types'

export const SessionContext = React.createContext<SessionContextType | null>(null)

/**
 * React context hook returning the session of the authenticated user.
 * @throws {Error} when used outside a {@link SessionProvider}.
 */
export function useSession() {
  const context = React.useContext(SessionContext)

  invariant(context != null, '`useSession` can only be used inside an `<SessionProvider />`.')

  return context
}

/**
 * Returns API to work with a session.
 */
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
