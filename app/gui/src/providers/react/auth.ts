import type { AuthStore, UserSession } from '$/providers/auth'
import * as react from 'react'
import invariant from 'tiny-invariant'
import { useInReactFunction, useVueValue } from './common'

export const AuthContext = react.createContext<AuthStore | null>(null)
export const useAuth = useInReactFunction(AuthContext)

/** A React context hook returning the user session for a user that may or may not be logged in. */
export function useUserSession() {
  const auth = useAuth()
  return useVueValue(react.useCallback(() => auth.session, [auth]))
}

/** A React context hook returning the user session for a user that is fully logged in. */
export function useFullUserSession(): UserSession {
  const session = useUserSession()
  invariant(session, 'Expected a user session.')
  return session
}

/** A React context hook returning the user session for a user that is fully logged in. */
export function useUser() {
  return useFullUserSession().user
}
