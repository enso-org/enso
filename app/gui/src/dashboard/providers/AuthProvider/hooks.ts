/** @file Hooks for `AuthProvider`. */
import { useContext } from 'react'
import invariant from 'tiny-invariant'
import type { FullUserSession } from './constants'
import { AuthContext, UserSessionType } from './constants'

/**
 * A React hook that provides access to the authentication context.
 *
 * Only the hook is exported, and not the context, because we only want to use the hook directly and
 * never the context component.
 * @throws {Error} when used outside a {@link AuthProvider}.
 */
export function useAuth() {
  const context = useContext(AuthContext)
  invariant(context != null, '`useAuth` must be used within an `<AuthProvider />`.')
  return context
}

/**
 * A React context hook returning the user session
 * for a user that has not yet completed registration.
 */
export function usePartialUserSession() {
  const { session } = useAuth()
  invariant(session?.type === UserSessionType.partial, 'Expected a partial user session.')
  return session
}

/** A React context hook returning the user session for a user that may or may not be logged in. */
export function useUserSession() {
  return useAuth().session
}

/** A React context hook returning the user session for a user that is fully logged in. */
export function useFullUserSession(): FullUserSession {
  const { session } = useAuth()
  invariant(session?.type === UserSessionType.full, 'Expected a full user session.')
  return session
}

/** A React context hook returning the user session for a user that is fully logged in. */
export function useUser() {
  const { user } = useFullUserSession()
  return user
}
