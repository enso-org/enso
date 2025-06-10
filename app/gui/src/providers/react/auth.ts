import { useStore } from '#/utilities/zustand'
import {
  authOverridesStore,
  AuthStore,
  type FullUserSession,
  UserSessionType,
} from '$/providers/auth'
import * as react from 'react'
import invariant from 'tiny-invariant'
import { useInReactFunction, useVueValue } from './common'

export const AuthContext = react.createContext<AuthStore | null>(null)
export const useAuth = useInReactFunction(AuthContext)

/**
 * A React context hook returning the user session
 * for a user that has not yet completed registration.
 */
export function usePartialUserSession() {
  const session = useUserSession()

  invariant(session?.type === UserSessionType.partial, 'Expected a partial user session.')

  return session
}

/** A React context hook returning the user session for a user that may or may not be logged in. */
export function useUserSession() {
  const auth = useAuth()
  return useVueValue(() => auth.session)
}

/** A React context hook returning the user session for a user that is fully logged in. */
export function useFullUserSession(): FullUserSession {
  const session = useUserSession()

  invariant(session?.type === UserSessionType.full, 'Expected a full user session.')

  return session
}

/** A React context hook returning the user session for a user that is fully logged in. */
export function useUser() {
  const { user } = useFullUserSession()

  return user
}

/** The current overridden plan. */
export function usePlanOverride() {
  return useStore(authOverridesStore, ({ planOverride }) => planOverride)
}

/** A function to set (or unset) the current overridden plan. */
export function useSetPlanOverride() {
  return useStore(authOverridesStore, ({ setPlanOverride }) => setPlanOverride)
}
