/** @file Constants for `AuthProvider`. */
import type { User } from '#/services/Backend'
import type { QueryKey, QueryObserverResult, RefetchOptions } from '@tanstack/react-query'
import { createContext } from 'react'
import type { UserSession } from './AuthProvider'

/** Possible types of {@link BaseUserSession}. */
export enum UserSessionType {
  offline = 'offline',
  partial = 'partial',
  full = 'full',
}

/**
 * Interface returned by the `useAuth` hook.
 *
 * Contains the currently authenticated user's session data, as well as methods for signing in,
 * signing out, etc. All interactions with the authentication API should be done through this
 * interface.
 *
 * See `Cognito` for details on each of the authentication functions.
 */
export interface AuthContextType {
  readonly authQueryKey: QueryKey
  readonly setUsername: (username: string) => Promise<boolean>
  /** @deprecated Never use this function. Prefer particular functions like `setUsername` or `deleteUser`. */
  readonly setUser: (user: Partial<User>) => void
  readonly deleteUser: () => Promise<boolean>
  readonly restoreUser: () => Promise<boolean>
  readonly refetchSession: (
    options?: RefetchOptions,
  ) => Promise<QueryObserverResult<UserSession | null>>
  /**
   * Session containing the currently authenticated user's authentication information.
   *
   * If the user has not signed in, the session will be `null`.
   */
  readonly session: UserSession | null
  /** Return `true` if the user is marked for deletion. */
  readonly isUserMarkedForDeletion: () => boolean
  /** Return `true` if the user is deleted completely. */
  readonly isUserDeleted: () => boolean
  /** Return `true` if the user is soft deleted. */
  readonly isUserSoftDeleted: () => boolean
}

export const AuthContext = createContext<AuthContextType | null>(null)
