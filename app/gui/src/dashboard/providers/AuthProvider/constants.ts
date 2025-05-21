/** @file Constants for `AuthProvider`. */
import type { UserSession as CognitoUserSession } from '#/authentication/cognito'
import type { UserSessionType } from '#/providers/AuthProvider/types'
import type { User } from '#/services/Backend'
import type { QueryKey, QueryObserverResult, RefetchOptions } from '@tanstack/react-query'
import { createContext } from 'react'

/** Properties common to all {@link UserSession}s. */
interface BaseUserSession extends CognitoUserSession {
  /** A discriminator for TypeScript to be able to disambiguate between `UserSession` variants. */
  readonly type: UserSessionType
}

/**
 * Object containing the currently signed-in user's session data, if the user has not yet set their
 * username.
 *
 * If a user has not yet set their username, they do not yet have an organization associated with
 * their account. Otherwise, this type is identical to the `Session` type. This type should ONLY be
 * used by the `SetUsername` component.
 */
export interface PartialUserSession extends BaseUserSession {
  readonly type: 'partial'
}

/** Object containing the currently signed-in user's session data. */
export interface FullUserSession extends BaseUserSession {
  /** User's organization information. */
  readonly type: 'full'
  readonly user: User
}

/**
 * A user session for a user that may be either fully registered,
 * or in the process of registering.
 */
export type UserSession = FullUserSession | PartialUserSession

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
