/** @file */

/**
 * Authentication state change events.
 *
 * These are issues by AWS Amplify when it detects a change in authentication state. For example,
 * when the user signs in or signs out by accessing a page like `enso://auth?code=...&state=...`.
 */
export type AuthEvent = (typeof AuthEvent)[keyof typeof AuthEvent]
export const AuthEvent = {
  /** Issued when the user has passed custom OAuth state parameters to some other auth event. */
  customOAuthState: 'customOAuthState',
  /** Issued when the user completes the sign-in process (via federated identity provider). */
  cognitoHostedUi: 'cognitoHostedUI',
  /** Issued when the user completes the sign-in process (via email/password). */
  signIn: 'signIn',
  /** Issued when the user signs out. */
  signOut: 'signOut',
} as const

export let authEventListener: ((event: AuthEvent, data?: unknown) => void) | null

/** Listen to authentication state changes. */
export function registerAuthEventListener(listener: (event: AuthEvent, data?: unknown) => void) {
  authEventListener = listener
}
