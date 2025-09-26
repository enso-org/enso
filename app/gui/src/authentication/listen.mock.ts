/** @file */
export type AuthEvent = 'customOAuthState' | 'cognitoHostedUI' | 'signIn' | 'signOut'

export let authEventListener: ((event: AuthEvent, data?: unknown) => void) | null

/** Listen to authentication state changes. */
export function registerAuthEventListener(listener: (event: AuthEvent, data?: unknown) => void) {
  authEventListener = listener
}
