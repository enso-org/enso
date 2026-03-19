/** @file */
export enum AuthEvent {
  customOAuthState = 'customOAuthState',
  signInWithRedirect = 'signInWithRedirect',
  signedIn = 'signedIn',
  signedOut = 'signedOut',
}

export let authEventListener: ((event: AuthEvent, data?: unknown) => void) | null

/** Listen to authentication state changes. */
export function registerAuthEventListener(listener: (event: AuthEvent, data?: unknown) => void) {
  authEventListener = listener
}
