/**
 * @file Module for listening to authentication events emitted by Amplify.
 *
 * Listening to authentication events is necessary to update the authentication state of the
 * application. For example, if the user signs out, we want to clear the authentication state so
 * that the login screen is rendered.
 */
import * as amplify from 'aws-amplify/utils'

/** Name of the string identifying the "hub" that AWS Amplify issues authentication events on. */
const AUTHENTICATION_HUB = 'auth'

/**
 * Authentication state change events.
 *
 * These are issues by AWS Amplify when it detects a change in authentication state. For example,
 * when the user signs in or signs out by accessing a page like `enso://auth?code=...&state=...`.
 */
export enum AuthEvent {
  /** Issued when the user has passed custom OAuth state parameters to some other auth event. */
  customOAuthState = 'customOAuthState',
  /** Issued when the user completes the sign-in process (via federated identity provider). */
  signInWithRedirect = 'signInWithRedirect',
  /** Issued when the user completes the sign-in process (via email/password). */
  signedIn = 'signedIn',
  /** Issued when the user signs out. */
  signedOut = 'signedOut',
}

/** Return `true` if the given `string` is an {@link AuthEvent}. */
function isAuthEvent(value: string): value is AuthEvent {
  return Object.values<string>(AuthEvent).includes(value)
}

/**
 * Callback called in response to authentication state changes.
 * @see {@link amplify.Hub.listen}.
 */
export type ListenerCallback = (event: AuthEvent, data?: unknown) => void

/**
 * Unsubscribe the {@link ListenerCallback} from authentication state changes.
 * @see {@link amplify.Hub.listen}.
 */
type UnsubscribeFunction = () => void

/**
 * Used to subscribe to {@link AuthEvent}s.
 *
 * Returns a function that MUST be called before re-subscribing,
 * to avoid memory leaks or duplicate event handlers.
 */
export type ListenFunction = (listener: ListenerCallback) => UnsubscribeFunction

/** Listen to authentication state changes. */
export function registerAuthEventListener(listener: ListenerCallback): () => void {
  return amplify.Hub.listen(AUTHENTICATION_HUB, (data) => {
    if (isAuthEvent(data.payload.event)) {
      listener(data.payload.event)
    }
  })
}
