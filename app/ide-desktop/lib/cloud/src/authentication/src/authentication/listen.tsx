import { Hub, HubCallback } from "@aws-amplify/core";



// =================
// === Constants ===
// =================

/** Name of the string identifying the "hub" that AWS Amplify issues authentication events on. */
const AUTHENTICATION_HUB = "auth";



// =================
// === AuthEvent ===
// =================

/**
 * List of all known authentication state change events.
 * 
 * @see {@link AuthEvent}
 */
const ALL_AUTH_EVENTS = [
    /** Issued when the user has passed custom OAuth state parameters to some other auth event. */
    "customOAuthState",
    /** Issued when the user completes the sign-in process (via federated identity provider). */
    "cognitoHostedUI",
    /** Issued when the user completes the sign-in process (via email/password). */
    "signIn",
    /** Issued when the user signs out. */
    "signOut",
];

/**
 * Authentication state change events.
 * 
 * These are issues by AWS Amplify when it detects a change in authentication state. For example,
 * when the user signs in or signs out by accessing a page like `enso://auth?code=...&state=...`.
 */
type AuthEvent = typeof ALL_AUTH_EVENTS[number];

/**
 * Function that returns `true` if the given `string` is an {@link AuthEvent}.
 */
const isAuthEvent = (value: string): value is AuthEvent => ALL_AUTH_EVENTS.includes(value);



// =================================
// === RegisterAuthEventListener ===
// =================================

/**
 * Type of the callback called in response to authentication state changes.
 * 
 * @see {@link Api["listen"]}
 */
type ListenerCallback = (event: AuthEvent, data?: any) => void;

/**
 * Function that unsubscribes the {@link ListenerCallback} from authentication state changes.
 * 
 * @see {@link Api["listen"]}
 */
type UnsubscribeFunction = () => void;

/**
 * A function that can be used to subscribe to {@link AuthEvent}s.
 * 
 * This function takes a {@link ListenerCallback} function as an argument. The callback will be
 * called whenever an {@link AuthEvent} fires. The callback will be called with the
 * {@link AuthEvent} as an argument, as well as optional data associated with the {@link AuthEvent}.
 * 
 * The returned function, when called, returns an {@link UnsubscribeFunction} that can be used to
 * unsubscribe from {@link AuthEvent}s. Ensure that you call this function before re-subscribing to
 * avoid memory leaks or duplicate event handlers.
 */
type ListenFunction = (listener: ListenerCallback) => UnsubscribeFunction;

const registerAuthEventListener: ListenFunction = (listener) => {
    const callback: HubCallback = (data) => isAuthEvent(data.payload.event) && listener(data.payload.event, data.payload.data);
    const cancel = Hub.listen(AUTHENTICATION_HUB, callback);
    return cancel
}

export { ListenFunction }
export default registerAuthEventListener;
