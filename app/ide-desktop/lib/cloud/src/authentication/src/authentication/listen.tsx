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
 * Authentication state change events.
 * 
 * These are issues by AWS Amplify when it detects a change in authentication state. For example,
 * when the user signs in or signs out by accessing a page like `enso://auth?code=...&state=...`.
 */
enum AuthEvent {
    /** Issued when the user has passed custom OAuth state parameters to some other auth event. */
    customOAuthState = "customOAuthState",
    /** Issued when the user completes the sign-in process (via federated identity provider). */
    cognitoHostedUi = "cognitoHostedUI",
    /** Issued when the user completes the sign-in process (via email/password). */
    signIn = "signIn",
    /** Issued when the user signs out. */
    signOut = "signOut",
}

/** Function that returns `true` if the given `string` is an {@link AuthEvent}. */
const isAuthEvent = (value: string): value is AuthEvent => Object.values(AuthEvent).includes(value as AuthEvent);



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
    const callback: HubCallback = (data) => { console.log("hub event", data, JSON.stringify(data)); isAuthEvent(data.payload.event) && listener(data.payload.event, data.payload.data) };
    const cancel = Hub.listen(AUTHENTICATION_HUB, callback);
    return cancel
}

export { ListenerCallback, ListenFunction }
export default registerAuthEventListener;
