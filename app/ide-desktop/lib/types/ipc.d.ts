/**
 * @file This file contains the types for the IPC events.
 * The IPC events are used to communicate between the main and renderer processes.
 */
/**
 * Payload for the save-access-token event that saves an access token to a credentials file.
 */
interface SaveAccessTokenPayload {
    /**
     * The JWT token to save.
     */
    readonly accessToken: string
    /**
     * The Cognito app integration client id
     */
    readonly clientId: string
    /**
     * The refresh token taken from initAuth flow.
     */
    readonly refreshToken: string
    /**
     * The Cognito url to refresh the token.
     */
    readonly refreshUrl: string
    /**
     * Time when the token will expire.
     * This is a string representation of a date in ISO 8601 format (e.g. "2021-01-01T00:00:00Z").
     */
    readonly expireAt: string
}
