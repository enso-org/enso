/** @file Types for IPC events sent between Electron's main process and its renderer process. */

// ===================
// === AccessToken ===
// ===================

/** Payload for the `save-access-token` event that saves an access token to a credentials file. */
export interface AccessToken {
  /** The JWT token to save. */
  readonly accessToken: string
  /** The Cognito app integration client id. */
  readonly clientId: string
  /** The refresh token taken from authorization flow. */
  readonly refreshToken: string
  /** The Cognito url to refresh the token. */
  readonly refreshUrl: string
  /**
   * The when the token will expire.
   * This is a string representation of a date in ISO 8601 format (e.g. "2021-01-01T00:00:00Z").
   */
  readonly expireAt: string
}
