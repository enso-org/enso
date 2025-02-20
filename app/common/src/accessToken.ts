/** @file Types for IPC events sent between Electron's main process and its renderer process. */

// ===================
// === AccessToken ===
// ===================

/** Credentials to be saved to a credentials file. */
export interface AccessToken {
  /** The user's JWT token. */
  readonly accessToken: string
  /** The ID for the Cognito app integration. */
  readonly clientId: string
  /** The refresh token taken from the authorization flow. */
  readonly refreshToken: string
  /** The Cognito URL to refresh the token. */
  readonly refreshUrl: string
  /**
   * The expiry time of the token.
   * This is a string representation of a date in ISO 8601 format (e.g. "2021-01-01T00:00:00Z").
   */
  readonly expireAt: string
}
