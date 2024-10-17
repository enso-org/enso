/** @file Functions for managing app configuration. */

/**
 * Read environment variables from a file based on the `ENSO_CLOUD_ENV_FILE_NAME`
 * environment variable. Reads from `.env` if the variable is blank or absent.
 * DOES NOT override existing environment variables if the variable is absent.
 */
export function readEnvironmentFromFile(): Promise<void>

/**
 * An object containing app configuration to inject.
 *
 * This includes:
 * - the base URL for backend endpoints
 * - the WebSocket URL for the chatbot
 * - the unique identifier for the cloud environment, for use in Sentry logs
 * - Stripe, Sentry and Amplify public keys
 */
export function getDefines(serverPort?: number): Record<string, string>

/** Load test environment variables, useful for when the Cloud backend is mocked or unnecessary. */
export function loadTestEnvironmentVariables(): void
