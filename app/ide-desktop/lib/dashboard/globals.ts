/** @file An object containing app configuration to inject.
 *
 * This includes:
 * - the base URL for backend endpoints
 * - the WebSocket URL for the chatbot
 * - the unique identifier for the cloud environment, for use in Sentry logs
 * - Stripe, Sentry and Amplify public keys */

/** The value as JSON if it is not nullish, else `'undefined'`. */
function stringify(string: unknown) {
  return string == null ? 'undefined' : JSON.stringify(string)
}

/** An object containing app configuration to inject.
 *
 * This includes:
 * - the base URL for backend endpoints
 * - the WebSocket URL for the chatbot
 * - the unique identifier for the cloud environment, for use in Sentry logs
 * - Stripe, Sentry and Amplify public keys */
// eslint-disable-next-line @typescript-eslint/no-magic-numbers
export function globals(devMode: boolean, serverPort = 8080) {
  return {
    /* eslint-disable @typescript-eslint/naming-convention */
    // Whether the application is being run locally. This determines whether some variables are
    // exposed to the global scope.
    'process.env.NODE_ENV': stringify(devMode ? 'development' : 'production'),
    'process.env.ENSO_CLOUD_REDIRECT': stringify(
      // The actual environment variable does not necessarily exist.
      // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
      process.env.ENSO_CLOUD_REDIRECT ?? `http://localhost:${serverPort}`
    ),
    'process.env.ENSO_CLOUD_ENVIRONMENT': stringify(
      // The actual environment variable does not necessarily exist.
      // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
      process.env.ENSO_CLOUD_ENVIRONMENT ?? 'production'
    ),
    'process.env.ENSO_CLOUD_API_URL': stringify(process.env.ENSO_CLOUD_API_URL),
    'process.env.ENSO_CLOUD_SENTRY_DSN': stringify(process.env.ENSO_CLOUD_SENTRY_DSN),
    'process.env.ENSO_CLOUD_STRIPE_KEY': stringify(process.env.ENSO_CLOUD_STRIPE_KEY),
    'process.env.ENSO_CLOUD_CHAT_URL': stringify(process.env.ENSO_CLOUD_CHAT_URL),
    'process.env.ENSO_CLOUD_COGNITO_USER_POOL_ID': stringify(
      process.env.ENSO_CLOUD_COGNITO_USER_POOL_ID
    ),
    'process.env.ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID': stringify(
      process.env.ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID
    ),
    'process.env.ENSO_CLOUD_COGNITO_DOMAIN': stringify(process.env.ENSO_CLOUD_COGNITO_DOMAIN),
    'process.env.ENSO_CLOUD_COGNITO_REGION': stringify(process.env.ENSO_CLOUD_COGNITO_REGION),
    /* eslint-enable @typescript-eslint/naming-convention */
  }
}
