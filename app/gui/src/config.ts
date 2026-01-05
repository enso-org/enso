/**
 * @file This file defines a global environemnt config that can be used throughout the app.
 * It is included directly into index.html and kept as a separate built artifact, so that
 * we can easily replace its contents in a separate build postprocessing step in `BUILD.bazel`.
 */

const processEnv = typeof process !== 'undefined' ? process.env : {}

/**
 * When running dev server, the config variables are grabbed from appropriate .env file.
 */
export const $config = {
  ENVIRONMENT: processEnv.ENSO_IDE_ENVIRONMENT ?? import.meta.env.ENSO_IDE_ENVIRONMENT,
  ENSO_HOST: processEnv.ENSO_IDE_HOST ?? import.meta.env.ENSO_IDE_HOST,
  API_URL: processEnv.ENSO_IDE_API_URL ?? import.meta.env.ENSO_IDE_API_URL,
  SENTRY_DSN: processEnv.ENSO_IDE_SENTRY_DSN ?? import.meta.env.ENSO_IDE_SENTRY_DSN,
  STRIPE_KEY: processEnv.ENSO_IDE_STRIPE_KEY ?? import.meta.env.ENSO_IDE_STRIPE_KEY,
  AUTH_ENDPOINT: processEnv.ENSO_IDE_AUTH_ENDPOINT ?? import.meta.env.ENSO_IDE_AUTH_ENDPOINT,
  COGNITO_USER_POOL_ID:
    processEnv.ENSO_IDE_COGNITO_USER_POOL_ID ?? import.meta.env.ENSO_IDE_COGNITO_USER_POOL_ID,
  COGNITO_USER_POOL_WEB_CLIENT_ID:
    processEnv.ENSO_IDE_COGNITO_USER_POOL_WEB_CLIENT_ID ??
    import.meta.env.ENSO_IDE_COGNITO_USER_POOL_WEB_CLIENT_ID,
  GOOGLE_ANALYTICS_TAG:
    processEnv.ENSO_IDE_GOOGLE_ANALYTICS_TAG ?? import.meta.env.ENSO_IDE_GOOGLE_ANALYTICS_TAG,
  COGNITO_DOMAIN: processEnv.ENSO_IDE_COGNITO_DOMAIN ?? import.meta.env.ENSO_IDE_COGNITO_DOMAIN,
  COGNITO_REGION: processEnv.ENSO_IDE_COGNITO_REGION ?? import.meta.env.ENSO_IDE_COGNITO_REGION,
  VERSION: processEnv.ENSO_IDE_VERSION ?? import.meta.env.ENSO_IDE_VERSION,
  COMMIT_HASH: processEnv.ENSO_IDE_COMMIT_HASH ?? import.meta.env.ENSO_IDE_COMMIT_HASH,
  YDOC_SERVER_URL: processEnv.ENSO_IDE_YDOC_SERVER_URL ?? import.meta.env.ENSO_IDE_YDOC_SERVER_URL,
  CLOUD_BUILD: processEnv.ENSO_IDE_CLOUD_BUILD ?? import.meta.env.ENSO_IDE_CLOUD_BUILD,
  AG_GRID_LICENSE_KEY:
    processEnv.ENSO_IDE_AG_GRID_LICENSE_KEY ?? import.meta.env.ENSO_IDE_AG_GRID_LICENSE_KEY,
  GOOGLE_OAUTH_CLIENT_ID:
    processEnv.ENSO_IDE_GOOGLE_OAUTH_CLIENT_ID ?? import.meta.env.ENSO_IDE_GOOGLE_OAUTH_CLIENT_ID,
  STRAVA_OAUTH_CLIENT_ID:
    processEnv.ENSO_IDE_STRAVA_OAUTH_CLIENT_ID ?? import.meta.env.ENSO_IDE_STRAVA_OAUTH_CLIENT_ID,
  MS365_OAUTH_CLIENT_ID:
    processEnv.ENSO_IDE_MS365_OAUTH_CLIENT_ID ?? import.meta.env.ENSO_IDE_MS365_OAUTH_CLIENT_ID,
  MAPBOX_API_TOKEN:
    processEnv.ENSO_IDE_MAPBOX_API_TOKEN ??
    (typeof window !== 'undefined' ? window.api?.mapBoxApiToken() : undefined) ??
    import.meta.env.ENSO_IDE_MAPBOX_API_TOKEN,
} as const

// Undefined env variables are typed as `any`, but we want them to be `string | undefined`.
export type $Config = {
  [K in keyof typeof $config]: unknown extends (typeof $config)[K] ? string | undefined
  : (typeof $config)[K]
}

if (typeof window !== 'undefined') {
  Object.defineProperty(window, '$config', {
    writable: false,
    configurable: false,
    enumerable: false,
    value: $config,
  })
}
