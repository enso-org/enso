/**
 * @file This file defines a global environemnt config that can be used throughout the app.
 * It is included directly into index.html and kept as a separate built artifact, so that
 * we can easily replace its contents in a separate build postprocessing step in `BUILD.bazel`.
 */

/**
 * When running dev server, the config variables are grabbed from appropriate .env file.
 */
const $config = {
  ENVIRONMENT: import.meta.env.ENSO_IDE_ENVIRONMENT,
  ENSO_HOST: import.meta.env.ENSO_IDE_HOST,
  API_URL: import.meta.env.ENSO_IDE_API_URL,
  SENTRY_DSN: import.meta.env.ENSO_IDE_SENTRY_DSN,
  STRIPE_KEY: import.meta.env.ENSO_IDE_STRIPE_KEY,
  CHAT_URL: import.meta.env.ENSO_IDE_CHAT_URL,
  COGNITO_USER_POOL_ID: import.meta.env.ENSO_IDE_COGNITO_USER_POOL_ID,
  COGNITO_USER_POOL_WEB_CLIENT_ID: import.meta.env.ENSO_IDE_COGNITO_USER_POOL_WEB_CLIENT_ID,
  GOOGLE_ANALYTICS_TAG: import.meta.env.ENSO_IDE_GOOGLE_ANALYTICS_TAG,
  COGNITO_DOMAIN: import.meta.env.ENSO_IDE_COGNITO_DOMAIN,
  COGNITO_REGION: import.meta.env.ENSO_IDE_COGNITO_REGION,
  VERSION: import.meta.env.ENSO_IDE_VERSION,
  COMMIT_HASH: import.meta.env.ENSO_IDE_COMMIT_HASH,
  PROJECT_MANAGER_URL: import.meta.env.ENSO_IDE_DEV_PROJECT_MANAGER_URL,
  YDOC_SERVER_URL: import.meta.env.ENSO_IDE_YDOC_SERVER_URL,
  CLOUD_BUILD: import.meta.env.ENSO_IDE_CLOUD_BUILD,
  AG_GRID_LICENSE_KEY: import.meta.env.ENSO_IDE_AG_GRID_LICENSE_KEY,
  MAPBOX_API_TOKEN: import.meta.env.ENSO_IDE_MAPBOX_API_TOKEN,
} as const

console.log($config)

// Undefined env variables are typed as `any`, but we want them to be `string | undefined`.
export type $Config = {
  [K in keyof typeof $config]: unknown extends (typeof $config)[K] ? string | undefined
  : (typeof $config)[K]
}

Object.defineProperty(window, '$config', {
  writable: false,
  configurable: false,
  enumerable: false,
  value: $config,
})
