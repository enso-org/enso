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
  AUTH_ENDPOINT: import.meta.env.ENSO_IDE_AUTH_ENDPOINT,
  AUTH0_CLIENT_ID: import.meta.env.ENSO_IDE_AUTH0_CLIENT_ID,
  AUTH0_DOMAIN: import.meta.env.ENSO_IDE_AUTH0_DOMAIN,
  GOOGLE_ANALYTICS_TAG: import.meta.env.ENSO_IDE_GOOGLE_ANALYTICS_TAG,
  VERSION: import.meta.env.ENSO_IDE_VERSION,
  COMMIT_HASH: import.meta.env.ENSO_IDE_COMMIT_HASH,
  PROJECT_MANAGER_URL: import.meta.env.ENSO_IDE_PROJECT_MANAGER_URL,
  YDOC_SERVER_URL: import.meta.env.ENSO_IDE_YDOC_SERVER_URL,
  CLOUD_BUILD: import.meta.env.ENSO_IDE_CLOUD_BUILD,
  AG_GRID_LICENSE_KEY: import.meta.env.ENSO_IDE_AG_GRID_LICENSE_KEY,
  GOOGLE_OAUTH_CLIENT_ID: import.meta.env.ENSO_IDE_GOOGLE_OAUTH_CLIENT_ID,
  MAPBOX_API_TOKEN: window.mapBoxApiToken?.() || import.meta.env.ENSO_IDE_MAPBOX_API_TOKEN,
} as const

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
