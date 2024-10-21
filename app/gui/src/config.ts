/**
 * @file This file defines a global environemnt config that can be used throughout the app.
 * It is included directly into index.html and kept as a separate built artifact, so that
 * we can easily replace its contents in a separate build postprocessing step in `BUILD.bazel`.
 */

/**
 * When running dev server, the config variables are grabbed from appropriate .env file.
 */
const $config = {
  ENVIRONMENT: import.meta.env.MODE,
  API_URL: import.meta.env.API_URL,
  SENTRY_DSN: import.meta.env.SENTRY_DSN,
  STRIPE_KEY: import.meta.env.STRIPE_KEY,
  CHAT_URL: import.meta.env.CHAT_URL,
  COGNITO_USER_POOL_ID: import.meta.env.COGNITO_USER_POOL_ID,
  COGNITO_USER_POOL_WEB_CLIENT_ID: import.meta.env.COGNITO_USER_POOL_WEB_CLIENT_ID,
  GOOGLE_ANALYTICS_TAG: import.meta.env.GOOGLE_ANALYTICS_TAG,
  COGNITO_DOMAIN: import.meta.env.COGNITO_DOMAIN,
  COGNITO_REGION: import.meta.env.COGNITO_REGION,
  VERSION: import.meta.env.VERSION,
  COMMIT_HASH: import.meta.env.COMMIT_HASH,
  PROJECT_MANAGER_URL: import.meta.env.DEV_PROJECT_MANAGER_URL,
  YDOC_SERVER_URL: import.meta.env.YDOC_SERVER_URL,
  CLOUD_BUILD: import.meta.env.CLOUD_BUILD,
} as const

// Undefined import.meta.env variables are typed as `any`, but we want them to be `string | undefined`.
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
