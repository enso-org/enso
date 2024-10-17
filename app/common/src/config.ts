/**
 * @file This file defines a global environemnt config that can be used throughout the app.
 * It is included directly into index.html and kept as a separate built artifact, so that
 * we can easily replace its contents in a separate build postprocessing step in `BUILD.bazel`.
 */

const $config = {
  REDIRECT: process.env.ENSO_CLOUD_REDIRECT,
  ENVIRONMENT: process.env.ENSO_CLOUD_ENVIRONMENT,
  API_URL: process.env.ENSO_CLOUD_API_URL,
  SENTRY_DSN: process.env.ENSO_CLOUD_SENTRY_DSN,
  STRIPE_KEY: process.env.ENSO_CLOUD_STRIPE_KEY,
  CHAT_URL: process.env.ENSO_CLOUD_CHAT_URL,
  COGNITO_USER_POOL_ID: process.env.ENSO_CLOUD_COGNITO_USER_POOL_ID,
  COGNITO_USER_POOL_WEB_CLIENT_ID: process.env.ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID,
  GOOGLE_ANALYTICS_TAG: process.env.ENSO_CLOUD_GOOGLE_ANALYTICS_TAG,
  COGNITO_DOMAIN: process.env.ENSO_CLOUD_COGNITO_DOMAIN,
  COGNITO_REGION: process.env.ENSO_CLOUD_COGNITO_REGION,
  VERSION: process.env.ENSO_CLOUD_DASHBOARD_VERSION,
  COMMIT_HASH: process.env.ENSO_CLOUD_DASHBOARD_COMMIT_HASH,
  PROJECT_MANAGER_URL: process.env.PROJECT_MANAGER_URL,
  YDOC_SERVER_URL: process.env.YDOC_SERVER_URL,
  CLOUD_BUILD: process.env.IS_CLOUD_BUILD,
} as const

window.$config = $config

// eslint-disable-next-line @typescript-eslint/no-unused-vars
interface Window {
  $config: typeof $config
}
