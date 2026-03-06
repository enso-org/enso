/**
 * @file This file defines a global environemnt config that can be used throughout the app.
 * It is included directly into index.html and kept as a separate built artifact, so that
 * we can easily replace its contents in a separate build postprocessing step in `BUILD.bazel`.
 */
/**
 * When running dev server, the config variables are grabbed from appropriate .env file.
 */
const DEFAULT_CONFIG = {
  HOST: import.meta.env.ENSO_IDE_HOST,
  ENVIRONMENT: import.meta.env.ENSO_IDE_ENVIRONMENT,
  SENTRY_DSN: import.meta.env.ENSO_IDE_SENTRY_DSN,
  API_URL: import.meta.env.ENSO_IDE_API_URL,
  VERSION: import.meta.env.ENSO_IDE_VERSION,
  COMMIT_HASH: import.meta.env.ENSO_IDE_COMMIT_HASH,
  GOOGLE_ANALYTICS_TAG: import.meta.env.ENSO_IDE_GOOGLE_ANALYTICS_TAG,
  YDOC_SERVER_URL: import.meta.env.ENSO_IDE_YDOC_SERVER_URL,
  CLOUD_BUILD: import.meta.env.ENSO_IDE_CLOUD_BUILD,
  AG_GRID_LICENSE_KEY: import.meta.env.ENSO_IDE_AG_GRID_LICENSE_KEY,
} as const

// Undefined env variables are typed as `any`, but we want them to be `string | undefined`.
export type $Config = {
  [K in keyof typeof DEFAULT_CONFIG]: unknown extends (typeof DEFAULT_CONFIG)[K] ?
    string | undefined
  : (typeof DEFAULT_CONFIG)[K]
}

const injectedConfig: $Config | undefined =
  typeof window !== 'undefined' && Object.prototype.hasOwnProperty.call(window, '$config') ?
    window.$config
  : undefined

export const $config: $Config = injectedConfig ?? DEFAULT_CONFIG

if (typeof window !== 'undefined') {
  if (!Object.prototype.hasOwnProperty.call(window, '$config')) {
    Object.defineProperty(window, '$config', {
      writable: false,
      configurable: false,
      enumerable: false,
      value: $config,
    })
  }
}
