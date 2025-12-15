/**
 * @file This file defines a global environment config that can be used throughout the app.
 * It is included directly into index.html and kept as a separate built artifact, so that
 * we can easily replace its contents in a separate build postprocessing step in `BUILD.bazel`.
 */

import { unsafeKeys } from './utilities/data/object.js'

declare global {
  interface ViteTypeOptions {
    // strictImportMetaEnv: unknown
  }

  // This needs to be ts-ignore, because not all packages have this key defined.
  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
  // @ts-ignore This key is also defined in Vite.
  type ImportMetaEnvFallbackKey =
    'strictImportMetaEnv' extends keyof ViteTypeOptions ? never : string

  interface ImportMetaEnv {
    [key: ImportMetaEnvFallbackKey]: any
    BASE_URL: string
    MODE: string
    DEV: boolean
    PROD: boolean
    SSR: boolean
  }

  interface ImportMeta {
    url: string
    readonly env: ImportMetaEnv
  }
}

const processEnv = typeof process !== 'undefined' ? process.env : {}
const importEnv = import.meta.env ?? {}

/** When running dev server, the config variables are grabbed from appropriate .env file. */
export const $config = {
  ENVIRONMENT: processEnv.ENSO_IDE_ENVIRONMENT ?? importEnv.ENSO_IDE_ENVIRONMENT,
  ENSO_HOST: processEnv.ENSO_IDE_HOST ?? importEnv.ENSO_IDE_HOST ?? 'https://ensoanalytics.com',
  API_URL: processEnv.ENSO_IDE_API_URL ?? importEnv.ENSO_IDE_API_URL,
  SENTRY_DSN: processEnv.ENSO_IDE_SENTRY_DSN ?? importEnv.ENSO_IDE_SENTRY_DSN,
  STRIPE_KEY: processEnv.ENSO_IDE_STRIPE_KEY ?? importEnv.ENSO_IDE_STRIPE_KEY,
  AUTH_ENDPOINT: processEnv.ENSO_IDE_AUTH_ENDPOINT ?? importEnv.ENSO_IDE_AUTH_ENDPOINT,
  COGNITO_USER_POOL_ID:
    processEnv.ENSO_IDE_COGNITO_USER_POOL_ID ?? importEnv.ENSO_IDE_COGNITO_USER_POOL_ID,
  COGNITO_USER_POOL_WEB_CLIENT_ID:
    processEnv.ENSO_IDE_COGNITO_USER_POOL_WEB_CLIENT_ID ??
    importEnv.ENSO_IDE_COGNITO_USER_POOL_WEB_CLIENT_ID,
  GOOGLE_ANALYTICS_TAG:
    processEnv.ENSO_IDE_GOOGLE_ANALYTICS_TAG ?? importEnv.ENSO_IDE_GOOGLE_ANALYTICS_TAG,
  COGNITO_DOMAIN: processEnv.ENSO_IDE_COGNITO_DOMAIN ?? importEnv.ENSO_IDE_COGNITO_DOMAIN,
  COGNITO_REGION: processEnv.ENSO_IDE_COGNITO_REGION ?? importEnv.ENSO_IDE_COGNITO_REGION,
  VERSION: processEnv.ENSO_IDE_VERSION ?? importEnv.ENSO_IDE_VERSION,
  COMMIT_HASH: processEnv.ENSO_IDE_COMMIT_HASH ?? importEnv.ENSO_IDE_COMMIT_HASH,
  PROJECT_MANAGER_URL:
    processEnv.ENSO_IDE_PROJECT_MANAGER_URL ?? importEnv.ENSO_IDE_PROJECT_MANAGER_URL,
  YDOC_SERVER_URL: processEnv.ENSO_IDE_YDOC_SERVER_URL ?? importEnv.ENSO_IDE_YDOC_SERVER_URL,
  CLOUD_BUILD: processEnv.ENSO_IDE_CLOUD_BUILD ?? importEnv.ENSO_IDE_CLOUD_BUILD,
  AG_GRID_LICENSE_KEY:
    processEnv.ENSO_IDE_AG_GRID_LICENSE_KEY ?? importEnv.ENSO_IDE_AG_GRID_LICENSE_KEY,
  GOOGLE_OAUTH_CLIENT_ID:
    processEnv.ENSO_IDE_GOOGLE_OAUTH_CLIENT_ID ?? importEnv.ENSO_IDE_GOOGLE_OAUTH_CLIENT_ID,
  STRAVA_OAUTH_CLIENT_ID:
    processEnv.ENSO_IDE_STRAVA_OAUTH_CLIENT_ID ?? importEnv.ENSO_IDE_STRAVA_OAUTH_CLIENT_ID,
  MS365_OAUTH_CLIENT_ID:
    processEnv.ENSO_IDE_MS365_OAUTH_CLIENT_ID ?? importEnv.ENSO_IDE_MS365_OAUTH_CLIENT_ID,
  MAPBOX_API_TOKEN:
    (typeof window === 'object' &&
      window &&
      'api' in window &&
      typeof window.api === 'object' &&
      window.api &&
      'mapBoxApiToken' in window.api &&
      typeof window.api.mapBoxApiToken === 'function' &&
      window.api?.mapBoxApiToken()) ||
    (processEnv.ENSO_IDE_MAPBOX_API_TOKEN ?? importEnv.ENSO_IDE_MAPBOX_API_TOKEN),
}

/** Sets the global configuration. */
export function setConfig(config: typeof $config) {
  for (const k of unsafeKeys(config)) {
    if (config[k] === undefined) {
      continue
    }
    $config[k] = config[k]
  }
}

// Undefined env variables are typed as `any`, but we want them to be `string | undefined`.
export type $Config = {
  [K in keyof typeof $config]: unknown extends (typeof $config)[K] ? string | undefined
  : (typeof $config)[K]
}
