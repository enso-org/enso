/**
 * @file This file defines a global environment config that can be used throughout the app.
 * It is included directly into index.html and kept as a separate built artifact, so that
 * we can easily replace its contents in a separate build postprocessing step in `BUILD.bazel`.
 */

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

/** When running dev server, the config variables are grabbed from appropriate .env file. */
export let $config = {
  ENVIRONMENT: process.env.ENSO_IDE_ENVIRONMENT ?? import.meta.env.ENSO_IDE_ENVIRONMENT,
  ENSO_HOST: process.env.ENSO_IDE_HOST ?? import.meta.env.ENSO_IDE_HOST,
  API_URL: process.env.ENSO_IDE_API_URL ?? import.meta.env.ENSO_IDE_API_URL,
  SENTRY_DSN: process.env.ENSO_IDE_SENTRY_DSN ?? import.meta.env.ENSO_IDE_SENTRY_DSN,
  STRIPE_KEY: process.env.ENSO_IDE_STRIPE_KEY ?? import.meta.env.ENSO_IDE_STRIPE_KEY,
  AUTH_ENDPOINT: process.env.ENSO_IDE_AUTH_ENDPOINT ?? import.meta.env.ENSO_IDE_AUTH_ENDPOINT,
  COGNITO_USER_POOL_ID:
    process.env.ENSO_IDE_COGNITO_USER_POOL_ID ?? import.meta.env.ENSO_IDE_COGNITO_USER_POOL_ID,
  COGNITO_USER_POOL_WEB_CLIENT_ID:
    process.env.ENSO_IDE_COGNITO_USER_POOL_WEB_CLIENT_ID ??
    import.meta.env.ENSO_IDE_COGNITO_USER_POOL_WEB_CLIENT_ID,
  GOOGLE_ANALYTICS_TAG:
    process.env.ENSO_IDE_GOOGLE_ANALYTICS_TAG ?? import.meta.env.ENSO_IDE_GOOGLE_ANALYTICS_TAG,
  COGNITO_DOMAIN: process.env.ENSO_IDE_COGNITO_DOMAIN ?? import.meta.env.ENSO_IDE_COGNITO_DOMAIN,
  COGNITO_REGION: process.env.ENSO_IDE_COGNITO_REGION ?? import.meta.env.ENSO_IDE_COGNITO_REGION,
  VERSION: process.env.ENSO_IDE_VERSION ?? import.meta.env.ENSO_IDE_VERSION,
  COMMIT_HASH: process.env.ENSO_IDE_COMMIT_HASH ?? import.meta.env.ENSO_IDE_COMMIT_HASH,
  PROJECT_MANAGER_URL:
    process.env.ENSO_IDE_PROJECT_MANAGER_URL ?? import.meta.env.ENSO_IDE_PROJECT_MANAGER_URL,
  YDOC_SERVER_URL: process.env.ENSO_IDE_YDOC_SERVER_URL ?? import.meta.env.ENSO_IDE_YDOC_SERVER_URL,
  CLOUD_BUILD: process.env.ENSO_IDE_CLOUD_BUILD ?? import.meta.env.ENSO_IDE_CLOUD_BUILD,
  AG_GRID_LICENSE_KEY:
    process.env.ENSO_IDE_AG_GRID_LICENSE_KEY ?? import.meta.env.ENSO_IDE_AG_GRID_LICENSE_KEY,
  GOOGLE_OAUTH_CLIENT_ID:
    process.env.ENSO_IDE_GOOGLE_OAUTH_CLIENT_ID ?? import.meta.env.ENSO_IDE_GOOGLE_OAUTH_CLIENT_ID,
  STRAVA_OAUTH_CLIENT_ID:
    process.env.ENSO_IDE_STRAVA_OAUTH_CLIENT_ID ?? import.meta.env.ENSO_IDE_STRAVA_OAUTH_CLIENT_ID,
  MS365_OAUTH_CLIENT_ID:
    process.env.ENSO_IDE_MS365_OAUTH_CLIENT_ID ?? import.meta.env.ENSO_IDE_MS365_OAUTH_CLIENT_ID,
  MAPBOX_API_TOKEN:
    (typeof window === 'object' &&
      window &&
      'api' in window &&
      typeof window.api === 'object' &&
      window.api &&
      'mapBoxApiToken' in window.api &&
      typeof window.api.mapBoxApiToken === 'function' &&
      window.api?.mapBoxApiToken()) ||
    (process.env.ENSO_IDE_MAPBOX_API_TOKEN ?? import.meta.env.ENSO_IDE_MAPBOX_API_TOKEN),
} as const

/** Sets the global configuration. */
export function setConfig(config: typeof $config) {
  $config = config
}

// Undefined env variables are typed as `any`, but we want them to be `string | undefined`.
export type $Config = {
  [K in keyof typeof $config]: unknown extends (typeof $config)[K] ? string | undefined
  : (typeof $config)[K]
}
