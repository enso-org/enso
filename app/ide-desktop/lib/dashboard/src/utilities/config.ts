/** @file Configuration definition for the Dashboard. */
import * as newtype from '#/utilities/newtype'

import * as auth from '#/authentication/config'

// =============
// === Types ===
// =============

/** Base URL for requests to our Cloud API backend. */
type ApiUrl = newtype.Newtype<`http://${string}` | `https://${string}`, 'ApiUrl'>
/** Create an {@link ApiUrl}. */
// This is a constructor function that constructs values of the type after which it is named.
// eslint-disable-next-line @typescript-eslint/no-redeclare
export const ApiUrl = newtype.newtypeConstructor<ApiUrl>()

/** URL to the websocket endpoint of the Help Chat. */
type ChatUrl = newtype.Newtype<`ws://${string}` | `wss://${string}`, 'ChatUrl'>

// This is a constructor function that constructs values of the type after which it is named.
// eslint-disable-next-line @typescript-eslint/no-redeclare
export const ChatUrl = newtype.newtypeConstructor<ChatUrl>()

// =================
// === Constants ===
// =================

/** The web domain of the cloud website. */
export const CLOUD_DOMAIN = 'https://cloud.enso.org'

/** The current environment that we're running in. */
export const ENVIRONMENT: Environment = typeof CLOUD_ENV !== 'undefined' ? CLOUD_ENV : 'production'

/** All possible URLs used as the OAuth redirects when running the cloud app. */
const CLOUD_REDIRECTS = {
  /** In development, a fixed port is used so that the redirect URL can be known ahead of time.
   * The redirect URL must be known ahead of time because it is registered with the OAuth provider
   * when it is created. In the native app, the port is unpredictable, but this is not a problem
   * because the native app does not use port-based redirects, but deep links. */
  development: auth.OAuthRedirect('http://localhost:8080'),
  production: auth.OAuthRedirect(
    typeof REDIRECT_OVERRIDE !== 'undefined' ? REDIRECT_OVERRIDE : CLOUD_DOMAIN
  ),
}

/** All possible API URLs, sorted by environment. */
const API_URLS = {
  pbuchu: ApiUrl('https://xw0g8j3tsb.execute-api.eu-west-1.amazonaws.com'),
  npekin: ApiUrl('https://opk1cxpwec.execute-api.eu-west-1.amazonaws.com'),
  npekin2: ApiUrl('https://8rf1a7iy49.execute-api.eu-west-1.amazonaws.com'),
  production: ApiUrl('https://7aqkn3tnbc.execute-api.eu-west-1.amazonaws.com'),
}

/**
 * All possible Help Chat endpoint URLs, sorted by environment.
 *
 * In development mode, the chat bot will need to be run locally:
 * https://github.com/enso-org/enso-bot */
const CHAT_URLS = {
  development: ChatUrl('ws://localhost:8082'),
  production: ChatUrl('wss://chat.cloud.enso.org'),
}

/** All possible Stripe API public keys, sorted by environment. */
const STRIPE_KEYS = {
  npekin:
    'pk_test_51O8REgAjUAkYBrsQooU5iMWumr7D4Vf9H2A671A8zXV87VwDOTenDbJx5g3PN9IjgkbK6omxlp01bGfghA3qZSIu00lYsytprU',
  development:
    'pk_test_51Iv1a0FpIovSdxvQBRpzZpfikr7CD6DFWFF8g2ycjut3d9PXD2Jc9I2j3G1DWWgMfaNzzHyXtvUr2GaNkuQayEzu00YHYfKtGC',
  production:
    'pk_test_51Iv3YNB30SZwisesLWKD1KCUmkkOy2Bbq0zwYO56zgSjdIf00Bw39BC1Zvn4PPjq6GHZd8Q8oaR6M0JlC1K9b1f1007cjnwi4e',
} as const

/** All possible configuration options, sorted by environment. */
const CONFIGS = {
  npekin: {
    cloudRedirect: CLOUD_REDIRECTS.development,
    apiUrl: API_URLS.npekin,
    chatUrl: CHAT_URLS.development,
    stripeKey: STRIPE_KEYS.npekin,
  } satisfies Config,
  npekin2: {
    cloudRedirect: CLOUD_REDIRECTS.development,
    apiUrl: API_URLS.npekin2,
    chatUrl: CHAT_URLS.development,
    stripeKey: STRIPE_KEYS.npekin,
  } satisfies Config,
  pbuchu: {
    cloudRedirect: CLOUD_REDIRECTS.development,
    apiUrl: API_URLS.pbuchu,
    chatUrl: CHAT_URLS.development,
    stripeKey: STRIPE_KEYS.development,
  } satisfies Config,
  production: {
    cloudRedirect: CLOUD_REDIRECTS.production,
    apiUrl: API_URLS.production,
    chatUrl: CHAT_URLS.production,
    stripeKey: STRIPE_KEYS.production,
  } satisfies Config,
}
/** Export the configuration that is currently in use. */
export const ACTIVE_CONFIG: Config = CONFIGS[ENVIRONMENT]

// ==============
// === Config ===
// ==============

/** Interface defining the configuration options that we expect to provide for the Dashboard. */
export interface Config {
  /** URL of the OAuth redirect when running in the cloud app.
   *
   * The desktop app redirects to a static deep link, so it does not have to be configured. */
  readonly cloudRedirect: auth.OAuthRedirect
  /** Base URL for requests to our Cloud API backend. */
  readonly apiUrl: ApiUrl
  /** URL to the websocket endpoint of the Help Chat. */
  readonly chatUrl: ChatUrl
  /** Key used to authenticate with the Stripe API. Must point at the same Stripe account that the
   * backend is configured to use (though the backend will use a secret key, while this component
   * uses a public key). */
  readonly stripeKey: string
}

// ===================
// === Environment ===
// ===================

/** Possible values for the environment/user we're running for and whose infrastructure we're
 * testing against. */
export type Environment = 'npekin' | 'npekin2' | 'pbuchu' | 'production'
