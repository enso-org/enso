/** @file Configuration definition for the Dashboard. */

import * as auth from './authentication/config'
import * as utils from './utils'

// =================
// === Constants ===
// =================

/** The current environment that we're running in. */
export const ENVIRONMENT: Environment = 'production'

/** All possible URLs used as the OAuth redirects when running the cloud app. */
const CLOUD_REDIRECTS = {
    /** In development, a fixed port is used so that the redirect URL can be known ahead of time.
     * The redirect URL must be known ahead of time because it is registered with the OAuth provider
     * when it is created. In the native app, the port is unpredictable, but this is not a problem
     * because the native app does not use port-based redirects, but deep links. */
    development: utils.brand<auth.OAuthRedirect>('http://localhost:8081'),
    production: utils.brand<auth.OAuthRedirect>('https://cloud.enso.org'),
}

/** All possible API URLs, sorted by environment. */
const API_URLS = {
    pbuchu: utils.brand<ApiUrl>('https://xw0g8j3tsb.execute-api.eu-west-1.amazonaws.com'),
    production: utils.brand<ApiUrl>('https://7aqkn3tnbc.execute-api.eu-west-1.amazonaws.com'),
}

/** All possible configuration options, sorted by environment. */
const CONFIGS = {
    pbuchu: {
        cloudRedirect: CLOUD_REDIRECTS.development,
        apiUrl: API_URLS.pbuchu,
    } satisfies Config,
    production: {
        cloudRedirect: CLOUD_REDIRECTS.production,
        apiUrl: API_URLS.production,
    } satisfies Config,
}
/** Export the configuration that is currently in use. */
export const ACTIVE_CONFIG: Config = CONFIGS[ENVIRONMENT]

// ==============
// === Config ===
// ==============

/** Interface defining the configuration options that we expect to provide for the Dashboard. */
export interface Config {
    /** URL used as the OAuth redirect when running in the cloud app. */
    cloudRedirect: auth.OAuthRedirect
    /** URL used as the base URL for requests to our Cloud API backend. */
    apiUrl: ApiUrl
}

// ===================
// === Environment ===
// ===================

/** Possible values for the environment/user we're running for and whose infrastructure we're
 * testing against. */
export type Environment = 'pbuchu' | 'production'

// ===========
// === API ===
// ===========

/** Base URL for requests to our Cloud API backend. */
type ApiUrl = utils.Brand<'ApiUrl'> & string
