/** @file Configuration definition for the Dashboard. */

import * as auth from "./authentication/config";



// =================
// === Constants ===
// =================

/** The current environment that we're running in. */
export const ENVIRONMENT: Environment = "production";

/** All possible URLs used as the OAuth redirects when running the cloud app. */
const CLOUD_REDIRECTS = {
    development: "http://localhost:8081" as auth.OAuthRedirect,
    production: "https://cloud.enso.org" as auth.OAuthRedirect,
}

/** All possible API URLs, sorted by environment. */
const API_URLS = {
    pbuchu: "https://xw0g8j3tsb.execute-api.eu-west-1.amazonaws.com" as ApiUrl,
    production: "https://7aqkn3tnbc.execute-api.eu-west-1.amazonaws.com" as ApiUrl,
}

/** All possible configuration options, sorted by environment. */
const CONFIGS = {
    pbuchu: {
        cloudRedirect: CLOUD_REDIRECTS.development,
        apiUrl: API_URLS.pbuchu,
    } as Config,
    production: {
        cloudRedirect: CLOUD_REDIRECTS.production,
        apiUrl: API_URLS.production,
    } as Config,
}
/** Export the configuration that is currently in use. */
export const ACTIVE_CONFIG: Config = CONFIGS[ENVIRONMENT];



// ==============
// === Config ===
// ==============

/** Interface defining the configuration options that we expect to provide for the Dashboard. */
export interface Config {
    /** URL used as the OAuth redirect when running in the cloud app. */
    cloudRedirect: auth.OAuthRedirect;
    /** URL used as the base URL for requests to our Cloud API backend. */
    apiUrl: ApiUrl;
}



// ===================
// === Environment ===
// ===================

/** Possible values for the environment/user we're running for and whose infrastructure we're testing
 * against. */
export type Environment = "production" | "pbuchu";



// ===========
// === API ===
// ===========

/** Base URL for requests to our Cloud API backend. */
type ApiUrl = string;
