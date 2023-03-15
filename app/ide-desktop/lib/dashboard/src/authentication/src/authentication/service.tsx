/** @file Provides an {@link AuthService} which consists of an underyling {@link cognito.Cognito}
 * API wrapper, along with some convenience callbacks to make URL redirects for the authentication
 * flows work with Electron. */

import * as common from "enso-common";

import * as authConfigModule from "./config";
import * as cognito from "./cognito";
import * as config from "../config";
import * as platform from "../platform";

// =================
// === Constants ===
// =================

/** URL used as the OAuth redirect when running in the desktop app. */
const DESKTOP_REDIRECT =
  `${common.DEEP_LINK_SCHEME}://auth` as authConfigModule.OAuthRedirect;
/** Map from platform to the OAuth redirect URL that should be used for that platform. */
const PLATFORM_TO_CONFIG: Record<
  platform.Platform,
  Pick<authConfigModule.AmplifyConfig, "redirectSignIn" | "redirectSignOut">
> = {
  [platform.Platform.desktop]: {
    redirectSignIn: DESKTOP_REDIRECT,
    redirectSignOut: DESKTOP_REDIRECT,
  },
  [platform.Platform.cloud]: {
    redirectSignIn: config.ACTIVE_CONFIG.cloudRedirect,
    redirectSignOut: config.ACTIVE_CONFIG.cloudRedirect,
  },
};

const BASE_AMPLIFY_CONFIG: Partial<authConfigModule.AmplifyConfig> = {
  region: authConfigModule.AWS_REGION,
  scope: authConfigModule.OAUTH_SCOPES,
  responseType: authConfigModule.OAUTH_RESPONSE_TYPE,
};

/** Collection of configuration details for Amplify user pools, sorted by deployment environment. */
const AMPLIFY_CONFIGS = {
  /** Configuration for @pbuchu's Cognito user pool. */
  pbuchu: {
    userPoolId: "eu-west-1_jSF1RbgPK",
    userPoolWebClientId: "1bnib0jfon3aqc5g3lkia2infr",
    domain: "pb-enso-domain.auth.eu-west-1.amazoncognito.com",
    ...BASE_AMPLIFY_CONFIG,
  },
  /** Configuration for the production Cognito user pool. */
  production: {
    userPoolId: "eu-west-1_9Kycu2SbD",
    userPoolWebClientId: "4j9bfs8e7415erf82l129v0qhe",
    domain: "production-enso-domain.auth.eu-west-1.amazoncognito.com",
    ...BASE_AMPLIFY_CONFIG,
  },
};

// ==================
// === AuthConfig ===
// ==================

/** Configuration for the authentication service. */
export interface AuthConfig {
  platform: platform.Platform;
}

// ===================
// === AuthService ===
// ===================

/** API for the authentication service. */
export interface AuthService {
  /** @see {@link cognito.Cognito}. */
  cognito: cognito.Cognito;
}

/* eslint-disable jsdoc/require-description-complete-sentence */
/** Creates an instance of the authentication service.
 *
 * # Warning
 *
 * This function should only be called once, and the returned service should be used throughout the
 * application. This is because it performs global configuration of the Amplify library. */
/* eslint-enable jsdoc/require-description-complete-sentence */
export function initAuthService(authConfig: AuthConfig): AuthService {
  const { platform } = authConfig;
  const amplifyConfig = loadAmplifyConfig(platform);
  const cognitoClient = new cognito.Cognito(amplifyConfig);
  return { cognito: cognitoClient };
}

function loadAmplifyConfig(
  platform: platform.Platform
): authConfigModule.AmplifyConfig {
  /** Load the environment-specific Amplify configuration. */
  const baseConfig = AMPLIFY_CONFIGS[config.ENVIRONMENT];
  /** Load the platform-specific Amplify configuration. */
  const platformConfig = PLATFORM_TO_CONFIG[platform];
  return { ...baseConfig, ...platformConfig } as authConfigModule.AmplifyConfig;
}
