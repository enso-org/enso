/** @file Provides an {@link AuthService} which consists of an underyling {@link cognito.Cognito}
 * API wrapper, along with some convenience callbacks to make URL redirects for the authentication
 * flows work with Electron. */

import * as common from "enso-studio-common";

import * as cognito from "./cognito";
import * as authConfig from "./config";
import * as config from "../config";
import * as app from "../components/app";

// =================
// === Constants ===
// =================

/** URL used as the OAuth redirect when running in the desktop app. */
const DESKTOP_REDIRECT =
  `${common.DEEP_LINK_SCHEME}://auth` as authConfig.OAuthRedirect;
/** Map from platform to the OAuth redirect URL that should be used for that platform. */
const PLATFORM_TO_CONFIG: Record<
  app.Platform,
  Pick<authConfig.AmplifyConfig, "redirectSignIn" | "redirectSignOut">
> = {
  [app.Platform.desktop]: {
    redirectSignIn: DESKTOP_REDIRECT,
    redirectSignOut: DESKTOP_REDIRECT,
  },
  [app.Platform.cloud]: {
    redirectSignIn: config.ACTIVE_CONFIG.cloudRedirect,
    redirectSignOut: config.ACTIVE_CONFIG.cloudRedirect,
  },
};

const BASE_AMPLIFY_CONFIG: Partial<authConfig.AmplifyConfig> = {
  region: authConfig.AWS_REGION,
  scope: authConfig.OAUTH_SCOPES,
  responseType: authConfig.OAUTH_RESPONSE_TYPE,
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
  platform: app.Platform;
}

// ===================
// === AuthService ===
// ===================

/** API for the authentication service. */
export interface AuthService {
  /** @see {@link cognito.Cognito} */
  cognito: cognito.Cognito;
}

/** Creates an instance of the authentication service.
 *
 * # Warning
 *
 * This function should only be called once, and the returned service should be used throughout the
 * application. This is because it performs global configuration of the Amplify library. */
export const initAuthService = (authConfig: AuthConfig): AuthService => {
  const { platform } = authConfig;
  const amplifyConfig = loadAmplifyConfig(platform);
  const cognitoClient = new cognito.Cognito(amplifyConfig);
  return { cognito: cognitoClient };
};

const loadAmplifyConfig = (
  platform: app.Platform
): authConfig.AmplifyConfig => {
  /** Load the environment-specific Amplify configuration. */
  const baseConfig = AMPLIFY_CONFIGS[config.ENVIRONMENT];
  /** Load the platform-specific Amplify configuration. */
  const platformConfig = PLATFORM_TO_CONFIG[platform];
  return { ...baseConfig, ...platformConfig } as authConfig.AmplifyConfig;
};
