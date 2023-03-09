/** @file Provides an {@link AuthService} which consists of an underyling {@link Cognito} API
 * wrapper, along with some convenience callbacks to make URL redirects for the authentication flows
 * work with Electron. */
import * as cognito from "./cognito";
import * as authConfig from "./config";
import * as config from "../config";



// =================
// === Constants ===
// =================

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
    /** Whether the application is running on a desktop (i.e., versus in the Cloud). */
    runningOnDesktop: boolean;
}



// ===================
// === AuthService ===
// ===================

/** API for the authentication service. */
export interface AuthService {
    /** @see {@link Cognito} */
    cognito: cognito.Cognito;
}

/** Creates an instance of the authentication service.
 *
 * # Warning
 *
 * This function should only be called once, and the returned service should be used throughout the
 * application. This is because it performs global configuration of the Amplify library.
 *
 * @param authConfig - Configuration for the authentication service.
 * @returns An instance of the authentication service. */
export const initAuthService = (authConfig: AuthConfig): AuthService => {
    const { runningOnDesktop } = authConfig;

    const amplifyConfig = loadAmplifyConfig(runningOnDesktop);
    const cognitoClient = new cognito.CognitoImpl(
        amplifyConfig
    );

    return {
        cognito: cognitoClient,
    };
};

const loadAmplifyConfig = (
    runningOnDesktop: boolean,
): authConfig.AmplifyConfig => {
    /** Load the environment-specific Amplify configuration. */
    const baseConfig = AMPLIFY_CONFIGS[config.ENVIRONMENT];

    /** Set the redirect URLs for the OAuth flows, depending on our environment. */
    baseConfig.redirectSignIn = runningOnDesktop
        ? authConfig.DESKTOP_REDIRECT
        : config.ACTIVE_CONFIG.cloudRedirect;
    baseConfig.redirectSignOut = runningOnDesktop
        ? authConfig.DESKTOP_REDIRECT
        : config.ACTIVE_CONFIG.cloudRedirect;

    return baseConfig as authConfig.AmplifyConfig;
};
