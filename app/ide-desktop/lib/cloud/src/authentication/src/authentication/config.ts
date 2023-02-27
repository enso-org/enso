// =================
// === Constants ===
// =================

/** AWS region in which our Cognito pool is located. */
export const AWS_REGION: AwsRegion = "eu-west-1";
/** URL used as the OAuth redirect when running in the desktop app. */
export const DESKTOP_REDIRECT: OAuthRedirect = "enso://auth";
/** URL used as the OAuth redirect when running in the cloud app. */
export const CLOUD_REDIRECT: OAuthRedirect = "http://localhost:8081";
/** Complete list of OAuth scopes used by the app. */
export const OAUTH_SCOPES: OAuthScope[] = ["email", "openid"];
/** OAuth response type used in the OAuth flows. */
export const OAUTH_RESPONSE_TYPE: OAuthResponseType = "code";



// =============
// === Types ===
// =============

// FIXME [NP]: document all these types
/** The AWS region in which our Cognito pool is located. This is always set to `eu-west-1` because
 * that is the only region in which our Cognito pools are currently available in. */
type AwsRegion = "eu-west-1";
/** ID of the "Cognito user pool" that contains authentication & identity data of our users.
 * 
 * This is created automatically by our Terraform scripts when the backend infrastructure is
 * created. Look in the `enso-org/cloud-v2` repo for details. */
type UserPoolId = string;
/** ID of an OAuth client authorized to interact with the Cognito user pool specified by the
 * {@link UserPoolId}.
 * 
 * This is created automatically by our Terraform scripts when the backend infrastructure is
 * created. Look in the `enso-org/cloud-v2` repo for details. */
type UserPoolWebClientId = string;
/** Domain of the Cognito user pool used for authenticating/identifying the user.
 * 
 * This must correspond to the public-facing domain name of the Cognito pool identified by the
 * {@link UserPoolId}, and must not contain an HTTP scheme, or a pathname. */
type OAuthDomain = string;
/** Possible OAuth scopes to request from the federated identity provider during OAuth sign-in. */
type OAuthScope = "email" | "openid";
/** The response type used to complete the OAuth flow. "code" means that the federated identity
 * provider will return an authorization code that can be exchanged for an access token. The
 * authorization code will be provided as a query parameter of the redirect URL. */
type OAuthResponseType = "code";
/** The URL used as a redirect (minus query parameters like `code` which get appended later), once
 * an OAuth flow (e.g., sign-in or sign-out) has completed. These must match the values set in the
 * Cognito pool and during the creation of the OAuth client. See the `enso-org/cloud-v2` repo for
 * details. */
type OAuthRedirect = "enso://auth" | "http://localhost:8081";
/** Callback used to open URLs for the OAuth flow. This is only used in the desktop app (i.e., not in
 * the cloud). This is because in the cloud we just keep the user in their browser, but in the app
 * we want to open OAuth URLs in the system browser. This is because the user can't be expected to
 * trust their credentials to an Electron app. */
export type OAuthUrlOpener = (url: string, redirectUrl: string) => void;
/** Function used to register a callback. The callback will get called when a deep link is received
 * by the app. This is only used in the desktop app (i.e., not in the cloud). This is used when the
 * user is redirected back to the app from the system browser, after completing an OAuth flow. */
export type RegisterOpenAuthenticationUrlCallbackFn = () => void;



// =====================
// === AmplifyConfig ===
// =====================

/** Configuration for the AWS Amplify library.
 *
 * This details user pools, federated identity providers, etc. that are used to authenticate users.
 * The values in this object are not secret, and can be swapped out for testing values to avoid
 * creating authenticated users in the production environment. */
export interface AmplifyConfig {
    region: AwsRegion,
    userPoolId: UserPoolId,
    userPoolWebClientId: UserPoolWebClientId,
    urlOpener?: OAuthUrlOpener,
    domain: OAuthDomain,
    scope: OAuthScope[],
    redirectSignIn: OAuthRedirect,
    redirectSignOut: OAuthRedirect,
    responseType: OAuthResponseType,
}



// ===========================
// === NestedAmplifyConfig ===
// ===========================

/** Same as {@link AmplifyConfig}, but in a format recognized by the AWS Amplify library. */
export interface NestedAmplifyConfig {
    region: AwsRegion,
    userPoolId: UserPoolId,
    userPoolWebClientId: UserPoolWebClientId,
    oauth: {
        options: {
            urlOpener?: OAuthUrlOpener,
        },
        domain: OAuthDomain,
        scope: OAuthScope[],
        redirectSignIn: OAuthRedirect,
        redirectSignOut: OAuthRedirect,
        responseType: OAuthResponseType,
    }
}

/** Converts the flattened `AmplifyConfig` struct to a form recognizable to the AWS Amplify library.
 * 
 * We use a flattened form of the config for easier object manipulation, but the AWS Amplify library
 * expects a nested form. */
export const toNestedAmplifyConfig = (config: AmplifyConfig): NestedAmplifyConfig => ({
    region: config.region,
    userPoolId: config.userPoolId,
    userPoolWebClientId: config.userPoolWebClientId,
    oauth: {
        options: {
            urlOpener: config.urlOpener,
        },
        domain: config.domain,
        scope: config.scope,
        redirectSignIn: config.redirectSignIn,
        redirectSignOut: config.redirectSignOut,
        responseType: config.responseType,
    },
})
