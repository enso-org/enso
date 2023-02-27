// =============
// === Types ===
// =============

// FIXME [NP]: document all these types
type AwsRegion = "eu-west-1";
type OAuthScope = "email" | "openid";
type OAuthResponseType = "code";
type OAuthRedirect = "enso://auth" | "http://localhost:8081";
export type OAuthUrlOpener = (url: string, redirectUrl: string) => void;
export type RegisterOpenAuthenticationUrlCallbackFn = () => void;

/**
 * The configuration for the AWS Amplify library.
 *
 * This details user pools, federated identity providers, etc. that are used to authenticate users.
 * The values in this object are not secret, and can be swapped out for testing values to avoid
 * creating authenticated users in the production environment.
 */
// FIXME [NP]: document all these values 
//   See: https://github.com/enso-org/enso/compare/develop...wip/db/cognito-auth-183351503#diff-319a7d209df303b404c07be91bfa179b12aaafcae1c67384dfe3cbffde80e010
export interface AmplifyConfig {
    region: AwsRegion,
    userPoolId: string,
    userPoolWebClientId: string,
    oauth: {
        options: {
            urlOpener?: OAuthUrlOpener,
        },
        domain: string,
        scope: OAuthScope[],
        redirectSignIn: OAuthRedirect,
        redirectSignOut: OAuthRedirect,
        responseType: OAuthResponseType,
    },
}
