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

// FIXME [NP]: move this to a config file
//const electronAmplifyConfigTesting: AuthOptions = {
//  region: "us-east-1",
//  // FIXME [NP]
//  //identityPoolId: "",
//  userPoolId: "us-east-1_VcFZzGyhv",
//  userPoolWebClientId: "7vic1uoogbq4aq2rve897j0ep0",
//  oauth: {
//    options: {}, // FIXME [NP]
//    domain: "test-enso-pool.auth.us-east-1.amazoncognito.com/",
//    scope: ['email', 'openid'], // FIXME [NP]
//    redirectSignIn: "enso://localhost",
//    redirectSignOut: "enso://localhost",
//    responseType: "code",
//  },
//}
//const browserAmplifyConfigNpekin: AuthOptions = {
//  region: "eu-west-1",
//  // FIXME [NP]
//  //identityPoolId: "",
//  userPoolId: "eu-west-1_sP5bQ4mJs",
//  userPoolWebClientId: "27gd0b05qlnkj1lcsnd0b4fb89",
//  oauth: {
//    options: {}, // FIXME [NP]
//    //domain: "https://npekin-enso-domain.auth.eu-west-1.amazoncognito.com",
//    domain: "npekin-enso-domain.auth.eu-west-1.amazoncognito.com/",
//    scope: ['email', 'openid'], // FIXME [NP]
//    redirectSignIn: "http://localhost:8081",
//    redirectSignOut: "http://localhost:8081",
//    responseType: "code",
//  },
//}
const AMPLIFY_CONFIG_ELECTRON_PBUCHU: AmplifyConfig = {
    region: "eu-west-1",
    // FIXME [NP]
    //identityPoolId: "",
    userPoolId: "eu-west-1_jSF1RbgPK",
    userPoolWebClientId: "1bnib0jfon3aqc5g3lkia2infr",
    oauth: {
        options: {}, // FIXME [NP]
        //domain: "https://npekin-enso-domain.auth.eu-west-1.amazoncognito.com",
        domain: "pb-enso-domain.auth.eu-west-1.amazoncognito.com",
        scope: ['email', 'openid'], // FIXME [NP]
        redirectSignIn: "enso://auth",
        redirectSignOut: "enso://auth",
        responseType: "code",
    },
}
const AMPLIFY_CONFIG_BROWSER_PBUCHU: AmplifyConfig = {
    region: "eu-west-1",
    // FIXME [NP]
    //identityPoolId: "",
    userPoolId: "eu-west-1_jSF1RbgPK",
    userPoolWebClientId: "1bnib0jfon3aqc5g3lkia2infr",
    oauth: {
        options: {}, // FIXME [NP]
        //domain: "https://npekin-enso-domain.auth.eu-west-1.amazoncognito.com",
        domain: "pb-enso-domain.auth.eu-west-1.amazoncognito.com",
        scope: ['email', 'openid'], // FIXME [NP]
        redirectSignIn: "http://localhost:8081",
        redirectSignOut: "http://localhost:8081",
        responseType: "code",
    },
}
//const browserAmplifyConfigProd: AuthOptions = {
//  region: "eu-west-1",
//  // FIXME [NP]
//  //identityPoolId: "",
//  userPoolId: "eu-west-1_9Kycu2SbD",
//  userPoolWebClientId: "4j9bfs8e7415erf82l129v0qhe",
//  oauth: {
//    options: {}, // FIXME [NP]
//    //domain: "https://npekin-enso-domain.auth.eu-west-1.amazoncognito.com",
//    domain: "production-enso-domain.auth.eu-west-1.amazoncognito.com/",
//    scope: ['email', 'openid'], // FIXME [NP]
//    redirectSignIn: "https://cloud.enso.org",
//    redirectSignOut: "https://cloud.enso.org",
//    responseType: "code",
//  },
//}


export type LoadAmplifyConfigProps = {
    kind: Cloud,
} | {
    kind: Desktop,
    urlOpener: OAuthUrlOpener,
    registerOpenAuthenticationUrlCallback: RegisterOpenAuthenticationUrlCallbackFn,
};
type Cloud = "cloud";
type Desktop = "desktop";

/**
 * Creates the configuration for the AWS Amplify library.
 *
 * @param config - Configuration for the Authentication API.
 * @returns Configuration for the AWS Amplify library, to be passed into `Auth.configure`.
 */
export const loadAmplifyConfig = (props: LoadAmplifyConfigProps): AmplifyConfig => {
    if (props.kind === "desktop") {
        const { urlOpener, registerOpenAuthenticationUrlCallback } = props;

        // FIXME [NP]: Don't rely on pre-defined dev environments.
        const amplifyConfig = AMPLIFY_CONFIG_ELECTRON_PBUCHU;

        // If we're running on the desktop, we want to override the default URL opener for OAuth
        // flows. This is because the default URL opener opens the URL in the desktop app itself,
        // but we want the user to be sent to their system browser instead. The user should be sent
        // to their system browser because:
        // 
        // - users trust their system browser with their credentials more than they trust our app;
        // - our app can keep itself on the relevant page until the user is sent back to it (i.e., we
        //   avoid unnecessary reloads/refreshes caused by redirects.
        amplifyConfig.oauth.options.urlOpener = urlOpener;

        // To handle redirects back to the application from the system browser, we also need to
        // register a custom URL handler.
        registerOpenAuthenticationUrlCallback();

        return amplifyConfig;
    } else {
        // FIXME [NP]: Don't rely on pre-defined dev environments.
        const amplifyConfig = AMPLIFY_CONFIG_BROWSER_PBUCHU;

        return amplifyConfig;
    }
}
