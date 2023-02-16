import { Auth, CognitoHostedUIIdentityProvider } from "@aws-amplify/auth";
import { CognitoUserSession } from "amazon-cognito-identity-js";
import { Logger } from "../components/app";



// =================
// === Constants ===
// =================

/**
 * The string used to identify the GitHub federated identity provider in AWS Amplify.
 *
 * This provider alone requires a string because it is not a standard provider, and thus has no
 * constant defined in the AWS Amplify library.
 */
const GITHUB_PROVIDER = "Github";



// =============================
// === Amplify Configuration ===
// =============================

// FIXME [NP]: document all these types
type AwsRegion = "eu-west-1";
type OAuthScope = "email" | "openid";
type OAuthResponseType = "code";
export type OAuthUrlOpener = (url: string, redirectUrl: string) => void;

/**
 * The configuration for the AWS Amplify library.
 *
 * This details user pools, federated identity providers, etc. that are used to authenticate users.
 * The values in this object are not secret, and can be swapped out for testing values to avoid
 * creating authenticated users in the production environment.
 */
// FIXME [NP]: document all these values 
//   See: https://github.com/enso-org/enso/compare/develop...wip/db/cognito-auth-183351503#diff-319a7d209df303b404c07be91bfa179b12aaafcae1c67384dfe3cbffde80e010
interface AmplifyConfig {
    region: AwsRegion,
    userPoolId: string,
    userPoolWebClientId: string,
    oauth: {
        options: {
            urlOpener?: OAuthUrlOpener,
        },
        domain: string,
        scope: OAuthScope[],
        redirectSignIn: string,
        redirectSignOut: string,
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

const AMPLIFY_CONFIG = AMPLIFY_CONFIG_BROWSER_PBUCHU;



// ====================
// === AssertString ===
// ====================

/**
 * Type signature for a function that asserts that a parameter is a string.
 */
type AssertString = (param: any, message: string) => asserts param is string

/**
 * Asserts that a parameter is a string.
 * 
 * Used both to assert that a parameter is a string at runtime, and to inform TypeScript that a
 * parameter is a string.
 * 
 * @param param - The parameter to assert.
 * @param message - The error message to throw if the assertion fails.
 * @throws An error if the assertion fails.
 */
const assertString: AssertString = (param, message) => {
    if (typeof param !== "string") {
      throw new Error(message);
    }
}



// ====================
// === AmplifyError ===
// ====================

/**
 * List of known error codes returned by the AWS Amplify library for Amplify errors.
 */
type AmplifyErrorCode =
  | "UserNotFoundException"
  | "UserNotConfirmedException"
  | "NotAuthorizedException" 
  | "InvalidPasswordException"
  | "UsernameExistsException" 
  | "NetworkError";

/**
 * The type of the object returned by the AWS Amplify library when an Amplify error occurs.
 */
interface AmplifyError {
    /**
     * Error code for disambiguating the error.
     */
    code: AmplifyErrorCode,
    name: string,
    /**
     * Human-readable error message.
     */
    message: string,
}

/**
 * Hints to TypeScript if we can safely cast an error to an `AmplifyError`.
 */
export const isAmplifyError = (error: unknown): error is AmplifyError => {
    if (error && typeof error === "object") {
      return "code" in error && "message" in error && "name" in error;
    }
    return false;
}



// =================
// === AuthError ===
// =================

/**
 * The type of the object returned by the AWS Amplify library when an auth error occurs.
 */
interface AuthError {
    name: string,
    log: string,
}

/**
 * Hints to TypeScript if we can safely cast an error to an `AuthError`.
 */
export const isAuthError = (error: unknown): error is AuthError => {
    if (error && typeof error === "object") {
        return "name" in error && "log" in error;
    }
    return false;
}



// ===================
// === UserSession ===
// ===================

/// User's session, provides information for identifying and authenticating the user.
interface UserSession {
    /// User's email address, used to uniquely identify the user.
    ///
    /// Provided by the identity provider the user used to log in. One of:
    ///
    /// - GitHub
    /// - Google
    /// - Email
    email: string;
    /// User's access token, used to authenticate the user (e.g., when making API calls).
    accessToken: string;
}

/**
 * Returns the current `CognitoUserSession`, or `null` if the user is not logged in.
 *
 * Will refresh the session if it has expired.
 *
 * @returns `CognitoUserSession` if the user is logged in, `null` otherwise.
 */
const getAmplifyCurrentSession = async () => {
    try {
        return await Auth.currentSession();
    } catch (error) {
        // If the user is not logged in, `Auth.currentSession()` throws an error. We catch this
        // error and return `null` instead.
        if (error === "No current user") {
            return null;
        }

        throw error;
    }
}

/**
 * Parses a `CognitoUserSession` into a `UserSession`.
 */
const parseUserSession = (session: CognitoUserSession): UserSession => {
    const payload = session.getIdToken().payload;
    // The `email` field is mandatory, so we assert that it exists and is a string.
    assertString(payload.email, "Payload does not have an email field.")
    const email = payload.email;
    const accessToken = session.getAccessToken().getJwtToken();

    return { email, accessToken };
}



// ===========
// === API ===
// ===========


// === AuthConfig ===

/**
 * Configuration for the authentication service.
 */
export type AuthConfig = DesktopConfig | BrowserConfig;

/**
 * Configuration for the authentication service, for when the application is running on the desktop.
 */
interface DesktopConfig {
    /**
     * Whether the application is running on a desktop (i.e., versus in the Cloud).
     */
    runningOnDesktop: true;
    /**
     * URL opener for opening a URL in the user's system browser, for OAuth flows.
     */
    urlOpener: OAuthUrlOpener,
}

/**
 * Configuration for the authentication service, for when the application is running in the Cloud.
 */
interface BrowserConfig {
    /**
     * Whether the application is running on a desktop (i.e., versus in the Cloud).
     */
    runningOnDesktop: false;
}

/**
 * Creates a configuration for the authentication service.
 */
export const config = (logger: Logger, runningOnDesktop: boolean) => {
    logger.log("Creating authentication service configuration.") 
    if (runningOnDesktop) {
        // # Running on Desktop
        //
        // Ensure that you have the `loginApi` context bridge exposed in the main process. See the
        // `exposeLoginApi` function for more details.
        const urlOpener = (url: string) => {
            // FIXME [NP]: remove this log
            logger.log("Opening URL in system browser.", url) 
            // # Safety
            //
            // We're using `window.loginApi` here, which is a context bridge to the main process.
            // We're assuming that the main process has exposed the `loginApi` context bridge, and
            // that it contains the `open` function.
            // @ts-expect-error
            // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
            window.loginApi.open(url)
        }

        return { runningOnDesktop, urlOpener }
    } else {
        return { runningOnDesktop }
    }
}


// === API ===

/**
 * API for the authentication service.
 */
interface Api {
    /**
     * Returns the current user's session, or `null` if the user is not logged in.
     *
     * Will refresh the session if it has expired.
     *
     * @returns `UserSession` if the user is logged in, `null` otherwise.
     * @throws An error if the session cannot be retrieved.
     */
    userSession: () => Promise<UserSession | null>;
    /**
     * Sign up with the given parameters (i.e., username and password).
     * 
     * Does not rely on external identity providers (e.g., Google or GitHub).
     *
     * @returns A promise that resolves if the sign up was successful.
     * @throws An error if sign up fails.
     */
    signUp: (username: string, password: string) => Promise<void>;
    /**
     * Sends the verification code to confirm the user's email address.
     *
     * @param email - User's email address.
     * @param code - Verification code that was sent to the user's email address.
     * @returns A promise that resolves if the verification code was sent successfully.
     * @throws An error if the verification fails.
     */
    confirmSignUp: (email: string, code: string) => Promise<void>;
    /**
     * Signs in via the Google federated identity provider.
     * 
     * This function will open the Google authentication page in the user's browser. The user will
     * be asked to log in to their Google account, and then to grant access to the application.
     * After the user has granted access, the browser will be redirected to the application.
     */
    signInWithGoogle: () => Promise<void>;
    /**
     * Signs in via the GitHub federated identity provider.
     * 
     * This function will open the GitHub authentication page in the user's browser. The user will
     * be asked to log in to their GitHub account, and then to grant access to the application.
     * After the user has granted access, the browser will be redirected to the application.
     */
    signInWithGithub: () => Promise<void>;
    /**
     * Signs in with the given username and password.
     * 
     * Does not rely on external identity providers (e.g., Google or GitHub).
     * 
     * @param username - Username of the user to sign in.
     * @param password - Password of the user to sign in.
     * @returns A promise that resolves if the sign in was successful.
     * @throws An error if sign in fails.
     */
    signInWithPassword: (username: string, password: string) => Promise<void>;
    /**
     * Sends a password reset email to the given email address.
     * 
     * The user will be able to reset their password by following the link in the email, which takes
     * them to the "reset password" page of the application. The verification code will be filled in
     * automatically.
     *
     * @param email - Email address to send the password reset email to.
     * @returns A promise that resolves if the email was sent successfully.
     * @throws An error if the email fails to send.
     */
    forgotPassword: (email: string) => Promise<void>;
    /**
     * Submits a new password for the given email address.
     * 
     * The user will have received a verification code in an email, which they will have entered on
     * the "reset password" page of the application. This function will submit the new password
     * along with the verification code, changing the user's password.
     *
     * @param email - Email address to reset the password for.
     * @param code - Verification code that was sent to the user's email address.
     * @param password - New password to set.
     * @returns A promise that resolves if the password was reset successfully.
     * @throws An error if the password fails to reset.
     */
    forgotPasswordSubmit: (email: string, code: string, password: string) => Promise<void>;
    /**
     * Signs out the current user.
     * 
     * @returns A promise that resolves if the sign out was successful.
     * @throws An error if sign out fails.
     */
    signOut: () => Promise<void>;
}

/**
 * Creates an instance of the authentication API.
 * 
 * # Warning
 * 
 * This function should only be called once, and the returned API should be used throughout the
 * application. This is because it performs global configuration of the Amplify library.
 * 
 * @param config - Configuration for the API.
 * @returns An instance of the authentication API.
 */
const api = (config: AuthConfig): Api => {
    const { runningOnDesktop } = config;

    // If we're running on the desktop, we want to override the default URL opener for OAuth flows.
    // This is because the default URL opener opens the URL in the desktop app itself, but we want
    // the user to be sent to their system browser instead. The user should be sent to their system
    // browser because:
    //
    // - users trust their system browser with their credentials more than they trust our app;
    // - our app can keep itself on the relevant page until the user is sent back to it (i.e., we
    //   avoid unnecessary reloads/refreshes caused by redirects.
    if (runningOnDesktop) {
        AMPLIFY_CONFIG.oauth.options.urlOpener = config.urlOpener;
    }

    // Amplify expects `Auth.configure` to be called before any other `Auth` methods are called. By
    // wrapping all the `Auth` methods we care about and returning an API object, we ensure that
    // `Auth.configure` is called before any other `Auth` methods are called.
    Auth.configure(AMPLIFY_CONFIG)

    const userSession = () => getAmplifyCurrentSession()
        .then((session) => session ? parseUserSession(session) : null);

    const signUp = async (username: string, password: string) => {
        const params = {
            username,
            password,
            attributes: {
                email: username,
                // Add a custom attribute indicating whether the user is signing up from the
                // desktop. This is used to determine the schema used in the callback links sent in
                // the verification emails. For example, `http://` for the Cloud, and `enso://` for
                // the desktop.
                // eslint-disable-next-line @typescript-eslint/naming-convention
                "custom:fromDesktop": runningOnDesktop ? "true" : "false",
            }
        }

        return await Auth
            .signUp(params)
            // We don't care about the details in the success case, just that it happened.
            .then(() => {})
    }

    const confirmSignUp = (email: string, code: string) => Auth.confirmSignUp(email, code)

    const signInWithGoogle = () => Auth.federatedSignIn({ provider: CognitoHostedUIIdentityProvider.Google })
        // We don't care about the details in the success case, just that it happened.
        .then(() => {});

    const signInWithGithub = () => Auth.federatedSignIn({ customProvider: GITHUB_PROVIDER })
        // We don't care about the details in the success case, just that it happened.
        .then(() => {});

    const signInWithPassword = (username: string, password: string) => Auth.signIn(username, password)

    const forgotPassword = async (email: string) => Auth.forgotPassword(email)

    const forgotPasswordSubmit = async (email: string, code: string, password: string) => Auth.forgotPasswordSubmit(email, code, password)
        // We don't care about the details in the success case, just that it happened.
        .then(() => {})

    const signOut = () => Auth.signOut()

    return {
        userSession,
        signUp,
        confirmSignUp,
        signInWithGoogle,
        signInWithGithub,
        signInWithPassword,
        forgotPassword,
        forgotPasswordSubmit,
        signOut,
    }
}

export default api
