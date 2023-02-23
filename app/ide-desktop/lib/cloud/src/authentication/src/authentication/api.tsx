// FIXME [NP]: find and resolve all typescript/eslint errors (including silenced ones)
import { Auth, CognitoHostedUIIdentityProvider } from "@aws-amplify/auth";
import { CognitoUserSession } from "amazon-cognito-identity-js";
import { CONFIRM_REGISTRATION_PATH, DASHBOARD_PATH, Logger, LOGIN_PATH, RESET_PASSWORD_PATH } from "../components/app";
import { AwsCognitoOAuthOpts } from "@aws-amplify/auth/lib-esm/types";
import { NavigateFunction, redirect } from "react-router-dom";
import { Hub, HubCallback } from "@aws-amplify/core";
import registerAuthEventListener, { ListenFunction } from "./listen";



// =================
// === Constants ===
// =================

/** Name of the string identifying the "hub" that AWS Amplify issues authentication events on. */
const AUTHENTICATION_HUB = "auth";

/**
 * The string used to identify the GitHub federated identity provider in AWS Amplify.
 *
 * This provider alone requires a string because it is not a standard provider, and thus has no
 * constant defined in the AWS Amplify library.
 */
const GITHUB_PROVIDER = "Github";

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



// ==================
// === AuthConfig ===
// ==================


// === AuthConfig ===

/** Configuration for the authentication service. */
export interface AuthConfig {
    /** Logger for the authentication service. */
    logger: Logger;
    /** Whether the application is running on a desktop (i.e., versus in the Cloud). */
    runningOnDesktop: boolean;
    /**
     * Function to navigate to a given (relative) URL.
     *
     * Used to redirect to pages like the password reset page with the query parameters set in the
     * URL (e.g., `?verification_code=...`).
     */
    navigate: (url: string) => void;
}



// ===========
// === API ===
// ===========

/**
 * API for the authentication service.
 */
export interface Api {
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
    /** @see {@link ListenFunction} */
    registerAuthEventListener: ListenFunction;
}

/**
 * Creates an instance of the authentication API.
 * 
 * # Warning
 * 
 * This function should only be called once, and the returned API should be used throughout the
 * application. This is because it performs global configuration of the Amplify library.
 * 
 * @param authConfig - Configuration for the authentication API.
 * @returns An instance of the authentication API.
 */
// FIXME [NP]: make this type safe
const api = (authConfig: AuthConfig): Api => {
    const { logger, runningOnDesktop } = authConfig;

    // Amplify expects `Auth.configure` to be called before any other `Auth` methods are called. By
    // wrapping all the `Auth` methods we care about and returning an API object, we ensure that
    // `Auth.configure` is called before any other `Auth` methods are called.
    const config = amplifyConfig(authConfig)
    registerOpenAuthenticationUrlCallback(authConfig)
    Auth.configure(config)


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

    const signInWithGoogle = () => Auth
        .federatedSignIn({
            provider: CognitoHostedUIIdentityProvider.Google,
            // FIXME [NP]: document this https://github.com/aws-amplify/amplify-js/issues/3391#issuecomment-756473970
            customState: runningOnDesktop ? window.location.pathname : undefined,
        })
        // We don't care about the details in the success case, just that it happened.
        // FIXME [NP]: remove the log
        .then((result) => logger.log("signInWithGoogle", result));

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
        registerAuthEventListener,
    }
}

/**
 * Register the callback that will be invoked when an `enso://` schema URL is opened in the app.
 * 
 * Typically this callback is invoked when the user is redirected back to the app after:
 *
 * 1. authenticating with a federated identity provider; or
 * 2. clicking a "reset password" link in a password reset email.
 *
 * This is only used when running on the desktop, as the browser version of the app lets Amplify
 * handle the redirect for us. On the desktop however, we need to handle the redirect ourselves,
 * because it's a deep link into the app, and Amplify doesn't handle deep links.
 *
 * @param config - Configuration for the authentication library.
 */
// FIXME [NP]: type this?
const registerOpenAuthenticationUrlCallback = (config: AuthConfig) => {
    const { logger, runningOnDesktop, navigate } = config;

    if (!runningOnDesktop) { return }

    const openAuthenticationUrlCallback = (url: string) => {
        const parsedUrl = new URL(url)
        // FIXME [NP]: remove log
        console.log("openAuthenticationUrlCallback::URL::", url)

        // FIXME [NP]: constantize
        if (parsedUrl.pathname === "/confirmation") {
            // Navigate to a relative URL to handle the confirmation link.
            const redirectUrl = `${CONFIRM_REGISTRATION_PATH}${parsedUrl.search}`
            navigate(redirectUrl)
        // If the user is being redirected from a federated identity provider, then we need to pass
        // the URL to the Amplify library, which will parse the URL and complete the OAuth flow.
        // FIXME [NP]: constantize
        // FIXME [NP]: pass the URL as a path, not a host
        } else if (parsedUrl.pathname === "/") {
            // FIXME [NP]: don't use `enso://auth` for both authentication redirect & signout redirect so we don't have to disambiguate here.
            if (parsedUrl.search === "") {
                logger.log("FIXME [NP]: why does the signout navigate not wor?")
                navigate(LOGIN_PATH)
            } else {
                // FIXME [NP]: remove this log
                logger.log("authenticatedRedirectCallback::Current URL::", window.location.href);
                logger.log("authenticatedRedirectCallback::URL::", url);
                // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
                (Auth as any)._handleAuthResponse(url)
            }
        // If the user is being redirected from a password reset email, then we need to navigate to
        // the password reset page, with the verification code and email passed in the URL so they
        // can be filled in automatically.
        // FIXME [NP]: constantize
        // FIXME [NP]: change from password-reset to reset-password
        } else if (parsedUrl.pathname === "/password-reset") {
            // Navigate to a relative URL to handle the password reset.
            const redirectUrl = `${RESET_PASSWORD_PATH}${parsedUrl.search}`
            navigate(redirectUrl)
        }
    }

    // @ts-expect-error
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
    window.loginApi.setOpenAuthenticationUrlCallback(openAuthenticationUrlCallback)
}



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

/**
 * Creates the configuration for the AWS Amplify library.
 *
 * @param config - Configuration for the Authentication API.
 * @returns Configuration for the AWS Amplify library, to be passed into `Auth.configure`.
 */
const amplifyConfig = (authConfig: AuthConfig): AmplifyConfig => {
    const { logger, runningOnDesktop } = authConfig;

    // FIXME [NP]: Don't rely on pre-defined dev environments.
    const config = runningOnDesktop ? AMPLIFY_CONFIG_ELECTRON_PBUCHU : AMPLIFY_CONFIG_BROWSER_PBUCHU;

    // If we're running on the desktop, we want to override the default URL opener for OAuth flows.
    // This is because the default URL opener opens the URL in the desktop app itself, but we want
    // the user to be sent to their system browser instead. The user should be sent to their system
    // browser because:
    //
    // - users trust their system browser with their credentials more than they trust our app;
    // - our app can keep itself on the relevant page until the user is sent back to it (i.e., we
    //   avoid unnecessary reloads/refreshes caused by redirects.
    if (runningOnDesktop) {
        // Ensure that you have the `loginApi` context bridge exposed in the main process. See the
        // `exposeLoginApi` function for more details.
        // FIXME [NP]: is the above doc OK?
        const urlOpener = (url: string) => {
            // FIXME [NP]: remove this log
            logger.log("Opening URL in system browser.", url) 
            // # Safety
            //
            // We're using `window.loginApi` here, which is a context bridge to the main process.
            // We're assuming that the main process has exposed the `loginApi` context bridge, and
            // that it contains the `openExternalUrl` function.
            // @ts-expect-error
            // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
            window.loginApi.openExternalUrl(url)
        }

        config.oauth.options.urlOpener = urlOpener;
    }

    return config;
}



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
  | "NetworkError"
  | "InvalidParameterException";

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

export default api
