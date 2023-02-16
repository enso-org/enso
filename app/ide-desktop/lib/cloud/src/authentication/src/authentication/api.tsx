import { Auth, CognitoHostedUIIdentityProvider } from "@aws-amplify/auth";
import { CognitoUserSession } from "amazon-cognito-identity-js";



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


// === Config ===

interface Config {
    /// Whether the application is running on a desktop (i.e., versus in the Cloud).
    runningOnDesktop: boolean;
}


// === API ===

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
     */
    signInWithGoogle: () => Promise<void>;
    /**
     * Signs in via the GitHub federated identity provider.
     */
    signInWithGithub: () => Promise<void>;
    /**
     * Signs in with the given username and password.
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
     * @param email - Email address to send the password reset email to.
     * @returns A promise that resolves if the email was sent successfully.
     * @throws An error if the email fails to send.
     */
    forgotPassword: (email: string) => Promise<void>;
    /**
     * Submits a new password for the given email address.
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

const api = (config: Config): Api => {
    const { runningOnDesktop } = config;

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
