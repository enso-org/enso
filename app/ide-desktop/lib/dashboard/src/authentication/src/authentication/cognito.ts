import { Auth, CognitoHostedUIIdentityProvider, SignUpParams } from "@aws-amplify/auth";
import { CognitoUserSession } from "amazon-cognito-identity-js";
import { Result, Option } from "ts-results";
import { Logger } from "../providers/logger";
import { AmplifyConfig, toNestedAmplifyConfig } from "./config";


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

const SIGN_IN_WITH_PASSWORD_USER_NOT_FOUND_MESSAGE = "User not found. Please register first.";
const SIGN_IN_WITH_PASSWORD_USER_NOT_CONFIRMED_MESSAGE = "User is not confirmed. Please check your email for a confirmation link.";
const SIGN_IN_WITH_PASSWORD_INCORRECT_USERNAME_OR_PASSWORD_MESSAGE = "Incorrect username or password.";
const FORGOT_PASSWORD_USER_NOT_FOUND_MESSAGE = "User not found. Please register first.";
const FORGOT_PASSWORD_USER_NOT_CONFIRMED_MESSAGE = "Cannot reset password for user with unverified email. Please verify your email first.";

/** A list of known Amplify errors that we can match against prior to trying to convert to our
 * internal error types. This is useful for disambiguating between Amplify errors with the same code
 * but different messages. */
const KNOWN_ERRORS = {
    userAlreadyConfirmed: {
        code: "NotAuthorizedException",
        message: "User cannot be confirmed. Current status is CONFIRMED",
    },
    forgotPasswordUserNotConfirmed: {
        code: "InvalidParameterException",
        message: "Cannot reset password for the user as there is no registered/verified email or phone_number",
    }
}



// ====================
// === AmplifyError ===
// ====================

/** Object returned by the AWS Amplify library when an Amplify error occurs. */
interface AmplifyError extends Error {
    /** Error code for disambiguating the error. */
    code: string,
}

/** Hints to TypeScript if we can safely cast an `unknown` error to an `AmplifyError`. */
const isAmplifyError = (error: unknown): error is AmplifyError => {
    if (error && typeof error === "object") {
      return "code" in error && "message" in error && "name" in error;
    }
    return false;
}

const intoAmplifyErrorOrThrow = (error: unknown): AmplifyError => {
    if (isAmplifyError(error)) {
        return error;
    } else {
        throw error;
    }
}



// =================
// === AuthError ===
// =================

/** Object returned by the AWS Amplify library when an auth error occurs. */
interface AuthError {
    name: string,
    log: string,
}

/** Hints to TypeScript if we can safely cast an `unknown` error to an `AuthError`. */
const isAuthError = (error: unknown): error is AuthError => {
    if (error && typeof error === "object") {
        return "name" in error && "log" in error;
    }
    return false;
}



// ===============
// === Cognito ===
// ===============

export interface Cognito {
    /**
     * Returns the current user's session.
     *
     * Will refresh the session if it has expired.
     *
     * @returns `UserSession` if the user is logged in, `None` otherwise.
     * @throws An error if failed due to an unknown error.
     */
    userSession: () => Promise<Option<UserSession>>;
    /**
     * Sign up with the given parameters (i.e., username and password).
     * 
     * Does not rely on external identity providers (e.g., Google or GitHub).
     *
     * @returns A promise that resolves to either success or known error.
     * @throws An error if failed due to an unknown error.
     */
    signUp: (username: string, password: string) => Promise<Result<null, SignUpError>>;
    /**
     * Sends the verification code to confirm the user's email address.
     *
     * @param email - User's email address.
     * @param code - Verification code that was sent to the user's email address.
     * @returns A promise that resolves to either success or known error.
     * @throws An error if failed due to an unknown error.
     */
    confirmSignUp: (email: string, code: string) => Promise<Result<null, ConfirmSignUpError>>;
    /**
     * Signs in via the Google federated identity provider.
     * 
     * This function will open the Google authentication page in the user's browser. The user will
     * be asked to log in to their Google account, and then to grant access to the application.
     * After the user has granted access, the browser will be redirected to the application.
     */
    signInWithGoogle: () => Promise<null>;
    /**
     * Signs in via the GitHub federated identity provider.
     * 
     * This function will open the GitHub authentication page in the user's browser. The user will
     * be asked to log in to their GitHub account, and then to grant access to the application.
     * After the user has granted access, the browser will be redirected to the application.
     */
    signInWithGitHub: () => Promise<null>;
    /**
     * Signs in with the given username and password.
     * 
     * Does not rely on external identity providers (e.g., Google or GitHub).
     * 
     * @param username - Username of the user to sign in.
     * @param password - Password of the user to sign in.
     * @returns A promise that resolves to either success or known error.
     * @throws An error if failed due to an unknown error.
     */
    signInWithPassword: (username: string, password: string) => Promise<Result<null, SignInWithPasswordError>>;
    /**
     * Sends a password reset email to the given email address.
     * 
     * The user will be able to reset their password by following the link in the email, which takes
     * them to the "reset password" page of the application. The verification code will be filled in
     * automatically.
     *
     * @param email - Email address to send the password reset email to.
     * @returns A promise that resolves to either success or known error.
     * @throws An error if failed due to an unknown error.
     */
    forgotPassword: (username: string) => Promise<Result<null, ForgotPasswordError>>;
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
     * @returns A promise that resolves to either success or known error.
     * @throws An error if failed due to an unknown error.
     */
    forgotPasswordSubmit: (username: string, code: string, newPassword: string) => Promise<Result<null, ForgotPasswordSubmitError>>;
    /**
     * Signs out the current user.
     * 
     * @returns A promise that resolves if successful.
     */
    signOut: () => Promise<void>;
}



// ===================
// === CognitoImpl ===
// ===================

export class CognitoImpl implements Cognito {
    private readonly logger: Logger;
    private readonly fromDesktop: boolean;

    constructor(
        logger: Logger,
        fromDesktop: boolean,
        amplifyConfig: AmplifyConfig,
    ) {
        this.logger = logger;
        this.fromDesktop = fromDesktop;

        // Amplify expects `Auth.configure` to be called before any other `Auth` methods are called.
        // By wrapping all the `Auth` methods we care about and returning an `Cognito` API object
        // containing them, we ensure that `Auth.configure` is called before any other `Auth`
        // methods are called.
        const config = toNestedAmplifyConfig(amplifyConfig);
        Auth.configure(config)
    }


    // === Interface `impl`s ===

    /**
     * We want to signal to Amplify to fire a "custom state change" event when the user is
     * redirected back to the application after signing in via an external identity provider. This
     * is done so we get a chance to fix the location history that Amplify messes up when it
     * redirects the user to the identity provider's authentication page.
     * 
     * In order to do so, we need to pass custom state along for the entire OAuth flow, which is
     * obtained by calling this function. This function will return the current location path if
     * the user is signing in from the desktop application, and `undefined` otherwise.
     * 
     * We use `undefined` outside of the desktop application because Amplify only messes up the
     * location history in the desktop application.
     * 
     * See: https://github.com/aws-amplify/amplify-js/issues/3391#issuecomment-756473970
     */
    customState = () => this.fromDesktop ? window.location.pathname : undefined;
    userSession = userSession;
    signUp = (username: string, password: string) => signUp(username, password, this.fromDesktop)
    confirmSignUp = confirmSignUp
    signInWithGoogle = () => signInWithGoogle(this.customState())
    signInWithGitHub = signInWithGitHub;
    signInWithPassword = signInWithPassword
    forgotPassword = forgotPassword
    forgotPasswordSubmit = forgotPasswordSubmit
    signOut = () => signOut(this.logger)
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



// ===================
// === UserSession ===
// ===================

/// User's session, provides information for identifying and authenticating the user.
export interface UserSession {
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

const userSession = () => getAmplifyCurrentSession()
    .then(result => result.map(parseUserSession))
    .then(result => result.toOption());

type CurrentSessionErrorKind =
    | "NoCurrentUser";

const intoCurrentSessionErrorKind = (error: unknown): CurrentSessionErrorKind => {
    if (error === "No current user") {
        return "NoCurrentUser";
    } else {
        throw error;
    }
}

/**
 * Returns the current `CognitoUserSession`.
 *
 * Will refresh the session if it has expired.
 *
 * @returns `CognitoUserSession` if the user is logged in, `CurrentSessionErrorKind`
 * otherwise.
 */
const getAmplifyCurrentSession = () => Result
    .wrapAsync(() => Auth.currentSession())
    .then(result => result.mapErr(intoCurrentSessionErrorKind));

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



// ==============
// === SignUp ===
// ==============

const signUp = (username: string, password: string, fromDesktop: boolean) => Result
    .wrapAsync(() => {
        const params = intoSignUpParams(username, password, fromDesktop);
        return Auth.signUp(params)
    })
    // We don't care about the details in the success case, just that it happened.
    .then(result => result.map(() => null))
    .then(result => result.mapErr(intoAmplifyErrorOrThrow))
    .then(result => result.mapErr(intoSignUpErrorOrThrow))

const intoSignUpParams = (username: string, password: string, fromDesktop: boolean): SignUpParams => ({
    username,
    password,
    attributes: {
        email: username,
        // Add a custom attribute indicating whether the user is signing up from the
        // desktop. This is used to determine the schema used in the callback links sent in
        // the verification emails. For example, `http://` for the Cloud, and `enso://` for
        // the desktop.
        //
        // # Safety
        //
        // It is necessary to disable the naming convention rule here, because the key is expected
        // to appear exactly as-is in Cognito, so we must match it.
        // eslint-disable-next-line @typescript-eslint/naming-convention
        "custom:fromDesktop": fromDesktop ? "true": "false",
    }
})

type SignUpErrorKind =
    | "UsernameExists"
    | "InvalidParameter";

export interface SignUpError {
    kind: SignUpErrorKind,
    message: string;
}

const intoSignUpErrorOrThrow = (error: AmplifyError): SignUpError => {
    if (error.code === "UsernameExistsException") {
        return {
            kind: "UsernameExists",
            message: error.message,
        };
    } else if (error.code === "InvalidParameterException") {
        return {
            kind: "InvalidParameter",
            message: error.message,
        };
    }

    throw error;
}



// =====================
// === ConfirmSignUp ===
// =====================

const confirmSignUp = async (email: string, code: string) => Result
    .wrapAsync(() => Auth.confirmSignUp(email, code))
    // We don't care about the details in the success case, just that it happened.
    .then(result => result.map(() => null))
    .then(result => result.mapErr(intoAmplifyErrorOrThrow))
    .then(result => result.mapErr(intoConfirmSignUpErrorOrThrow))

type ConfirmSignUpErrorKind =
    | "UserAlreadyConfirmed";

export interface ConfirmSignUpError {
    kind: ConfirmSignUpErrorKind,
    message: string;
}

const intoConfirmSignUpErrorOrThrow = (error: AmplifyError): ConfirmSignUpError => {
    if (error.code === KNOWN_ERRORS.userAlreadyConfirmed.code) {
        if (error.message === KNOWN_ERRORS.userAlreadyConfirmed.message) {
            return {
                // We don't re-use the `error.code` here because Amplify overloads the same kind
                // for multiple kinds of errors that we want to disambiguate.
                kind: "UserAlreadyConfirmed",
                message: error.message,
            };
        }
    }

    throw error;
}



// ========================
// === SignInWithGoogle ===
// ========================

const signInWithGoogle = async (customState?: string) => Auth
    .federatedSignIn({
        provider: CognitoHostedUIIdentityProvider.Google,
        customState,
    })
    // We don't care about the details in the success case, just that it happened.
    .then(() => null)



// ========================
// === SignInWithGoogle ===
// ========================

const signInWithGitHub = async () => Auth
    .federatedSignIn({
        customProvider: GITHUB_PROVIDER,
    })
    // We don't care about the details in the success case, just that it happened.
    .then(() => null)



// ==========================
// === SignInWithPassword ===
// ==========================

const signInWithPassword = async (username: string, password: string) => Result
    .wrapAsync(() => Auth.signIn(username, password))
    // We don't care about the details in the success case, just that it happened.
    .then(result => result.map(() => null))
    .then(result => result.mapErr(intoAmplifyErrorOrThrow))
    .then(result => result.mapErr(intoSignInWithPasswordErrorOrThrow))

type SignInWithPasswordErrorKind =
    | "UserNotFound"
    | "UserNotConfirmed"
    | "NotAuthorized";

export interface SignInWithPasswordError {
    kind: SignInWithPasswordErrorKind,
    message: string;
}

const intoSignInWithPasswordErrorOrThrow = (error: AmplifyError): SignInWithPasswordError => {
    switch (error.code) {
        case "UserNotFoundException":
            return {
                kind: "UserNotFound",
                message: SIGN_IN_WITH_PASSWORD_USER_NOT_FOUND_MESSAGE,
            };
        case "UserNotConfirmedException":
            return {
                kind: "UserNotConfirmed",
                message: SIGN_IN_WITH_PASSWORD_USER_NOT_CONFIRMED_MESSAGE,
            };
        case "NotAuthorizedException":
            return {
                kind: "NotAuthorized",
                message: SIGN_IN_WITH_PASSWORD_INCORRECT_USERNAME_OR_PASSWORD_MESSAGE,
            };
    }

    throw error
}



// ======================
// === ForgotPassword ===
// ======================

const forgotPassword = async (email: string) => Result
    .wrapAsync(() => Auth.forgotPassword(email))
    // We don't care about the details in the success case, just that it happened.
    .then(result => result.map(() => null))
    .then(result => result.mapErr(intoAmplifyErrorOrThrow))
    .then(result => result.mapErr(intoForgotPasswordErrorOrThrow))

type ForgotPasswordErrorKind =
    | "UserNotFound"
    | "UserNotConfirmed";

export interface ForgotPasswordError {
    kind: ForgotPasswordErrorKind,
    message: string;
}

const intoForgotPasswordErrorOrThrow = (error: AmplifyError): ForgotPasswordError => {
    if (error.code === "UserNotFoundException") {
        return {
            kind: "UserNotFound",
            message: FORGOT_PASSWORD_USER_NOT_FOUND_MESSAGE,
        };
    } else if (error.code === KNOWN_ERRORS.forgotPasswordUserNotConfirmed.code) {
        if (error.message === KNOWN_ERRORS.forgotPasswordUserNotConfirmed.message) {
            return {
                kind: "UserNotConfirmed",
                message: FORGOT_PASSWORD_USER_NOT_CONFIRMED_MESSAGE,
            }
        }
    }

    throw error;
}



// ============================
// === ForgotPasswordSubmit ===
// ============================

const forgotPasswordSubmit = async (email: string, code: string, password: string) => Result
    .wrapAsync(() => Auth.forgotPasswordSubmit(email, code, password))
    // We don't care about the details in the success case, just that it happened.
    .then(result => result.map(() => null))
    .then(result => result.mapErr(intoForgotPasswordSubmitErrorOrThrow))

type ForgotPasswordSubmitErrorKind =
    | "AuthError"
    | "AmplifyError";

export interface ForgotPasswordSubmitError {
    kind: ForgotPasswordSubmitErrorKind,
    message: string;
}

const intoForgotPasswordSubmitErrorOrThrow = (error: unknown): ForgotPasswordSubmitError => {
    if (isAuthError(error)) {
        return {
            kind: "AuthError",
            message: error.log,
        };
    } else if (isAmplifyError(error)) {
        return {
            kind: "AmplifyError",
            message: error.message,
        }
    }

    throw error;
}



// ===============
// === SignOut ===
// ===============

const signOut = async (logger: Logger) => {
    // FIXME [NP2]: For some reason, the redirect back to the IDE from the browser doesn't work
    // correctly so this `await` throws a timeout error. As a workaround, we catch this error and
    // force a refresh of the session manually by running the `signOut` again. This works because
    // Amplify will see that we've already signed out and clear the cache accordingly.  Ideally we
    // should figure out how to fix the redirect and remove this `catch`. This has the unintended
    // consequence of catching any other errors that might occur during sign out, that we really
    // shouldn't be catching. This also has the unintended consequence of delaying the sign out
    // process by a few seconds (until the timeout occurs).
    try {
        await Auth.signOut();
    } catch(error) {
        logger.error("Sign out failed", error);
    } finally {
        await Auth.signOut();
    }
}
