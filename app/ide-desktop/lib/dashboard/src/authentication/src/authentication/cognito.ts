/** @file Provides {@link Cognito} class which is the entrypoint into the AWS Amplify library.
 *
 * All of the functions used for authentication are provided by the AWS Amplify library, but we
 * provide a thin wrapper around them to make them easier to use. Mainly, we perform some error
 * handling and conditional logic to vary behaviour between desktop & cloud. */
import * as amplify from '@aws-amplify/auth'
import * as cognito from 'amazon-cognito-identity-js'
import * as results from 'ts-results'

import * as app from '../components/app'
import * as config from './config'
import * as loggerProvider from '../providers/logger'

// =================
// === Constants ===
// =================

/** String used to identify the GitHub federated identity provider in AWS Amplify.
 *
 * This provider alone requires a string because it is not a standard provider, and thus has no
 * constant defined in the AWS Amplify library. */
const GITHUB_PROVIDER = 'Github'

const MESSAGES = {
    signInWithPassword: {
        userNotFound: 'User not found. Please register first.',
        userNotConfirmed: 'User is not confirmed. Please check your email for a confirmation link.',
        incorrectUsernameOrPassword: 'Incorrect username or password.',
    },
}

/** A list of errors thrown by the Amplify library that we catch and handle ourselves.
 *
 * Not all errors are caught and handled. Any errors not listed here are allowed to propagate up.
 *
 * Errors are grouped by the AWS Amplify function that throws the error (e.g., `signUp`).
 *
 * Each error must include an `internalCode` field, which is the code that the Amplify library uses
 * to identify the error.
 *
 * Some errors also include an `internalMessage` field, which is the message that the Amplify
 * library associates with the error. This field is used to distinguish between errors that have the
 * same `internalCode`.
 *
 * Amplify reuses some codes for multiple kinds of errors. In the case of ambiguous errors, the
 * `kind` field provides a unique string that can be used to brand the error in place of the
 * `internalCode`, when rethrowing the error. */
const KNOWN_ERRORS = {
    /** Errors specific to the `currentSession` function. */
    currentSession: {
        noCurrentUser: {
            message: 'No current user' as const,
            kind: 'NoCurrentUser' as const,
        },
    },
    /** Errors specific to the `signUp` function. */
    signUp: {
        usernameExists: {
            internalCode: 'UsernameExistsException' as const,
            kind: 'UsernameExists' as const,
        },
        invalidParameter: {
            internalCode: 'InvalidParameterException' as const,
            kind: 'InvalidParameter' as const,
        },
    },
    /** Errors specific to the `confirmSignUp` function. */
    confirmSignUp: {
        userAlreadyConfirmed: {
            internalCode: 'NotAuthorizedException' as const,
            internalMessage: 'User cannot be confirmed. Current status is CONFIRMED' as const,
            kind: 'UserAlreadyConfirmed' as const,
        },
    },
}

// ====================
// === AmplifyError ===
// ====================

/** Error thrown by the AWS Amplify library when an Amplify error occurs.
 *
 * Some Amplify errors (e.g., network connectivity errors) can not be resolved within the
 * application. Un-resolvable errors are allowed to flow up to the top-level error handler. Errors
 * that can be resolved must be caught and handled as early as possible. The {@link KNOWN_ERRORS}
 * map lists the Amplify errors that we want to catch and convert to typed responses.
 *
 * # Handling Amplify Errors
 *
 * Use the {@link isAmplifyError} function to check if an `unknown` error is an
 * {@link AmplifyError}. If it is, use the {@link intoAmplifyErrorOrThrow} function to convert it
 * from `unknown` to a typed object. Then, use the {@link KNOWN_ERRORS} to see if the error is one
 * that must be handled by the application (i.e., it is an error that is relevant to our business
 * logic). */
interface AmplifyError extends Error {
    /** Error code for disambiguating the error. */
    code: string
}

/** Hints to TypeScript if we can safely cast an `unknown` error to an {@link AmplifyError}. */
const isAmplifyError = (error: unknown): error is AmplifyError => {
    if (error && typeof error === 'object') {
        return 'code' in error && 'message' in error && 'name' in error
    }
    return false
}

/** Converts the `unknown` error into an {@link AmplifyError} and returns it, or re-throws it if
 * conversion is not possible. */
const intoAmplifyErrorOrThrow = (error: unknown): AmplifyError => {
    if (isAmplifyError(error)) {
        return error
    } else {
        throw error
    }
}

// ===============
// === Cognito ===
// ===============

/** Interface defining the methods provided by this module for interacting with Cognito for
 * authentication.
 *
 * The methods defined in this API are thin wrappers around the AWS Amplify library with error
 * handling added. This way, the methods don't throw all errors, but define exactly which errors
 * they return. The caller can then handle them via pattern matching on the {@link results.Result}
 * type. */
export interface Cognito {
    /** Returns the current {@link UserSession}, or `None` if the user is not logged in.
     *
     * Will refresh the {@link UserSession} if it has expired. */
    userSession: () => Promise<results.Option<UserSession>>
    /** Sign up with with username and password.
     *
     * Does not rely on federated identity providers (e.g., Google or GitHub). */
    signUp: (username: string, password: string) => Promise<results.Result<null, SignUpError>>
    /** Sends the email address verification code.
     *
     * The user will receive a link in their email. The user must click the link to go to the email
     * verification page. The email verification page will parse the verification code from the URL.
     * If the verification code matches, the email address is marked as verified. Once the email
     * address is verified, the user can sign in. */
    confirmSignUp: (
        email: string,
        code: string
    ) => Promise<results.Result<null, ConfirmSignUpError>>
    /** Signs in via the Google federated identity provider.
     *
     * This function will open the Google authentication page in the user's browser. The user will
     * be asked to log in to their Google account, and then to grant access to the application.
     * After the user has granted access, the browser will be redirected to the application. */
    signInWithGoogle: () => Promise<null>
    /** Signs in via the GitHub federated identity provider.
     *
     * This function will open the GitHub authentication page in the user's browser. The user will
     * be asked to log in to their GitHub account, and then to grant access to the application.
     * After the user has granted access, the browser will be redirected to the application. */
    signInWithGitHub: () => Promise<null>
    /** Signs in with the given username and password.
     *
     * Does not rely on external identity providers (e.g., Google or GitHub).
     *
     * @param username - Username of the user to sign in.
     * @param password - Password of the user to sign in.
     * @returns A promise that resolves to either success or known error.
     * @throws An error if failed due to an unknown error. */
    signInWithPassword: (
        username: string,
        password: string
    ) => Promise<results.Result<null, SignInWithPasswordError>>
    /** Signs out the current user.
     *
     * @returns A promise that resolves if successful. */
    signOut: () => Promise<void>
}

// ===================
// === CognitoImpl ===
// ===================

/** A class implementing the {@link Cognito} API by wrapping AWS Amplify functions. */
export class CognitoImpl implements Cognito {
    private readonly logger: loggerProvider.Logger
    private readonly platform: app.Platform

    constructor(
        logger: loggerProvider.Logger,
        platform: app.Platform,
        amplifyConfig: config.AmplifyConfig
    ) {
        this.logger = logger
        this.platform = platform

        /** Amplify expects `Auth.configure` to be called before any other `Auth` methods are
         * called. By wrapping all the `Auth` methods we care about and returning an `Cognito` API
         * object containing them, we ensure that `Auth.configure` is called before any other `Auth`
         * methods are called. */
        const nestedAmplifyConfig = config.toNestedAmplifyConfig(amplifyConfig)
        amplify.Auth.configure(nestedAmplifyConfig)
    }

    // === Interface `impl`s ===

    /** We want to signal to Amplify to fire a "custom state change" event when the user is
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
     * See: https://github.com/aws-amplify/amplify-js/issues/3391#issuecomment-756473970 */
    customState = () =>
        this.platform === app.Platform.desktop ? window.location.pathname : undefined
    userSession = userSession
    signUp = (username: string, password: string) => signUp(username, password, this.platform)
    confirmSignUp = confirmSignUp
    signInWithGoogle = () => signInWithGoogle(this.customState())
    signInWithGitHub = signInWithGitHub
    signInWithPassword = signInWithPassword
    signOut = () => signOut(this.logger)
}

// ====================
// === AssertString ===
// ====================

/** Type signature for a function that asserts that a parameter is a string. */
type AssertString = (param: any, message: string) => asserts param is string

/** Asserts that a parameter is a string; throws an error `message` if the assertion fails.
 *
 * Used both to assert that a parameter is a string at runtime, and to inform TypeScript that a
 * parameter is a string. */
const assertString: AssertString = (param, message) => {
    if (typeof param !== 'string') {
        throw new Error(message)
    }
}

// ===================
// === UserSession ===
// ===================

/** User's session, provides information for identifying and authenticating the user. */
export interface UserSession {
    /** User's email address, used to uniquely identify the user.
     *
     * Provided by the identity provider the user used to log in. One of:
     *
     * - GitHub
     * - Google
     * - Email */
    email: string
    /** User's access token, used to authenticate the user (e.g., when making API calls). */
    accessToken: string
}

const userSession = () =>
    getAmplifyCurrentSession()
        .then(result => result.map(parseUserSession))
        .then(result => result.toOption())

type CurrentSessionErrorKind = typeof KNOWN_ERRORS.currentSession.noCurrentUser.kind

const intoCurrentSessionErrorKind = (error: unknown): CurrentSessionErrorKind => {
    if (error === KNOWN_ERRORS.currentSession.noCurrentUser.message) {
        return KNOWN_ERRORS.currentSession.noCurrentUser.kind
    } else {
        throw error
    }
}

/** Returns the current `CognitoUserSession` if the user is logged in, or `CurrentSessionErrorKind`
 * otherwise.
 *
 * Will refresh the session if it has expired. */
const getAmplifyCurrentSession = () =>
    results.Result.wrapAsync(() => amplify.Auth.currentSession()).then(result =>
        result.mapErr(intoCurrentSessionErrorKind)
    )

/** Parses a `CognitoUserSession` into a `UserSession`. */
const parseUserSession = (session: cognito.CognitoUserSession): UserSession => {
    const payload = session.getIdToken().payload
    /** The `email` field is mandatory, so we assert that it exists and is a string. */
    assertString(payload.email, 'Payload does not have an email field.')
    const email = payload.email
    const accessToken = session.getAccessToken().getJwtToken()
    return { email, accessToken }
}

// ==============
// === SignUp ===
// ==============

const signUp = (username: string, password: string, platform: app.Platform) =>
    results.Result.wrapAsync(() => {
        const params = intoSignUpParams(username, password, platform)
        return amplify.Auth.signUp(params)
    })
        /** The contents of a successful response are not relevant, so we discard them. */
        .then(result => result.map(() => null))
        .then(result => result.mapErr(intoAmplifyErrorOrThrow))
        .then(result => result.mapErr(intoSignUpErrorOrThrow))

const intoSignUpParams = (
    username: string,
    password: string,
    platform: app.Platform
): amplify.SignUpParams => ({
    username,
    password,
    attributes: {
        email: username,
        /** Add a custom attribute indicating whether the user is signing up from the desktop. This
         * is used to determine the schema used in the callback links sent in the verification
         * emails. For example, `http://` for the Cloud, and `enso://` for the desktop. */
        /** # Safety
         *
         * It is necessary to disable the naming convention rule here, because the key is expected
         * to appear exactly as-is in Cognito, so we must match it. */
        // eslint-disable-next-line @typescript-eslint/naming-convention
        'custom:fromDesktop': platform === app.Platform.desktop ? 'true' : 'false',
    },
})

type SignUpErrorKind =
    | typeof KNOWN_ERRORS.signUp.usernameExists.kind
    | typeof KNOWN_ERRORS.signUp.invalidParameter.kind

export interface SignUpError {
    kind: SignUpErrorKind
    message: string
}

const intoSignUpErrorOrThrow = (error: AmplifyError): SignUpError => {
    if (error.code === KNOWN_ERRORS.signUp.usernameExists.internalCode) {
        return {
            kind: KNOWN_ERRORS.signUp.usernameExists.kind,
            message: error.message,
        }
    } else if (error.code === KNOWN_ERRORS.signUp.invalidParameter.internalCode) {
        return {
            kind: KNOWN_ERRORS.signUp.invalidParameter.kind,
            message: error.message,
        }
    }

    throw error
}

// =====================
// === ConfirmSignUp ===
// =====================

const confirmSignUp = async (email: string, code: string) =>
    results.Result.wrapAsync(() => amplify.Auth.confirmSignUp(email, code))
        /** The contents of a successful response are not relevant, so we discard them. */
        .then(result => result.map(() => null))
        .then(result => result.mapErr(intoAmplifyErrorOrThrow))
        .then(result => result.mapErr(intoConfirmSignUpErrorOrThrow))

type ConfirmSignUpErrorKind = typeof KNOWN_ERRORS.confirmSignUp.userAlreadyConfirmed.kind

export interface ConfirmSignUpError {
    kind: ConfirmSignUpErrorKind
    message: string
}

const intoConfirmSignUpErrorOrThrow = (error: AmplifyError): ConfirmSignUpError => {
    if (error.code === KNOWN_ERRORS.confirmSignUp.userAlreadyConfirmed.internalCode) {
        if (error.message === KNOWN_ERRORS.confirmSignUp.userAlreadyConfirmed.internalMessage) {
            return {
                /** Don't re-use the original `error.code` here because Amplify overloads the same
                 * code for multiple kinds of errors. We replace it with a custom code that has no
                 * ambiguity. */
                kind: KNOWN_ERRORS.confirmSignUp.userAlreadyConfirmed.kind,
                message: error.message,
            }
        }
    }

    throw error
}

// ========================
// === SignInWithGoogle ===
// ========================

const signInWithGoogle = async (customState?: string) =>
    amplify.Auth.federatedSignIn({
        provider: amplify.CognitoHostedUIIdentityProvider.Google,
        customState,
    })
        /** We don't care about the details in the success case, just that it happened. */
        .then(() => null)

// ========================
// === SignInWithGoogle ===
// ========================

const signInWithGitHub = async () =>
    amplify.Auth.federatedSignIn({
        customProvider: GITHUB_PROVIDER,
    })
        /** We don't care about the details in the success case, just that it happened. */
        .then(() => null)

// ==========================
// === SignInWithPassword ===
// ==========================

const signInWithPassword = async (username: string, password: string) =>
    results.Result.wrapAsync(() => amplify.Auth.signIn(username, password))
        /** We don't care about the details in the success case, just that it happened. */
        .then(result => result.map(() => null))
        .then(result => result.mapErr(intoAmplifyErrorOrThrow))
        .then(result => result.mapErr(intoSignInWithPasswordErrorOrThrow))

type SignInWithPasswordErrorKind = 'UserNotFound' | 'UserNotConfirmed' | 'NotAuthorized'

export interface SignInWithPasswordError {
    kind: SignInWithPasswordErrorKind
    message: string
}

const intoSignInWithPasswordErrorOrThrow = (error: AmplifyError): SignInWithPasswordError => {
    switch (error.code) {
        case 'UserNotFoundException':
            return {
                kind: 'UserNotFound',
                message: MESSAGES.signInWithPassword.userNotFound,
            }
        case 'UserNotConfirmedException':
            return {
                kind: 'UserNotConfirmed',
                message: MESSAGES.signInWithPassword.userNotConfirmed,
            }
        case 'NotAuthorizedException':
            return {
                kind: 'NotAuthorized',
                message: MESSAGES.signInWithPassword.incorrectUsernameOrPassword,
            }
    }

    throw error
}



// ===============
// === SignOut ===
// ===============

const signOut = async (logger: loggerProvider.Logger) => {
    // TODO [NP]: https://github.com/enso-org/cloud-v2/issues/341
    // For some reason, the redirect back to the IDE from the browser doesn't work correctly so this
    // `await` throws a timeout error. As a workaround, we catch this error and force a refresh of
    // the session manually by running the `signOut` again. This works because Amplify will see that
    // we've already signed out and clear the cache accordingly.  Ideally we should figure out how
    // to fix the redirect and remove this `catch`. This has the unintended consequence of catching
    // any other errors that might occur during sign out, that we really shouldn't be catching. This
    // also has the unintended consequence of delaying the sign out process by a few seconds (until
    // the timeout occurs).
    try {
        await amplify.Auth.signOut()
    } catch (error) {
        logger.error('Sign out failed', error)
    } finally {
        await amplify.Auth.signOut()
    }
}
