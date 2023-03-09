/** @file Provides {@link Cognito} class which is the entrypoint into the AWS Amplify library.
 *
 * All of the functions used for authentication are provided by the AWS Amplify library, but we
 * provide a thin wrapper around them to make them easier to use. Mainly, we perform some error
 * handling and conditional logic to vary behaviour between desktop & cloud. */
import * as amplify from '@aws-amplify/auth'
import * as cognito from 'amazon-cognito-identity-js'
import * as results from 'ts-results'

import * as loggerProvider from '../providers/logger'
import * as config from './config'



// =================
// === Constants ===
// =================

/** A list of known Amplify errors that we can match against prior to trying to convert to our
 * internal error types. This is useful for disambiguating between Amplify errors with the same code
 * but different messages. */
const KNOWN_ERRORS = {
    userAlreadyConfirmed: {
        code: 'NotAuthorizedException',
        message: 'User cannot be confirmed. Current status is CONFIRMED',
    },
}



// ====================
// === AmplifyError ===
// ====================

/** Object returned by the AWS Amplify library when an Amplify error occurs. */
interface AmplifyError extends Error {
    /** Error code for disambiguating the error. */
    code: string
}

/** Hints to TypeScript if we can safely cast an `unknown` error to an `AmplifyError`. */
const isAmplifyError = (error: unknown): error is AmplifyError => {
    if (error && typeof error === 'object') {
        return 'code' in error && 'message' in error && 'name' in error
    }
    return false
}

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

export interface Cognito {
    /** Returns the current user's session.
     *
     * Will refresh the session if it has expired.
     *
     * @returns `UserSession` if the user is logged in, `None` otherwise.
     * @throws An error if failed due to an unknown error. */
    userSession: () => Promise<results.Option<UserSession>>
    /** Sign up with the given parameters (i.e., username and password).
     *
     * Does not rely on external identity providers (e.g., Google or GitHub).
     *
     * @returns A promise that resolves to either success or known error.
     * @throws An error if failed due to an unknown error. */
    signUp: (username: string, password: string) => Promise<results.Result<null, SignUpError>>
    /** Sends the verification code to confirm the user's email address.
     *
     * @param email - User's email address.
     * @param code - Verification code that was sent to the user's email address.
     * @returns A promise that resolves to either success or known error.
     * @throws An error if failed due to an unknown error. */
    confirmSignUp: (
        email: string,
        code: string
    ) => Promise<results.Result<null, ConfirmSignUpError>>
}



// ===================
// === CognitoImpl ===
// ===================

export class CognitoImpl implements Cognito {
    private readonly logger: loggerProvider.Logger
    private readonly fromDesktop: boolean

    constructor(
        logger: loggerProvider.Logger,
        fromDesktop: boolean,
        amplifyConfig: config.AmplifyConfig
    ) {
        this.logger = logger
        this.fromDesktop = fromDesktop

        // Amplify expects `Auth.configure` to be called before any other `Auth` methods are called.
        // By wrapping all the `Auth` methods we care about and returning an `Cognito` API object
        // containing them, we ensure that `Auth.configure` is called before any other `Auth`
        // methods are called.
        const nestedAmplifyConfig = config.toNestedAmplifyConfig(amplifyConfig)
        amplify.Auth.configure(nestedAmplifyConfig)
    }

    // === Interface `impl`s ===

    userSession = userSession
    signUp = (username: string, password: string) => signUp(username, password, this.fromDesktop)
    confirmSignUp = confirmSignUp
}



// ====================
// === AssertString ===
// ====================

/** Type signature for a function that asserts that a parameter is a string. */
type AssertString = (param: any, message: string) => asserts param is string

/** Asserts that a parameter is a string.
 *
 * Used both to assert that a parameter is a string at runtime, and to inform TypeScript that a
 * parameter is a string.
 *
 * @param param - The parameter to assert.
 * @param message - The error message to throw if the assertion fails.
 * @throws An error if the assertion fails. */
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

type CurrentSessionErrorKind = 'NoCurrentUser'

const intoCurrentSessionErrorKind = (error: unknown): CurrentSessionErrorKind => {
    if (error === 'No current user') {
        return 'NoCurrentUser'
    } else {
        throw error
    }
}

/** Returns the current `CognitoUserSession`.
 *
 * Will refresh the session if it has expired.
 *
 * @returns `CognitoUserSession` if the user is logged in, `CurrentSessionErrorKind`
 * otherwise. */
const getAmplifyCurrentSession = () =>
    results.Result.wrapAsync(() => amplify.Auth.currentSession()).then(result =>
        result.mapErr(intoCurrentSessionErrorKind)
    )

/** Parses a `CognitoUserSession` into a `UserSession`. */
const parseUserSession = (session: cognito.CognitoUserSession): UserSession => {
    const payload = session.getIdToken().payload
    // The `email` field is mandatory, so we assert that it exists and is a string.
    assertString(payload.email, 'Payload does not have an email field.')
    const email = payload.email
    const accessToken = session.getAccessToken().getJwtToken()

    return { email, accessToken }
}



// ==============
// === SignUp ===
// ==============

const signUp = (username: string, password: string, fromDesktop: boolean) =>
    results.Result.wrapAsync(() => {
        const params = intoSignUpParams(username, password, fromDesktop)
        return amplify.Auth.signUp(params)
    })
        // We don't care about the details in the success case, just that it happened.
        .then(result => result.map(() => null))
        .then(result => result.mapErr(intoAmplifyErrorOrThrow))
        .then(result => result.mapErr(intoSignUpErrorOrThrow))

const intoSignUpParams = (
    username: string,
    password: string,
    fromDesktop: boolean
): amplify.SignUpParams => ({
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
        'custom:fromDesktop': fromDesktop ? 'true' : 'false',
    },
})

type SignUpErrorKind = 'UsernameExists' | 'InvalidParameter'

export interface SignUpError {
    kind: SignUpErrorKind
    message: string
}

const intoSignUpErrorOrThrow = (error: AmplifyError): SignUpError => {
    if (error.code === 'UsernameExistsException') {
        return {
            kind: 'UsernameExists',
            message: error.message,
        }
    } else if (error.code === 'InvalidParameterException') {
        return {
            kind: 'InvalidParameter',
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
        // We don't care about the details in the success case, just that it happened.
        .then(result => result.map(() => null))
        .then(result => result.mapErr(intoAmplifyErrorOrThrow))
        .then(result => result.mapErr(intoConfirmSignUpErrorOrThrow))

type ConfirmSignUpErrorKind = 'UserAlreadyConfirmed'

export interface ConfirmSignUpError {
    kind: ConfirmSignUpErrorKind
    message: string
}

const intoConfirmSignUpErrorOrThrow = (error: AmplifyError): ConfirmSignUpError => {
    if (error.code === KNOWN_ERRORS.userAlreadyConfirmed.code) {
        if (error.message === KNOWN_ERRORS.userAlreadyConfirmed.message) {
            return {
                // We don't re-use the `error.code` here because Amplify overloads the same kind
                // for multiple kinds of errors that we want to disambiguate.
                kind: 'UserAlreadyConfirmed',
                message: error.message,
            }
        }
    }

    throw error
}
