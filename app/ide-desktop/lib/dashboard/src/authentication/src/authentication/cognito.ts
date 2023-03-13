/** @file Provides {@link Cognito} class which is the entrypoint into the AWS Amplify library.
 *
 * All of the functions used for authentication are provided by the AWS Amplify library, but we
 * provide a thin wrapper around them to make them easier to use. Mainly, we perform some error
 * handling and conditional logic to vary behaviour between desktop & cloud. */
import * as amplify from '@aws-amplify/auth'
import * as cognito from 'amazon-cognito-identity-js'
import * as results from 'ts-results'

import * as config from './config'

// ===============
// === Cognito ===
// ===============

export class Cognito {
    constructor(amplifyConfig: config.AmplifyConfig) {
        /** Amplify expects `Auth.configure` to be called before any other `Auth` methods are
         * called. By wrapping all the `Auth` methods we care about and returning an `Cognito` API
         * object containing them, we ensure that `Auth.configure` is called before any other `Auth`
         * methods are called. */
        const nestedAmplifyConfig = config.toNestedAmplifyConfig(amplifyConfig)
        amplify.Auth.configure(nestedAmplifyConfig)
    }

    // === Interface `impl`s ===

    /** Returns the current user's session, or `None` if the user is not logged in.
     *
     * Will refresh the session if it has expired. */
    userSession: () => Promise<results.Option<UserSession>> = userSession
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

const NO_CURRENT_USER_ERROR_KIND = 'NoCurrentUser' as const
type CurrentSessionErrorKind = typeof NO_CURRENT_USER_ERROR_KIND

const intoCurrentSessionErrorKind = (error: unknown): CurrentSessionErrorKind => {
    if (error === 'No current user') {
        return NO_CURRENT_USER_ERROR_KIND
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
