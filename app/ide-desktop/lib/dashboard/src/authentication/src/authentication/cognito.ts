/** @file Provides {@link Cognito} class which is the entrypoint into the AWS Amplify library.
 *
 * All of the functions used for authentication are provided by the AWS Amplify library, but we
 * provide a thin wrapper around them to make them easier to use. Mainly, we perform some error
 * handling and conditional logic to vary behavior between desktop & cloud. */
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
    async userSession(this: void) {
        const amplifySession = await getAmplifyCurrentSession()
        return amplifySession.map(parseUserSession).toOption()
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
     * - GitHub,
     * - Google, or
     * - Email. */
    email: string
    /** User's access token, used to authenticate the user (e.g., when making API calls). */
    accessToken: string
}

const NO_CURRENT_USER_ERROR_KIND = 'NoCurrentUser' as const
type CurrentSessionErrorKind = typeof NO_CURRENT_USER_ERROR_KIND

function intoCurrentSessionErrorKind(error: unknown): CurrentSessionErrorKind {
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
async function getAmplifyCurrentSession() {
    const currentSession = await results.Result.wrapAsync(() => amplify.Auth.currentSession())
    return currentSession.mapErr(intoCurrentSessionErrorKind)
}

/** Parses a `CognitoUserSession` into a `UserSession`.
 * @throws If the `email` field of the payload is not a string. */
function parseUserSession(session: cognito.CognitoUserSession): UserSession {
    const payload: Record<string, unknown> = session.getIdToken().payload
    const email = payload.email
    /** The `email` field is mandatory, so we assert that it exists and is a string. */
    if (typeof email !== 'string') {
        throw new Error('Payload does not have an email field.')
    }
    const accessToken = session.getAccessToken().getJwtToken()
    return { email, accessToken }
}
