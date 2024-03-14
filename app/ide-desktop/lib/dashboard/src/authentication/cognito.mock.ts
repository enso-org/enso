/** @file Provides {@link Cognito} class which is the entrypoint into the AWS Amplify library.
 *
 * All of the functions used for authentication are provided by the AWS Amplify library, but we
 * provide a thin wrapper around them to make them easier to use. Mainly, we perform some error
 * handling and conditional logic to vary behavior between desktop & cloud.
 *
 * # Error Handling
 *
 * The AWS Amplify library throws errors when authentication fails. We catch these errors and
 * convert them to typed responses. This allows us to exhaustively handle errors by providing
 * information on the types of errors returned, in function return types.
 *
 * Not all errors are caught and handled. Any errors not relevant to business logic or control flow
 * are allowed to propagate up.
 *
 * Errors are grouped by the AWS Amplify function that throws the error (e.g., `signUp`). This is
 * because the Amplify library reuses some error codes for multiple kinds of errors. For example,
 * the `UsernameExistsException` error code is used for both the `signUp` and `confirmSignUp`
 * functions. This would be fine if the same error code didn't meet different conditions for each
 *
 * Each error must provide a way to disambiguate from other errors. Typically, our error definitions
 * include an `internalCode` field, which is the code that the Amplify library uses to identify the
 * error.
 *
 * Some errors also include an `internalMessage` field, which is the message that the Amplify
 * library associates with the error. This field is used to distinguish between errors that have the
 * same `internalCode`.
 *
 * Amplify reuses some codes for multiple kinds of errors. In the case of ambiguous errors, the
 * `kind` field provides a unique string that can be used to brand the error in place of the
 * `internalCode`, when rethrowing the error. */
import type * as cognito from 'amazon-cognito-identity-js'

import type * as loggerProvider from '#/providers/LoggerProvider'

import type * as service from '#/authentication/service'

import type * as original from './cognito.ts'
import * as listen from './listen.mock'

// This file exports a subset of the values from the original file.
// @ts-expect-error This is a mock file that needs to reference its original file.
// eslint-disable-next-line no-restricted-syntax
export { UserSession, CognitoErrorType, isAmplifyError } from './cognito.ts'

// There are unused function parameters in this file.
/* eslint-disable @typescript-eslint/no-unused-vars */

// =================
// === Constants ===
// =================

/** One second, in milliseconds. */
const SEC_MS = 1_000
/** Ten hours, in seconds. */
const TEN_HOURS_S = 36_000

// ===============
// === Cognito ===
// ===============

const MOCK_ORGANIZATION_ID_KEY = 'mock_organization_id'
const MOCK_EMAIL_KEY = 'mock_email'

let mockOrganizationId = localStorage.getItem(MOCK_ORGANIZATION_ID_KEY)
let mockEmail = localStorage.getItem(MOCK_EMAIL_KEY)

// All documentation in this class is inherited from the class it implements.
/* eslint-disable jsdoc/require-jsdoc */
/** Thin wrapper around Cognito endpoints from the AWS Amplify library with error handling added.
 * This way, the methods don't throw all errors, but define exactly which errors they return. */
// @ts-expect-error The private properties are structurally incompatible because they are not
// visible from outside the class.
export class Cognito implements original.Cognito {
  isSignedIn = false

  constructor(
    private readonly logger: loggerProvider.Logger,
    private readonly supportsDeepLinks: boolean,
    private readonly amplifyConfig: service.AmplifyConfig
  ) {}

  saveAccessToken() {
    // Ignored.
  }

  userSession() {
    const date = Math.floor(Number(new Date()) / SEC_MS)
    const expirationDate = date + TEN_HOURS_S
    if (!this.isSignedIn) {
      return Promise.resolve(null)
    } else {
      const currentSession: cognito.CognitoUserSession = {
        isValid: () => true,
        getRefreshToken: () => ({
          getToken: () => '',
        }),
        getIdToken: () => ({
          payload: {
            email: mockEmail,
          },
          decodePayload: () => ({}),
          // Do not need to be the same as the dates for the access token.
          getIssuedAt: () => date,
          getExpiration: () => expirationDate,
          getJwtToken: () => '',
        }),
        getAccessToken: () => ({
          payload: {},
          decodePayload: () => ({}),
          getIssuedAt: () => date,
          getExpiration: () => expirationDate,
          getJwtToken: () =>
            `.${window.btoa(
              JSON.stringify({
                /* eslint-disable @typescript-eslint/naming-convention */
                sub: '62bdf414-c47f-4c76-a333-c564f841c256',
                iss: 'https://cognito-idp.eu-west-1.amazonaws.com/eu-west-1_9Kycu2SbD',
                client_id: '4j9bfs8e7415erf82l129v0qhe',
                origin_jti: '3bd05163-dce7-496e-93f4-ac84c33448aa',
                event_id: '7392b8de-66d6-4f60-8050-a90253911e45',
                token_use: 'access',
                scope: 'aws.cognito.signin.user.admin',
                auth_time: date,
                exp: expirationDate,
                iat: date,
                jti: '5ab178b7-97a6-4956-8913-1cffee4a0da1',
                username: mockEmail,
                /* eslint-enable @typescript-eslint/naming-convention */
              })
            )}.`,
        }),
      }
      return Promise.resolve(parseUserSession(currentSession))
    }
  }

  organizationId() {
    return Promise.resolve(mockOrganizationId)
  }

  signUp(username: string, password: string, organizationId: string | null) {
    mockOrganizationId = organizationId
    if (organizationId != null) {
      localStorage.setItem(MOCK_ORGANIZATION_ID_KEY, organizationId)
    } else {
      localStorage.removeItem(MOCK_ORGANIZATION_ID_KEY)
    }
    return signUp(this.supportsDeepLinks, username, password, organizationId)
  }

  confirmSignUp(email: string, code: string) {
    mockEmail = email
    localStorage.setItem(MOCK_EMAIL_KEY, email)
    return confirmSignUp(email, code)
  }

  signInWithGoogle() {
    this.isSignedIn = true
    listen.authEventListener?.(listen.AuthEvent.signIn)
    return Promise.resolve()
  }

  signInWithGitHub() {
    this.isSignedIn = true
    listen.authEventListener?.(listen.AuthEvent.signIn)
    return Promise.resolve()
  }

  /** Sign in with the given username and password.
   *
   * Does not rely on external identity providers (e.g., Google or GitHub). */
  signInWithPassword(username: string, _password: string) {
    this.isSignedIn = true
    mockEmail = username
    localStorage.setItem(MOCK_EMAIL_KEY, username)
    listen.authEventListener?.(listen.AuthEvent.signIn)
    return Promise.resolve(null)
  }

  /** Sign out the current user. */
  signOut() {
    listen.authEventListener?.(listen.AuthEvent.signOut)
    this.isSignedIn = false
    return Promise.resolve()
  }

  /** Mock implementation. */
  forgotPassword(_email: string) {
    return Promise.resolve(null)
  }

  /** Mock implementation. */
  forgotPasswordSubmit(_email: string, _code: string, _password: string) {
    return Promise.resolve(null)
  }

  /** Mock implementation. */
  changePassword(_oldPassword: string, _newPassword: string) {
    return Promise.resolve(null)
  }

  /** Mock implementation. */
  private customState() {
    return null
  }
}
/* eslint-enable jsdoc/require-jsdoc */

// ===================
// === UserSession ===
// ===================

/** Parse a {@link cognito.CognitoUserSession} into an {@link original.UserSession}.
 * @throws If the `email` field of the payload is not a string. */
function parseUserSession(session: cognito.CognitoUserSession): original.UserSession {
  const payload: Readonly<Record<string, unknown>> = session.getIdToken().payload
  const email = payload.email
  /** The `email` field is mandatory, so we assert that it exists and is a string. */
  if (typeof email !== 'string') {
    throw new Error('Payload does not have an email field.')
  } else {
    const accessToken = `.${window.btoa(JSON.stringify({ username: email }))}.`
    return { email, accessToken, refreshToken: '', clientId: '', expireAt: '', refreshUrl: '' }
  }
}

// ==============
// === SignUp ===
// ==============

/** A wrapper around the Amplify "sign up" endpoint that converts known errors
 * to `SignUpError`s. */
function signUp(
  _supportsDeepLinks: boolean,
  _username: string,
  _password: string,
  _organizationId: string | null
) {
  return Promise.resolve(null)
}

// =====================
// === ConfirmSignUp ===
// =====================

/** A wrapper around the Amplify "confirm sign up" endpoint that converts known errors
 * to `ConfirmSignUpError`s. */
function confirmSignUp(_email: string, _code: string) {
  return Promise.resolve(null)
}
