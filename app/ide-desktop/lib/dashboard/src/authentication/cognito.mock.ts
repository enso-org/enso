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
import type * as amplify from '@aws-amplify/auth'
import type * as cognito from 'amazon-cognito-identity-js'
import * as results from 'ts-results'

import type * as loggerProvider from '../providers/LoggerProvider'
// @ts-expect-error This is a mock file that needs to reference its original file.
import * as original from './cognito.ts'
import type * as config from './config'
/* eslint-enable no-restricted-syntax */

import * as listen from './listen.mock'

// This file exports a subset of the values from the original file.
/* eslint-disable no-restricted-syntax */
// @ts-expect-error This is a mock file that needs to reference its original file.
export { CognitoErrorType } from './cognito.ts'

// There are unused function parameters in this file.
/* eslint-disable @typescript-eslint/no-unused-vars */

// =================
// === Constants ===
// =================

/** One second, in milliseconds. */
const SEC_MS = 1_000
/** One day, in milliseconds. */
const DAY_MS = 86_400_000
/** Ten hours, in seconds. */
const TEN_HOURS_S = 36_000

// ===============
// === Cognito ===
// ===============

const MOCK_ORGANIZATION_ID_KEY = 'mock_organization_id'
const MOCK_EMAIL_KEY = 'mock_email'

let mockOrganizationId = localStorage.getItem(MOCK_ORGANIZATION_ID_KEY)
let mockEmail = localStorage.getItem(MOCK_EMAIL_KEY)

/** Thin wrapper around Cognito endpoints from the AWS Amplify library with error handling added.
 * This way, the methods don't throw all errors, but define exactly which errors they return.
 * The caller can then handle them via pattern matching on the {@link results.Result} type. */
export class Cognito {
  isSignedIn = false

  /** Create a new Cognito wrapper. */
  constructor(
    private readonly logger: loggerProvider.Logger,
    private readonly supportsDeepLinks: boolean,
    private readonly amplifyConfig: config.AmplifyConfig
  ) {}

  /** Save the access token to a file for further reuse. */
  saveAccessToken() {
    // Ignored.
  }

  /** Return the current {@link UserSession}, or `None` if the user is not logged in.
   *
   * Will refresh the {@link UserSession} if it has expired. */
  async userSession() {
    const currentSession = await results.Result.wrapAsync(() => {
      const date = Math.floor(Number(new Date()) / SEC_MS)
      const expirationDate = date + TEN_HOURS_S
      if (!this.isSignedIn) {
        // eslint-disable-next-line @typescript-eslint/no-throw-literal
        throw 'No current user'
      } else {
        return Promise.resolve<cognito.CognitoUserSession>({
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
        })
      }
    })
    const amplifySession = currentSession.mapErr(original.intoCurrentSessionErrorType)
    return amplifySession.map(parseUserSession).unwrapOr(null)
  }

  /** Returns the associated organization ID of the current user, which is passed during signup,
   * or `null` if the user is not associated with an existing organization. */
  async organizationId() {
    return Promise.resolve(mockOrganizationId)
  }

  /** Sign up with username and password.
   *
   * Does not rely on federated identity providers (e.g., Google or GitHub). */
  signUp(username: string, password: string, organizationId: string | null) {
    mockOrganizationId = organizationId
    if (organizationId != null) {
      localStorage.setItem(MOCK_ORGANIZATION_ID_KEY, organizationId)
    } else {
      localStorage.removeItem(MOCK_ORGANIZATION_ID_KEY)
    }
    return signUp(this.supportsDeepLinks, username, password, organizationId)
  }

  /** Send the email address verification code.
   *
   * The user will receive a link in their email. The user must click the link to go to the email
   * verification page. The email verification page will parse the verification code from the URL.
   * If the verification code matches, the email address is marked as verified. Once the email
   * address is verified, the user can sign in. */
  confirmSignUp(email: string, code: string) {
    mockEmail = email
    localStorage.setItem(MOCK_EMAIL_KEY, email)
    return confirmSignUp(email, code)
  }

  /** Sign in via the Google federated identity provider.
   *
   * This function will open the Google authentication page in the user's browser. The user will
   * be asked to log in to their Google account, and then to grant access to the application.
   * After the user has granted access, the browser will be redirected to the application. */
  async signInWithGoogle() {
    this.isSignedIn = true
    listen.authEventListener?.(listen.AuthEvent.signIn)
    await Promise.resolve()
  }

  /** Sign in via the GitHub federated identity provider.
   *
   * This function will open the GitHub authentication page in the user's browser. The user will
   * be asked to log in to their GitHub account, and then to grant access to the application.
   * After the user has granted access, the browser will be redirected to the application. */
  signInWithGitHub() {
    this.isSignedIn = true
    listen.authEventListener?.(listen.AuthEvent.signIn)
    return Promise.resolve({
      accessKeyId: 'access key id',
      sessionToken: 'session token',
      secretAccessKey: 'secret access key',
      identityId: 'identity id',
      authenticated: true,
      expiration: new Date(Number(new Date()) + DAY_MS),
    })
  }

  /** Sign in with the given username and password.
   *
   * Does not rely on external identity providers (e.g., Google or GitHub). */
  async signInWithPassword(username: string, _password: string) {
    this.isSignedIn = true
    mockEmail = username
    localStorage.setItem(MOCK_EMAIL_KEY, username)
    const result = await results.Result.wrapAsync(async () => {
      listen.authEventListener?.(listen.AuthEvent.signIn)
      await Promise.resolve()
    })
    return result
      .mapErr(original.intoAmplifyErrorOrThrow)
      .mapErr(original.intoSignInWithPasswordErrorOrThrow)
  }

  /** Sign out the current user. */
  async signOut() {
    listen.authEventListener?.(listen.AuthEvent.signOut)
    this.isSignedIn = false
    return Promise.resolve(null)
  }

  /** Send a password reset email.
   *
   * The user will be able to reset their password by following the link in the email, which takes
   * them to the "reset password" page of the application. The verification code will be filled in
   * automatically. */
  async forgotPassword(_email: string) {
    const result = await results.Result.wrapAsync(async () => {
      // Ignored.
    })
    return result
      .mapErr(original.intoAmplifyErrorOrThrow)
      .mapErr(original.intoForgotPasswordErrorOrThrow)
  }

  /** Submit a new password for the given email address.
   *
   * The user will have received a verification code in an email, which they will have entered on
   * the "reset password" page of the application. This function will submit the new password
   * along with the verification code, changing the user's password. */
  async forgotPasswordSubmit(_email: string, _code: string, _password: string) {
    const result = await results.Result.wrapAsync(async () => {
      // Ignored.
    })
    return result.mapErr(original.intoForgotPasswordSubmitErrorOrThrow)
  }

  /** Change a password for current authenticated user.
   *
   * Allow users to independently modify their passwords. The user needs to provide the old
   * password, new password, and repeat new password to change their old password to the new
   * one. The validation of the repeated new password is handled by the `changePasswordModel`
   * component. */
  async changePassword(_oldPassword: string, _newPassword: string) {
    const cognitoUserResult = await currentAuthenticatedUser()
    if (cognitoUserResult.ok) {
      const result = await results.Result.wrapAsync(async () => {
        // Ignored.
      })
      return result.mapErr(original.intoAmplifyErrorOrThrow)
    } else {
      return results.Err(cognitoUserResult.val)
    }
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

/** Parse a {@link cognito.CognitoUserSession} into a {@link UserSession}.
 * @throws If the `email` field of the payload is not a string. */
function parseUserSession(session: cognito.CognitoUserSession): UserSession {
  const payload: Record<string, unknown> = session.getIdToken().payload
  const email = payload.email
  /** The `email` field is mandatory, so we assert that it exists and is a string. */
  if (typeof email !== 'string') {
    throw new Error('Payload does not have an email field.')
  } else {
    const accessToken = `.${window.btoa(JSON.stringify({ username: email }))}.`
    return { email, accessToken }
  }
}

// ==============
// === SignUp ===
// ==============

/** A wrapper around the Amplify "sign up" endpoint that converts known errors
 * to `SignUpError`s. */
async function signUp(
  _supportsDeepLinks: boolean,
  _username: string,
  _password: string,
  _organizationId: string | null
) {
  const result = await results.Result.wrapAsync(async () => {
    // Ignored.
  })
  return result.mapErr(original.intoAmplifyErrorOrThrow).mapErr(original.intoSignUpErrorOrThrow)
}

// =====================
// === ConfirmSignUp ===
// =====================

/** A wrapper around the Amplify "confirm sign up" endpoint that converts known errors
 * to `ConfirmSignUpError`s. */
async function confirmSignUp(_email: string, _code: string) {
  return results.Result.wrapAsync(async () => {
    // Ignored.
  }).then(result =>
    result.mapErr(original.intoAmplifyErrorOrThrow).mapErr(original.intoConfirmSignUpErrorOrThrow)
  )
}

// ======================
// === ChangePassword ===
// ======================

/** A wrapper around the Amplify "current authenticated user" endpoint that converts known errors
 * to `AmplifyError`s. */
async function currentAuthenticatedUser() {
  const result = await results.Result.wrapAsync(
    // The methods are not needed.
    // eslint-disable-next-line no-restricted-syntax
    async () => await Promise.resolve<amplify.CognitoUser>({} as unknown as amplify.CognitoUser)
  )
  return result.mapErr(original.intoAmplifyErrorOrThrow)
}
