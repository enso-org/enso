/**
 * @file Provides {@link Cognito} class which is the entrypoint into the AWS Amplify library.
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
 * `internalCode`, when rethrowing the error.
 */
import * as amplify from '@aws-amplify/auth'
import * as cognito from 'amazon-cognito-identity-js'
import * as results from 'ts-results'

import * as detect from 'enso-common/src/detect'

import type * as loggerProvider from '#/providers/LoggerProvider'

import * as dateTime from '#/utilities/dateTime'
import type * as saveAccessToken from 'enso-common/src/accessToken'

import * as service from '#/authentication/service'

// =================
// === Constants ===
// =================

/**
 * String used to identify the GitHub federated identity provider in AWS Amplify.
 *
 * This provider alone requires a string because it is not a standard provider, and thus has no
 * constant defined in the AWS Amplify library.
 */
const GITHUB_PROVIDER = 'Github'
/** One second, in milliseconds. */
const SEC_MS = 1_000

// ================
// === UserInfo ===
// ================

// The names come from a third-party API and cannot be changed.
/* eslint-disable @typescript-eslint/naming-convention */
/** Attributes returned from {@link amplify.Auth.currentUserInfo}. */
interface UserAttributes {
  readonly email: string
  readonly email_verified: boolean
  readonly sub: string
  readonly 'custom:fromDesktop'?: string
  readonly 'custom:organizationId'?: string
}
/* eslint-enable @typescript-eslint/naming-convention */

/** The type of multi-factor authentication (MFA) that the user has set up. */
export type MfaType = 'NOMFA' | 'SMS_MFA' | 'SOFTWARE_TOKEN_MFA' | 'TOTP'

/**
 * The type of challenge that the user is currently facing after signing in.
 *
 * The `NO_CHALLENGE` value is used when the user is not currently facing any challenge.
 */
export type UserSessionChallenge = cognito.ChallengeName | 'NO_CHALLENGE'

/** User information returned from {@link amplify.Auth.currentUserInfo}. */
interface UserInfo {
  readonly username: string
  readonly id: undefined
  readonly attributes: UserAttributes
}

// ====================
// === AmplifyError ===
// ====================

/**
 * Error thrown by the AWS Amplify library when an Amplify error occurs.
 *
 * Some Amplify errors (e.g., network connectivity errors) can not be resolved within the
 * application. Un-resolvable errors are allowed to flow up to the top-level error handler. Errors
 * that can be resolved must be caught and handled as early as possible.
 *
 * # Handling Amplify Errors
 *
 * Use the {@link isAmplifyError} function to check if an `unknown` error is an
 * {@link AmplifyError}. If it is, use the {@link intoAmplifyErrorOrThrow} function to convert it
 * from `unknown` to a typed object. Then, use one of the response error handling functions  (e.g.
 * {@link intoSignUpErrorOrThrow}) to see if the error is one that must be handled by the
 * application (i.e., it is an error that is relevant to our business logic).
 */
export interface AmplifyError extends Error {
  /** Error code for disambiguating the error. */
  readonly code: string
}

/** Hint to TypeScript if we can safely cast an `unknown` error to an {@link AmplifyError}. */
function isAmplifyError(error: unknown): error is AmplifyError {
  if (error != null && typeof error === 'object') {
    return 'code' in error && 'message' in error && 'name' in error
  } else {
    return false
  }
}

/**
 * Convert the `unknown` error into an {@link AmplifyError} and returns it, or re-throws it if
 * conversion is not possible.
 * @throws If the error is not an amplify error.
 */
export function intoAmplifyErrorOrThrow(error: unknown): AmplifyError {
  if (isAmplifyError(error)) {
    return error
  } else {
    throw error
  }
}

// =================
// === AuthError ===
// =================

/** Object returned by the AWS Amplify library when an auth error occurs. */
interface AuthError {
  readonly name: string
  readonly log: string
}

/** Hint to TypeScript if we can safely cast an `unknown` error to an `AuthError`. */
function isAuthError(error: unknown): error is AuthError {
  if (error != null && typeof error === 'object') {
    return 'name' in error && 'log' in error
  } else {
    return false
  }
}

// ====================
// === CognitoError ===
// ====================

/** Internal IDs of Cognito errors that may occur when requesting a password reset. */
export enum CognitoErrorType {
  userAlreadyConfirmed = 'UserAlreadyConfirmed',
  usernameExists = 'UsernameExists',
  invalidParameter = 'InvalidParameter',
  invalidPassword = 'InvalidPassword',
  notAuthorized = 'NotAuthorized',
  userNotConfirmed = 'UserNotConfirmed',
  userNotFound = 'UserNotFound',
  userBrokenState = 'UserBrokenState',
  amplifyError = 'AmplifyError',
  authError = 'AuthError',
  noCurrentUser = 'NoCurrentUser',
}

/**
 * Base interface for all errors output from this module.
 * Every user-facing error MUST extend this interface.
 */
interface CognitoError {
  readonly type: CognitoErrorType
  readonly message: string
}

// ===============
// === Cognito ===
// ===============

/**
 * Thin wrapper around Cognito endpoints from the AWS Amplify library with error handling added.
 * This way, the methods don't throw all errors, but define exactly which errors they return.
 * The caller can then handle them via pattern matching on the {@link results.Result} type.
 */
export class Cognito {
  /** Create a new Cognito wrapper. */
  constructor(
    private readonly logger: loggerProvider.Logger,
    private readonly supportsDeepLinks: boolean,
    private readonly amplifyConfig: service.AmplifyConfig,
  ) {
    /**
     * Amplify expects `Auth.configure` to be called before any other `Auth` methods are
     * called. By wrapping all the `Auth` methods we care about and returning an `Cognito` API
     * object containing them, we ensure that `Auth.configure` is called before any other `Auth`
     * methods are called.
     */
    const nestedAmplifyConfig = service.toNestedAmplifyConfig(amplifyConfig)
    amplify.Auth.configure(nestedAmplifyConfig)
  }

  /** Save the access token to a file for further reuse. */
  saveAccessToken(accessTokenPayload: saveAccessToken.AccessToken | null) {
    this.amplifyConfig.saveAccessToken?.(accessTokenPayload)
  }

  /**
   * Return the current {@link UserSession}, or `None` if the user is not logged in.
   *
   * Will refresh the {@link UserSession} if it has expired.
   */
  async userSession() {
    const currentSession = await results.Result.wrapAsync(() => amplify.Auth.currentSession())
    const amplifySession = currentSession.mapErr(intoCurrentSessionErrorType)

    return amplifySession
      .map((session) => parseUserSession(session, this.amplifyConfig.userPoolWebClientId))
      .unwrapOr(null)
  }

  /**
   * Returns the associated organization ID of the current user, which is passed during signup,
   * or `null` if the user is not associated with an existing organization.
   */
  async organizationId() {
    // This `any` comes from a third-party API and cannot be avoided.
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const userInfo: UserInfo = await amplify.Auth.currentUserInfo()
    return userInfo.attributes['custom:organizationId'] ?? null
  }

  /** Gets user email from cognito */
  async email() {
    // This `any` comes from a third-party API and cannot be avoided.
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const userInfo: UserInfo = await amplify.Auth.currentUserInfo()
    return userInfo.attributes.email
  }

  /**
   * Sign up with username and password.
   *
   * Does not rely on federated identity providers (e.g., Google or GitHub).
   */
  async signUp(username: string, password: string, organizationId: string | null) {
    const result = await results.Result.wrapAsync(async () => {
      const params = intoSignUpParams(this.supportsDeepLinks, username, password, organizationId)
      await amplify.Auth.signUp(params)
    })
    return result.mapErr(intoAmplifyErrorOrThrow).mapErr(intoSignUpErrorOrThrow)
  }

  /**
   * Send the email address verification code.
   *
   * The user will receive a link in their email. The user must click the link to go to the email
   * verification page. The email verification page will parse the verification code from the URL.
   * If the verification code matches, the email address is marked as verified. Once the email
   * address is verified, the user can sign in.
   */
  async confirmSignUp(email: string, code: string) {
    const result = await results.Result.wrapAsync(async () => {
      await amplify.Auth.confirmSignUp(email, code)
    })
    return result.mapErr(intoAmplifyErrorOrThrow).mapErr(intoConfirmSignUpErrorOrThrow)
  }

  /**
   * Sign in via the Google federated identity provider.
   *
   * This function will open the Google authentication page in the user's browser. The user will
   * be asked to log in to their Google account, and then to grant access to the application.
   * After the user has granted access, the browser will be redirected to the application.
   */
  async signInWithGoogle() {
    const customState = this.customState()
    const provider = amplify.CognitoHostedUIIdentityProvider.Google
    await amplify.Auth.federatedSignIn({
      provider,
      ...(customState != null ? { customState } : {}),
    })
  }

  /**
   * Sign in via the GitHub federated identity provider.
   *
   * This function will open the GitHub authentication page in the user's browser. The user will
   * be asked to log in to their GitHub account, and then to grant access to the application.
   * After the user has granted access, the browser will be redirected to the application.
   */
  async signInWithGitHub() {
    await amplify.Auth.federatedSignIn({
      customProvider: GITHUB_PROVIDER,
    })
  }

  /**
   * Sign in with the given username and password.
   *
   * Does not rely on external identity providers (e.g., Google or GitHub).
   */
  async signInWithPassword(username: string, password: string) {
    const result = await results.Result.wrapAsync(async () => {
      // This `any` comes from a third-party API and cannot be avoided.
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
      const maybeUser = await amplify.Auth.signIn(username, password)

      if (maybeUser instanceof cognito.CognitoUser) {
        return maybeUser
      } else {
        // eslint-disable-next-line no-restricted-properties
        console.error(
          'Unknown result from signIn, expected CognitoUser, got ' + typeof maybeUser,
          JSON.stringify(maybeUser),
        )
        throw new Error('Unknown response from the server, please try again later ')
      }
    })

    return result.mapErr(intoAmplifyErrorOrThrow).mapErr(intoSignInWithPasswordErrorOrThrow)
  }

  /** Refresh the current user session. */
  async refreshUserSession() {
    const result = await results.Result.wrapAsync(async () => {
      const currentUser = await currentAuthenticatedUser()
      const refreshToken = (await amplify.Auth.currentSession()).getRefreshToken()

      return await new Promise<cognito.CognitoUserSession>((resolve, reject) => {
        currentUser
          .unwrap()
          .refreshSession(refreshToken, (error, session: cognito.CognitoUserSession) => {
            if (error instanceof Error) {
              reject(error)
            } else {
              resolve(session)
            }
          })
      })
    })

    return result
      .map((session) => parseUserSession(session, this.amplifyConfig.userPoolWebClientId))
      .unwrapOr(null)
  }

  /** Sign out the current user. */
  async signOut() {
    // FIXME [NP]: https://github.com/enso-org/cloud-v2/issues/341
    // For some reason, the redirect back to the IDE from the browser doesn't work correctly so this
    // `await` throws a timeout error. As a workaround, we catch this error and force a refresh of
    // the session manually by running the `signOut` again. This works because Amplify will see that
    // we've already signed out and clear the cache accordingly. Ideally we should figure out how
    // to fix the redirect and remove this `catch`. This has the unintended consequence of catching
    // any other errors that might occur during sign out, that we really shouldn't be catching. This
    // also has the unintended consequence of delaying the sign out process by a few seconds (until
    // the timeout occurs).
    try {
      await amplify.Auth.signOut()
    } catch (error) {
      this.logger.error('Sign out failed', error)
    } finally {
      await amplify.Auth.signOut()
    }
  }

  /**
   * Send a password reset email.
   *
   * The user will be able to reset their password by following the link in the email, which takes
   * them to the "reset password" page of the application. The verification code will be filled in
   * automatically.
   */
  async forgotPassword(email: string) {
    const result = await results.Result.wrapAsync(async () => {
      await amplify.Auth.forgotPassword(email)
    })
    return result.mapErr(intoAmplifyErrorOrThrow).mapErr(intoForgotPasswordErrorOrThrow)
  }

  /**
   * Submit a new password for the given email address.
   *
   * The user will have received a verification code in an email, which they will have entered on
   * the "reset password" page of the application. This function will submit the new password
   * along with the verification code, changing the user's password.
   */
  async forgotPasswordSubmit(email: string, code: string, password: string) {
    const result = await results.Result.wrapAsync(async () => {
      await amplify.Auth.forgotPasswordSubmit(email, code, password)
    })
    return result.mapErr(intoForgotPasswordSubmitErrorOrThrow)
  }

  /**
   * Change a password for current authenticated user.
   *
   * Allow users to independently modify their passwords. The user needs to provide the old
   * password, new password, and repeat new password to change their old password to the new
   * one. The validation of the repeated new password is handled by the `changePasswordModel`
   * component.
   */
  async changePassword(oldPassword: string, newPassword: string) {
    const cognitoUserResult = await currentAuthenticatedUser()
    if (cognitoUserResult.ok) {
      const cognitoUser = cognitoUserResult.unwrap()
      const result = await results.Result.wrapAsync(async () => {
        await amplify.Auth.changePassword(cognitoUser, oldPassword, newPassword)
      })
      return result.mapErr(intoAmplifyErrorOrThrow)
    } else {
      return results.Err(cognitoUserResult.val)
    }
  }

  /** Start the TOTP setup process. Returns the secret and the URL to scan the QR code. */
  async setupTOTP() {
    const email = await this.email()
    const cognitoUserResult = await currentAuthenticatedUser()
    if (cognitoUserResult.ok) {
      const cognitoUser = cognitoUserResult.unwrap()

      const result = (
        await results.Result.wrapAsync(() => amplify.Auth.setupTOTP(cognitoUser))
      ).map((data) => {
        const str = 'otpauth://totp/AWSCognito:' + email + '?secret=' + data + '&issuer=' + 'Enso'

        return { secret: data, url: str } as const
      })

      return result.mapErr(intoAmplifyErrorOrThrow)
    } else {
      return results.Err(cognitoUserResult.val)
    }
  }

  /**
   * Verify the TOTP token during the setup process.
   * Use it *only* during the setup process.
   */
  async verifyTotpSetup(totpToken: string) {
    const cognitoUserResult = await currentAuthenticatedUser()
    if (cognitoUserResult.ok) {
      const cognitoUser = cognitoUserResult.unwrap()
      const result = await results.Result.wrapAsync(async () => {
        await amplify.Auth.verifyTotpToken(cognitoUser, totpToken)
      })
      return result.mapErr(intoAmplifyErrorOrThrow)
    } else {
      return results.Err(cognitoUserResult.val)
    }
  }

  /** Set the user's preferred MFA method. */
  async updateMFAPreference(mfaMethod: MfaType) {
    const cognitoUserResult = await currentAuthenticatedUser()
    if (cognitoUserResult.ok) {
      const cognitoUser = cognitoUserResult.unwrap()
      const result = await results.Result.wrapAsync(
        async () => await amplify.Auth.setPreferredMFA(cognitoUser, mfaMethod),
      )
      return result.mapErr(intoAmplifyErrorOrThrow)
    } else {
      return results.Err(cognitoUserResult.val)
    }
  }

  /** Get the user's preferred MFA method. */
  async getMFAPreference() {
    const cognitoUserResult = await currentAuthenticatedUser()
    if (cognitoUserResult.ok) {
      const cognitoUser = cognitoUserResult.unwrap()
      const result = await results.Result.wrapAsync(async () => {
        // eslint-disable-next-line no-restricted-syntax
        return (await amplify.Auth.getPreferredMFA(cognitoUser)) as MfaType
      })
      return result.mapErr(intoAmplifyErrorOrThrow)
    } else {
      return results.Err(cognitoUserResult.val)
    }
  }

  /**
   * Verify the TOTP token.
   * Returns the user session if the token is valid.
   */
  async verifyTotpToken(totpToken: string) {
    const cognitoUserResult = await currentAuthenticatedUser()

    if (cognitoUserResult.ok) {
      const cognitoUser = cognitoUserResult.unwrap()

      return (
        await results.Result.wrapAsync(() => amplify.Auth.verifyTotpToken(cognitoUser, totpToken))
      ).mapErr(intoAmplifyErrorOrThrow)
    } else {
      return results.Err(cognitoUserResult.val)
    }
  }

  /** Confirm the sign in with the MFA token. */
  async confirmSignIn(
    user: amplify.CognitoUser,
    confirmationCode: string,
    mfaType: 'SMS_MFA' | 'SOFTWARE_TOKEN_MFA',
  ) {
    const result = await results.Result.wrapAsync(() =>
      amplify.Auth.confirmSignIn(user, confirmationCode, mfaType),
    )

    return result.mapErr(intoAmplifyErrorOrThrow)
  }

  /**
   * We want to signal to Amplify to fire a "custom state change" event when the user is
   * redirected back to the application after signing in via an external identity provider. This
   * is done so we get a chance to fix the location history. The location history is the history
   * of the pages visited within the application. Amplify messes up the history when it redirects
   * the user to the identity provider's authentication page. This is because Amplify believes
   * that we are in the browser, so the location needs to be modified to account for leaving the
   * page and coming back. However, in the Electron app we never leave the page. The rest of the
   * flow is handled in the system browser instead. So we must undo the changes that Amplify
   * makes.
   *
   * In order to do so, we need to pass custom state along for the entire OAuth flow, which is
   * obtained by calling this function. This function will return the current location path if
   * the user is signing in from the desktop application, and `null` otherwise.
   *
   * We use `null` outside of the desktop application because Amplify only messes up the
   * location history in the desktop application.
   *
   * See: https://github.com/aws-amplify/amplify-js/issues/3391#issuecomment-756473970
   */
  private customState() {
    return detect.isOnElectron() ? window.location.pathname : null
  }
}

// ===================
// === UserSession ===
// ===================

/** User's session, provides information for identifying and authenticating the user. */
export interface UserSession {
  /**
   * User's email address, used to uniquely identify the user.
   *
   * Provided by the identity provider the user used to log in. One of:
   *
   * - GitHub,
   * - Google, or
   * - Email.
   */
  readonly email: string
  /** User's access token, used to authenticate the user (e.g., when making API calls). */
  readonly accessToken: string
  /** User's refresh token, used to refresh the access token when it expires. */
  readonly refreshToken: string
  /** URL to refresh the access token. */
  readonly refreshUrl: string
  /** Time when the access token will expire, date and time in ISO 8601 format (UTC timezone). */
  readonly expireAt: dateTime.Rfc3339DateTime
  /** Cognito app integration client ID. */
  readonly clientId: string
}

/**
 * Parse a `CognitoUserSession` into a {@link UserSession}.
 * @throws If the `email` field of the payload is not a string.
 */
function parseUserSession(session: cognito.CognitoUserSession, clientId: string): UserSession {
  const payload: Readonly<Record<string, unknown>> = session.getIdToken().payload
  const email = payload.email
  const refreshUrl = extractRefreshUrlFromSession(session)
  /** The `email` field is mandatory, so we assert that it exists and is a string. */
  if (typeof email !== 'string') {
    throw new Error('Payload does not have an email field.')
  } else {
    const expirationTimestamp = session.getAccessToken().getExpiration()

    const expireAt = dateTime.toRfc3339(new Date(expirationTimestamp * SEC_MS))

    return {
      email,
      clientId,
      expireAt,
      refreshUrl,
      accessToken: session.getAccessToken().getJwtToken(),
      refreshToken: session.getRefreshToken().getToken(),
    }
  }
}

/**
 * Extract the refresh session endpoint URL from the JWT token payload
 * @see https://docs.aws.amazon.com/cognito/latest/developerguide/amazon-cognito-user-pools-using-the-access-token.html
 * @throws Error if the `iss` field of the payload is not a valid URL.
 */
function extractRefreshUrlFromSession(session: cognito.CognitoUserSession): string {
  const { iss } = session.getAccessToken().payload

  if (typeof iss !== 'string') {
    throw new Error('Payload does not have an `iss` field.')
  } else {
    try {
      return new URL(iss).toString()
    } catch {
      throw new Error('`iss` field is not a valid URL')
    }
  }
}

/**
 * Convert an {@link AmplifyError} into a {@link CognitoErrorType} if it is a known error,
 * else re-throws the error.
 * @throws {Error} If the error is not recognized.
 */
export function intoCurrentSessionErrorType(error: unknown): CognitoErrorType.noCurrentUser {
  if (error === 'No current user') {
    return CognitoErrorType.noCurrentUser
  } else {
    throw error
  }
}

// ==============
// === SignUp ===
// ==============

/** Format a username and password as an {@link amplify.SignUpParams}. */
function intoSignUpParams(
  supportsDeepLinks: boolean,
  username: string,
  password: string,
  organizationId: string | null,
): amplify.SignUpParams {
  return {
    username,
    password,
    attributes: {
      email: username,
      /**
       * Add a custom attribute indicating whether the user is signing up from the desktop.
       * This is used to determine the schema used in the callback links sent in the
       * verification emails. For example, `http://` for the Cloud, and `enso://` for the
       * desktop.
       *
       * # Naming Convention
       *
       * It is necessary to disable the naming convention rule here, because the key is
       * expected to appear exactly as-is in Cognito, so we must match it.
       */
      // eslint-disable-next-line @typescript-eslint/naming-convention
      ...(supportsDeepLinks ? { 'custom:fromDesktop': JSON.stringify(true) } : {}),
      // eslint-disable-next-line @typescript-eslint/naming-convention
      ...(organizationId != null ? { 'custom:organizationId': organizationId } : {}),
    },
  }
}

/** An error that may occur when signing up. */
export interface SignUpError extends CognitoError {
  readonly type:
    | CognitoErrorType.invalidParameter
    | CognitoErrorType.invalidPassword
    | CognitoErrorType.usernameExists
  readonly message: string
}

/**
 * Convert an {@link AmplifyError} into a {@link SignUpError} if it is a known error,
 * else re-throws the error.
 * @throws {Error} If the error is not recognized.
 */
export function intoSignUpErrorOrThrow(error: AmplifyError): SignUpError {
  if (error.code === 'UsernameExistsException') {
    return {
      type: CognitoErrorType.usernameExists,
      message: error.message,
    }
  } else if (error.code === 'InvalidParameterException') {
    return {
      type: CognitoErrorType.invalidParameter,
      message: error.message,
    }
  } else if (error.code === 'InvalidPasswordException') {
    return {
      type: CognitoErrorType.invalidPassword,
      message: error.message,
    }
  } else {
    throw error
  }
}

// =====================
// === ConfirmSignUp ===
// =====================

/** An error that may occur when confirming registration. */
export interface ConfirmSignUpError extends CognitoError {
  readonly type: CognitoErrorType.userAlreadyConfirmed | CognitoErrorType.userNotFound
  readonly message: string
}

/**
 * Convert an {@link AmplifyError} into a {@link ConfirmSignUpError} if it is a known error,
 * else re-throws the error.
 * @throws {Error} If the error is not recognized.
 */
export function intoConfirmSignUpErrorOrThrow(error: AmplifyError): ConfirmSignUpError {
  if (
    error.code === 'NotAuthorizedException' &&
    error.message === 'User cannot be confirmed. Current status is CONFIRMED'
  ) {
    return {
      /**
       * Don't re-use the original `error.code` here because Amplify overloads the same code
       * for multiple kinds of errors. We replace it with a custom code that has no
       * ambiguity.
       */
      type: CognitoErrorType.userAlreadyConfirmed,
      message: error.message,
    }
  } else if (
    error.code === 'UserNotFoundException' &&
    error.message === 'Username/client id combination not found.'
  ) {
    return {
      /**
       * Don't re-use the original `error.code` here because Amplify overloads the same code
       * for multiple kinds of errors. We replace it with a custom code that has no
       * ambiguity.
       */
      type: CognitoErrorType.userNotFound,
      message: 'Incorrect email or confirmation code.',
    }
  } else {
    throw error
  }
}

// ==========================
// === SignInWithPassword ===
// ==========================

/** An error that may occur when signing in with a password. */
export interface SignInWithPasswordError extends CognitoError {
  readonly type:
    | CognitoErrorType.notAuthorized
    | CognitoErrorType.userNotConfirmed
    | CognitoErrorType.userNotFound
  readonly message: string
}

/**
 * Convert an {@link AmplifyError} into a {@link SignInWithPasswordError} if it is a known error,
 * else re-throws the error.
 * @throws {Error} If the error is not recognized.
 */
export function intoSignInWithPasswordErrorOrThrow(error: AmplifyError): SignInWithPasswordError {
  switch (error.code) {
    case 'UserNotFoundException':
      return {
        type: CognitoErrorType.userNotFound,
        message: 'User not found. Please sign up first.',
      }
    case 'UserNotConfirmedException':
      return {
        type: CognitoErrorType.userNotConfirmed,
        message: 'User not confirmed. Please check your email for a confirmation link.',
      }
    case 'NotAuthorizedException':
      return {
        type: CognitoErrorType.notAuthorized,
        message: 'Incorrect username or password.',
      }
    default:
      throw error
  }
}

// ======================
// === ForgotPassword ===
// ======================

/** An error that may occur when requesting a password reset. */
export interface ForgotPasswordError extends CognitoError {
  readonly type:
    | CognitoErrorType.userBrokenState
    | CognitoErrorType.userNotConfirmed
    | CognitoErrorType.userNotFound
  readonly message: string
}

/**
 * Convert an {@link AmplifyError} into a {@link ForgotPasswordError} if it is a known error,
 * else re-throws the error.
 * @throws {Error} If the error is not recognized.
 */
export function intoForgotPasswordErrorOrThrow(error: AmplifyError): ForgotPasswordError {
  if (error.code === 'UserNotFoundException') {
    return {
      type: CognitoErrorType.userNotFound,
      message: 'Cannot reset password as user not found.',
    }
  } else if (
    error.code === 'InvalidParameterException' &&
    error.message ===
      'Cannot reset password for the user as there is no registered/verified email or ' +
        'phone_number'
  ) {
    return {
      type: CognitoErrorType.userNotConfirmed,
      message:
        'Cannot reset password for user with an unverified email. ' +
        'Please verify your email first.',
    }
  } else if (
    error.code === 'NotAuthorizedException' &&
    error.message === 'User password cannot be reset in the current state.'
  ) {
    return {
      type: CognitoErrorType.userBrokenState,
      message: 'User account is in a broken state. Please contact support.',
    }
  } else {
    throw error
  }
}

// ============================
// === ForgotPasswordSubmit ===
// ============================

/** An error that may occur when resetting a password. */
export interface ForgotPasswordSubmitError extends CognitoError {
  readonly type: CognitoErrorType.amplifyError | CognitoErrorType.authError
  readonly message: string
}

/**
 * Convert an {@link AmplifyError} into a {@link ForgotPasswordSubmitError}
 * if it is a known error, else re-throws the error.
 * @throws {Error} If the error is not recognized.
 */
export function intoForgotPasswordSubmitErrorOrThrow(error: unknown): ForgotPasswordSubmitError {
  if (isAuthError(error)) {
    return {
      type: CognitoErrorType.authError,
      message: error.log,
    }
  } else if (isAmplifyError(error)) {
    return {
      type: CognitoErrorType.amplifyError,
      message: error.message,
    }
  } else {
    throw error
  }
}

// ======================
// === ChangePassword ===
// ======================

/**
 * A wrapper around the Amplify "current authenticated user" endpoint that converts known errors
 * to {@link AmplifyError}s.
 */
async function currentAuthenticatedUser() {
  const result = await results.Result.wrapAsync(
    /**
     * The interface provided by Amplify declares that the return type is
     * `Promise<CognitoUser | any>`, but TypeScript automatically converts it to `Promise<any>`.
     * Therefore, it is necessary to use `as` to narrow down the type to
     * `Promise<CognitoUser>`.
     */
    // eslint-disable-next-line no-restricted-syntax
    () => amplify.Auth.currentAuthenticatedUser() as Promise<amplify.CognitoUser>,
  )
  return result.mapErr(intoAmplifyErrorOrThrow)
}
export { CognitoUser } from '@aws-amplify/auth'
