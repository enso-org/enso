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
import * as cognito from 'amazon-cognito-identity-js'
import { Amplify } from 'aws-amplify'
import * as amplify from 'aws-amplify/auth'
import * as results from 'ts-results'

import * as detect from 'enso-common/src/utilities/detect'

import type * as loggerProvider from '#/providers/LoggerProvider'

import type * as saveAccessToken from 'enso-common/src/accessToken'
import * as dateTime from 'enso-common/src/utilities/data/dateTime'

import * as service from '$/authentication/service'
import { cognitoUserPoolsTokenProvider } from 'aws-amplify/auth/cognito'

/**
 * String used to identify the GitHub federated identity provider in AWS Amplify.
 *
 * This provider alone requires a string because it is not a standard provider, and thus has no
 * constant defined in the AWS Amplify library.
 */
const GITHUB_PROVIDER = 'Github'
/**
 * String used to identify the Microsoft federated identity provider in AWS Amplify.
 *
 * This provider alone requires a string because it is not a standard provider, and thus has no
 * constant defined in the AWS Amplify library.
 */
const MICROSOFT_PROVIDER = 'Microsoft'
/** One second, in milliseconds. */
const SEC_MS = 1_000

// The names come from a third-party API and cannot be changed.
/** Typed attributes returned from {@link amplify.Auth.fetchUserAttributes}. */
interface UserAttributes extends Partial<Record<amplify.UserAttributeKey, string>> {
  readonly email: string
  readonly email_verified: 'true' | 'false'
  readonly sub: string
  readonly 'custom:fromDesktop'?: string
  readonly 'custom:organizationId'?: string
}

function assertValidAttributes(
  attrs: Partial<Record<amplify.UserAttributeKey, string>>,
): asserts attrs is UserAttributes {
  if (attrs.email == null) throw new Error('No email in User Attributes')
  if (attrs.sub == null) throw new Error('No sub in User Attributes')
  if (attrs.email_verified !== 'true' && attrs.email_verified !== 'false')
    throw new Error('Invalid email_verified field in User Attributes')
}

/** The type of multi-factor authentication (MFA) including non-specified MFA */
export type MfaType = MfaProtectionTypes | 'NOMFA'
/**
 * MFA protection types that the user can set up.
 */
export type MfaProtectionTypes = 'SMS_MFA' | 'TOTP'

/**
 * The type of challenge that the user is currently facing after signing in.
 *
 * The `NO_CHALLENGE` value is used when the user is not currently facing any challenge.
 */
export type UserSessionChallenge = cognito.ChallengeName | 'NO_CHALLENGE'

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

/**
 * Return type for Confirm sign up endpoint
 */
export type ConfirmSignInReturn = Promise<
  results.Err<AmplifyError> | results.Ok<amplify.ConfirmSignInOutput>
>

/**
 * Return type for Setup TOTP endpoint
 */
export interface SetupTOTPReturn {
  /** The URL to scan the QR code */
  readonly url: string
  /** The secret to use for the TOTP */
  readonly secret: string
}

/**
 * Interface that represents Auth Provider API
 * Currently, it's tightly coupled with Cognito, but in the future, it should be decoupled from
 * Cognito and be able to be used with other Auth Providers.
 *
 * Currently used in unit tests to mock the Auth Provider API
 */
export interface ISessionProvider {
  readonly userSession: () => Promise<UserSession | null>
  readonly organizationId: () => Promise<string | null>
  readonly email: () => Promise<string>
  readonly signUp: (
    username: string,
    password: string,
    organizationId: string | null,
  ) => Promise<results.Err<SignUpError> | results.Ok<unknown>>
  readonly confirmSignUp: (
    email: string,
    code: string,
  ) => Promise<results.Err<ConfirmSignUpError> | results.Ok<unknown>>
  readonly signInWithApple: () => Promise<void>
  readonly signInWithGoogle: () => Promise<void>
  readonly signInWithGitHub: () => Promise<void>
  readonly signInWithMicrosoft: () => Promise<void>
  readonly signInWithPassword: (
    username: string,
    password: string,
  ) => Promise<results.Err<SignInWithPasswordError> | results.Ok<amplify.SignInOutput>>
  readonly refreshUserSession: () => Promise<UserSession | null>
  readonly signOut: () => Promise<void>
  readonly forgotPassword: (
    email: string,
  ) => Promise<results.Err<ForgotPasswordError> | results.Ok<unknown>>
  readonly forgotPasswordSubmit: (
    email: string,
    code: string,
    password: string,
  ) => Promise<results.Err<ForgotPasswordSubmitError> | results.Ok<unknown>>
  readonly changePassword: (
    oldPassword: string,
    newPassword: string,
  ) => Promise<results.Err<AmplifyError> | results.Ok<unknown>>
  readonly resendSignUp: (username: string) => Promise<void>
  readonly setupTOTP: () => Promise<results.Err<AmplifyError> | results.Ok<SetupTOTPReturn>>
  readonly verifyTotpSetup: (
    totpToken: string,
  ) => Promise<results.Err<AmplifyError> | results.Ok<unknown>>
  readonly updateMFAPreference: (
    mfaMethod: MfaType,
  ) => Promise<results.Err<AmplifyError> | results.Ok<void>>
  readonly getMFAPreference: () => Promise<results.Err<AmplifyError> | results.Ok<MfaType>>
  readonly verifyTotpToken: (
    totpToken: string,
  ) => Promise<results.Err<AmplifyError> | results.Ok<boolean>>
  readonly saveAccessToken: (accessTokenPayload: saveAccessToken.AccessToken | null) => void
  readonly confirmSignIn: (challengeResponse: string) => ConfirmSignInReturn
}

/**
 * Thin wrapper around Cognito endpoints from the AWS Amplify library with error handling added.
 * This way, the methods don't throw all errors, but define exactly which errors they return.
 * The caller can then handle them via pattern matching on the {@link results.Result} type.
 */
export class Cognito implements ISessionProvider {
  public resolveOngoingLogin: (
    result: Awaited<
      ReturnType<
        NonNullable<NonNullable<amplify.SignInWithRedirectInput['options']>['authSessionOpener']>
      >
    >,
  ) => void = () => {}

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
    Amplify.configure(nestedAmplifyConfig)
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
    return amplify
      .fetchAuthSession()
      .then((result) => parseUserSession(result, this.amplifyConfig.userPoolWebClientId))
      .catch(() => null)
  }

  /**
   * Returns the associated organization ID of the current user, which is passed during signup,
   * or `null` if the user is not associated with an existing organization.
   */
  async organizationId() {
    // This `any` comes from a third-party API and cannot be avoided.
    const attributes = await amplify.fetchUserAttributes()
    return attributes['custom:organizationId'] ?? null
  }

  /** Gets user email from cognito */
  async email() {
    // This `any` comes from a third-party API and cannot be avoided.
    const attributes = await amplify.fetchUserAttributes()
    assertValidAttributes(attributes)
    return attributes.email
  }

  /**
   * Sign up with username and password.
   *
   * Does not rely on federated identity providers (e.g., Google or GitHub).
   */
  async signUp(username: string, password: string, organizationId: string | null) {
    const result = await results.Result.wrapAsync(async () => {
      const params = intoSignUpParams(
        this.supportsDeepLinks,
        username.toLowerCase(),
        password,
        organizationId,
      )
      await amplify.signUp(params)
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
      await amplify.confirmSignUp({ username: email.toLowerCase(), confirmationCode: code })
    })
    return result.mapErr(intoAmplifyErrorOrThrow).mapErr(intoConfirmSignUpErrorOrThrow)
  }

  /**
   * Sign in via the Apple federated identity provider.
   *
   * This function will open the Apple authentication page in the user's browser. The user will
   * be asked to log in to their Apple ID account, and then to grant access to the application.
   * After the user has granted access, the browser will be redirected to the application.
   */
  async signInWithApple() {
    const customState = this.customState()
    const options = this.signInWithRedirectOptions()
    await amplify.signInWithRedirect({
      provider: 'Apple',
      ...(customState != null ? { customState } : {}),
      options,
    })
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
    const options = this.signInWithRedirectOptions()
    await amplify.signInWithRedirect({
      provider: 'Google',
      ...(customState != null ? { customState } : {}),
      options,
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
    const options = this.signInWithRedirectOptions()
    await amplify.signInWithRedirect({
      provider: { custom: GITHUB_PROVIDER },
      options,
    })
  }

  /**
   * Sign in via the Microsoft federated identity provider.
   *
   * This function will open the Microsoft authentication page in the user's browser. The user will
   * be asked to log in to their Microsoft account, and then to grant access to the application.
   * After the user has granted access, the browser will be redirected to the application.
   */
  async signInWithMicrosoft() {
    const options = this.signInWithRedirectOptions()
    await amplify.signInWithRedirect({
      provider: { custom: MICROSOFT_PROVIDER },
      options,
    })
  }

  private signInWithRedirectOptions(): NonNullable<amplify.SignInWithRedirectInput['options']> {
    const urlOpener = this.amplifyConfig.urlOpener
    if (!urlOpener) return {}
    return {
      authSessionOpener: (urlString) => {
        try {
          urlOpener(urlString)
          return new Promise((resolve) => (this.resolveOngoingLogin = resolve))
        } catch (error) {
          return Promise.resolve({
            error,
            type: 'error',
          })
        }
      },
    }
  }

  /**
   * Sign in with the given username and password.
   *
   * Does not rely on external identity providers (e.g., Google or GitHub).
   */
  async signInWithPassword(username: string, password: string) {
    const result = await results.Result.wrapAsync(() =>
      amplify.signIn({ username: username.toLowerCase(), password }),
    )

    return result.mapErr(intoAmplifyErrorOrThrow).mapErr(intoSignInWithPasswordErrorOrThrow)
  }

  /** Refresh the current user session. */
  async refreshUserSession() {
    const result = await results.Result.wrapAsync(async () =>
      amplify.fetchAuthSession({ forceRefresh: true }),
    )

    return result
      .map((session) => parseUserSession(session, this.amplifyConfig.userPoolWebClientId))
      .unwrapOr(null)
  }

  /** Sign out the current user. */
  async signOut() {
    await amplify.signOut({ global: false, oauth: { redirectUrl: window.location.origin } })
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
      await amplify.resetPassword({ username: email.toLowerCase() })
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
      await amplify.confirmResetPassword({
        username: email.toLowerCase(),
        confirmationCode: code,
        newPassword: password,
      })
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
    const result = await results.Result.wrapAsync(() =>
      amplify.updatePassword({ oldPassword, newPassword }),
    )
    return result.mapErr(intoAmplifyErrorOrThrow)
  }

  /** Resend the sign up confirmation code to the user's email address. */
  async resendSignUp(username: string) {
    await amplify.resendSignUpCode({ username })
  }

  /** Start the TOTP setup process. Returns the secret and the URL to scan the QR code. */
  async setupTOTP() {
    const email = await this.email()
    const result = (await results.Result.wrapAsync(() => amplify.setUpTOTP())).map((data) => {
      const str = data.getSetupUri('Enso', email)

      return { secret: data.sharedSecret, url: str.toString() } as const
    })

    return result.mapErr(intoAmplifyErrorOrThrow)
  }

  /**
   * Verify the TOTP token during the setup process.
   * Use it *only* during the setup process.
   */
  async verifyTotpSetup(totpToken: string) {
    const result = await results.Result.wrapAsync(async () => {
      await amplify.verifyTOTPSetup({ code: totpToken })
    })
    return result.mapErr(intoAmplifyErrorOrThrow)
  }

  /** Set the user's preferred MFA method. */
  async updateMFAPreference(mfaMethod: MfaType) {
    const result = await results.Result.wrapAsync(() => {
      switch (mfaMethod) {
        case 'SMS_MFA':
          return amplify.updateMFAPreference({ sms: 'PREFERRED', totp: 'DISABLED' })
        case 'TOTP':
          return amplify.updateMFAPreference({ totp: 'PREFERRED', sms: 'DISABLED' })
        case 'NOMFA':
          return amplify.updateMFAPreference({ sms: 'DISABLED', totp: 'DISABLED' })
      }
    })
    return result.mapErr(intoAmplifyErrorOrThrow)
  }

  /** Get the user's preferred MFA method. */
  async getMFAPreference() {
    const result = await results.Result.wrapAsync(async () => {
      return ((await amplify.fetchMFAPreference()).preferred as MfaType | undefined) ?? 'NOMFA'
    })
    return result.mapErr(intoAmplifyErrorOrThrow)
  }

  /**
   * Verify the TOTP token.
   * Returns the user session if the token is valid.
   */
  async verifyTotpToken(totpToken: string) {
    return (
      await results.Result.wrapAsync(() =>
        amplify.verifyTOTPSetup({ code: totpToken }).then(() => true),
      )
    ).mapErr(intoAmplifyErrorOrThrow)
  }

  /** Confirm the sign in with the MFA token. */
  async confirmSignIn(challengeResponse: string): ConfirmSignInReturn {
    const result = await results.Result.wrapAsync(() =>
      amplify.confirmSignIn({ challengeResponse }),
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
async function parseUserSession(
  session: amplify.AuthSession,
  clientId: string,
): Promise<UserSession> {
  const payload = session.tokens?.idToken?.payload
  if (session.tokens == null || payload == null) throw new Error('Session idToken missing.')
  const email = payload.email
  const refreshUrl = extractRefreshUrlFromSession(session)
  /** The `email` field is mandatory, so we assert that it exists and is a string. */
  if (typeof email !== 'string') {
    throw new Error('Payload does not have an email field.')
  } else {
    const expirationTimestamp = session.tokens.accessToken.payload.exp ?? 0
    const expireAt = dateTime.toRfc3339(new Date(expirationTimestamp * SEC_MS))
    return {
      email,
      clientId,
      expireAt,
      refreshUrl,
      accessToken: session.tokens.accessToken.toString(),
      refreshToken: await fetchRefreshToken(),
    }
  }
}

async function fetchRefreshToken() {
  // Official Amplify Auth API does not support retrieving refresh tokens. Using solution from
  // https://github.com/aws-amplify/amplify-js/issues/14324#issuecomment-2884906161
  const authTokens = await cognitoUserPoolsTokenProvider.tokenOrchestrator
    .getTokenStore()
    .loadTokens()
  if (authTokens == null) throw new Error('Cannot read refreshToken: no authTokens loaded')
  if (authTokens.refreshToken == null) throw new Error('Missing refresh Token.')
  return authTokens.refreshToken
}

/**
 * Extract the refresh session endpoint URL from the JWT token payload
 * @see https://docs.aws.amazon.com/cognito/latest/developerguide/amazon-cognito-user-pools-using-the-access-token.html
 * @throws Error if the `iss` field of the payload is not a valid URL.
 */
function extractRefreshUrlFromSession(session: amplify.AuthSession): string {
  const iss = session.tokens?.accessToken.payload.iss

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

/** Format a username and password as an {@link amplify.SignUpParams}. */
function intoSignUpParams(
  supportsDeepLinks: boolean,
  username: string,
  password: string,
  organizationId: string | null,
): amplify.SignUpInput {
  return {
    username,
    password,
    options: {
      userAttributes: {
        email: username,
        /**
         * Add a custom attribute indicating whether the user is signing up from the desktop.
         * This is used to determine the schema used in the callback links sent in the
         * verification emails. For example, `http://` for the Cloud, and `enso://` for the
         * desktop.
         */
        ...(supportsDeepLinks ? { 'custom:fromDesktop': JSON.stringify(true) } : {}),
        ...(organizationId != null ? { 'custom:organizationId': organizationId } : {}),
      },
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
