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
// This SHOULD NOT import any runtime code.
// eslint-disable-next-line @typescript-eslint/consistent-type-imports
import type * as cognito from 'amazon-cognito-identity-js'
import * as results from 'ts-results'

import * as detect from 'enso-common/src/detect'

import * as config from '../../../../src/authentication/src/authentication/config'
import * as loggerProvider from '../../../../src/authentication/src/providers/logger'

/* eslint-disable @typescript-eslint/no-unused-vars */

// =================
// === Constants ===
// =================

/** One second, in milliseconds. */
const SEC_MS = 1_000
/** One day, in milliseconds. */
const DAY_MS = 86_400_000
/** One hour, in seconds. */
const HOUR_S = 3_600

const MESSAGES = {
    signInWithPassword: {
        userNotFound: 'Username not found. Please register first.',
        userNotConfirmed: 'User is not confirmed. Please check your email for a confirmation link.',
        incorrectUsernameOrPassword: 'Incorrect username or password.',
    },
    forgotPassword: {
        userNotFound: 'Username not found. Please register first.',
        userNotConfirmed: `Cannot reset password for user with an unverified email. \
Please verify your email first.`,
    },
}

// ================
// === UserInfo ===
// ================

// The names come from a third-party API and cannot be changed.
/* eslint-disable @typescript-eslint/naming-convention */
/** Attributes returned from {@link amplify.Auth.currentUserInfo}. */
interface UserAttributes {
    email: string
    email_verified: boolean
    sub: string
    'custom:fromDesktop'?: string
    'custom:organizationId'?: string
}
/* eslint-enable @typescript-eslint/naming-convention */

/** User information returned from {@link amplify.Auth.currentUserInfo}. */
interface UserInfo {
    username: string
    // The type comes from a third-party API and cannot be changed.
    // eslint-disable-next-line no-restricted-syntax
    id: undefined
    attributes: UserAttributes
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

/** Hint to TypeScript if we can safely cast an `unknown` error to an {@link AmplifyError}. */
function isAmplifyError(error: unknown): error is AmplifyError {
    if (error != null && typeof error === 'object') {
        return 'code' in error && 'message' in error && 'name' in error
    } else {
        return false
    }
}

/** Convert the `unknown` error into an {@link AmplifyError} and returns it, or re-throws it if
 * conversion is not possible.
 * @throws If the error is not an amplify error. */
function intoAmplifyErrorOrThrow(error: unknown): AmplifyError {
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
    name: string
    log: string
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

/** Base interface for all errors output from this module.
 * Every user-facing error MUST extend this interface. */
interface CognitoError {
    kind: string
    message: string
}

// ===============
// === Cognito ===
// ===============

/** Thin wrapper around Cognito endpoints from the AWS Amplify library with error handling added.
 * This way, the methods don't throw all errors, but define exactly which errors they return.
 * The caller can then handle them via pattern matching on the {@link results.Result} type. */
export class Cognito {
    /** The organization id to be returned by {@link organizationId}. */
    mockOrganizationId: string | null = null
    mockEmail: string | null = null
    signedIn = false

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
            // eslint-disable-next-line no-restricted-syntax
            return Promise.resolve({
                getAccessToken: () => {
                    return {
                        getJwtToken: () => {
                            return `.${window.btoa(
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
                                    exp: date + HOUR_S,
                                    iat: date,
                                    jti: '5ab178b7-97a6-4956-8913-1cffee4a0da1',
                                    username: this.mockEmail,
                                    /* eslint-enable @typescript-eslint/naming-convention */
                                })
                            )}.`
                        },
                    }
                },
            } as cognito.CognitoUserSession)
        })
        const amplifySession = currentSession.mapErr(intoCurrentSessionErrorKind)
        return amplifySession.map(parseUserSession).toOption()
    }

    /** Returns the associated organization ID of the current user, which is passed during signup,
     * or `null` if the user is not associated with an existing organization. */
    async organizationId() {
        return Promise.resolve(this.mockOrganizationId)
    }

    /** Sign up with username and password.
     *
     * Does not rely on federated identity providers (e.g., Google or GitHub). */
    signUp(username: string, password: string, organizationId: string | null) {
        return signUp(this.supportsDeepLinks, username, password, organizationId)
    }

    /** Send the email address verification code.
     *
     * The user will receive a link in their email. The user must click the link to go to the email
     * verification page. The email verification page will parse the verification code from the URL.
     * If the verification code matches, the email address is marked as verified. Once the email
     * address is verified, the user can sign in. */
    confirmSignUp(email: string, code: string) {
        this.mockEmail = email
        return confirmSignUp(email, code)
    }

    /** Sign in via the Google federated identity provider.
     *
     * This function will open the Google authentication page in the user's browser. The user will
     * be asked to log in to their Google account, and then to grant access to the application.
     * After the user has granted access, the browser will be redirected to the application. */
    signInWithGoogle() {
        return signInWithGoogle(this.customState())
    }

    /** Sign in via the GitHub federated identity provider.
     *
     * This function will open the GitHub authentication page in the user's browser. The user will
     * be asked to log in to their GitHub account, and then to grant access to the application.
     * After the user has granted access, the browser will be redirected to the application. */
    signInWithGitHub() {
        return signInWithGitHub()
    }

    /** Sign in with the given username and password.
     *
     * Does not rely on external identity providers (e.g., Google or GitHub). */
    signInWithPassword(username: string, password: string) {
        this.mockEmail = username
        return signInWithPassword(username, password)
    }

    /** Sign out the current user. */
    async signOut() {
        this.signedIn = false
        return Promise.resolve(null)
    }

    /** Send a password reset email.
     *
     * The user will be able to reset their password by following the link in the email, which takes
     * them to the "reset password" page of the application. The verification code will be filled in
     * automatically. */
    forgotPassword(email: string) {
        return forgotPassword(email)
    }

    /** Submit a new password for the given email address.
     *
     * The user will have received a verification code in an email, which they will have entered on
     * the "reset password" page of the application. This function will submit the new password
     * along with the verification code, changing the user's password. */
    forgotPasswordSubmit(email: string, code: string, password: string) {
        return forgotPasswordSubmit(email, code, password)
    }

    /** Change a password for current authenticated user.
     *
     * Allow users to independently modify their passwords. The user needs to provide the old
     * password, new password, and repeat new password to change their old password to the new
     * one. The validation of the repeated new password is handled by the `changePasswordModel`
     * component. */
    changePassword(oldPassword: string, newPassword: string) {
        return changePassword(oldPassword, newPassword)
    }

    /** We want to signal to Amplify to fire a "custom state change" event when the user is
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
     * See: https://github.com/aws-amplify/amplify-js/issues/3391#issuecomment-756473970 */
    private customState() {
        return detect.isRunningInElectron() ? window.location.pathname : null
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
        const accessToken = session.getAccessToken().getJwtToken()
        return { email, accessToken }
    }
}

/** Internal IDs of errors that may occur when getting the current session. */
export enum CurrentSessionErrorKind {
    noCurrentUser = 'NoCurrentUser',
}

const CURRENT_SESSION_NO_CURRENT_USER_ERROR = {
    internalMessage: 'No current user',
    kind: CurrentSessionErrorKind.noCurrentUser,
}

/** Convert an {@link AmplifyError} into a {@link CurrentSessionErrorKind} if it is a known error,
 * else re-throws the error.
 * @throws {Error} If the error is not recognized. */
function intoCurrentSessionErrorKind(error: unknown): CurrentSessionErrorKind {
    if (error === CURRENT_SESSION_NO_CURRENT_USER_ERROR.internalMessage) {
        return CURRENT_SESSION_NO_CURRENT_USER_ERROR.kind
    } else {
        throw error
    }
}

// ==============
// === SignUp ===
// ==============

/** A wrapper around the Amplify "sign up" endpoint that converts known errors
 * to {@link SignUpError}s. */
async function signUp(
    _supportsDeepLinks: boolean,
    _username: string,
    _password: string,
    _organizationId: string | null
) {
    const result = await results.Result.wrapAsync(async () => {
        // Ignored.
    })
    return result.mapErr(intoAmplifyErrorOrThrow).mapErr(intoSignUpErrorOrThrow)
}

/** Format a username and password as an {@link amplify.SignUpParams}. */
function intoSignUpParams(
    supportsDeepLinks: boolean,
    username: string,
    password: string,
    organizationId: string | null
) {
    return {
        username,
        password,
        attributes: {
            email: username,
            /** Add a custom attribute indicating whether the user is signing up from the desktop.
             * This is used to determine the schema used in the callback links sent in the
             * verification emails. For example, `http://` for the Cloud, and `enso://` for the
             * desktop.
             *
             * # Naming Convention
             *
             * It is necessary to disable the naming convention rule here, because the key is
             * expected to appear exactly as-is in Cognito, so we must match it. */
            // eslint-disable-next-line @typescript-eslint/naming-convention
            ...(supportsDeepLinks ? { 'custom:fromDesktop': JSON.stringify(true) } : {}),
            // eslint-disable-next-line @typescript-eslint/naming-convention
            ...(organizationId != null ? { 'custom:organizationId': organizationId } : {}),
        },
    }
}

/** Internal IDs of errors that may occur when signing up. */
export enum SignUpErrorKind {
    usernameExists = 'UsernameExists',
    invalidParameter = 'InvalidParameter',
    invalidPassword = 'InvalidPassword',
}

const SIGN_UP_USERNAME_EXISTS_ERROR = {
    internalCode: 'UsernameExistsException',
    kind: SignUpErrorKind.usernameExists,
}

const SIGN_UP_INVALID_PARAMETER_ERROR = {
    internalCode: 'InvalidParameterException',
    kind: SignUpErrorKind.invalidParameter,
}

const SIGN_UP_INVALID_PASSWORD_ERROR = {
    internalCode: 'InvalidPasswordException',
    kind: SignUpErrorKind.invalidPassword,
}

/** An error that may occur when signing up. */
export interface SignUpError extends CognitoError {
    kind: SignUpErrorKind
    message: string
}

/** Convert an {@link AmplifyError} into a {@link SignUpError} if it is a known error,
 * else re-throws the error.
 * @throws {Error} If the error is not recognized. */
function intoSignUpErrorOrThrow(error: AmplifyError): SignUpError {
    if (error.code === SIGN_UP_USERNAME_EXISTS_ERROR.internalCode) {
        return {
            kind: SIGN_UP_USERNAME_EXISTS_ERROR.kind,
            message: error.message,
        }
    } else if (error.code === SIGN_UP_INVALID_PARAMETER_ERROR.internalCode) {
        return {
            kind: SIGN_UP_INVALID_PARAMETER_ERROR.kind,
            message: error.message,
        }
    } else if (error.code === SIGN_UP_INVALID_PASSWORD_ERROR.internalCode) {
        return {
            kind: SIGN_UP_INVALID_PASSWORD_ERROR.kind,
            message: error.message,
        }
    } else {
        throw error
    }
}

// =====================
// === ConfirmSignUp ===
// =====================

/** A wrapper around the Amplify "confirm sign up" endpoint that converts known errors
 * to {@link ConfirmSignUpError}s. */
async function confirmSignUp(_email: string, _code: string) {
    return results.Result.wrapAsync(async () => {
        await Promise.resolve(null)
    }).then(result => result.mapErr(intoAmplifyErrorOrThrow).mapErr(intoConfirmSignUpErrorOrThrow))
}

/** Internal IDs of errors that may occur when confirming registration. */
export enum ConfirmSignUpErrorKind {
    userAlreadyConfirmed = 'UserAlreadyConfirmed',
}

const CONFIRM_SIGN_UP_USER_ALREADY_CONFIRMED_ERROR = {
    internalCode: 'NotAuthorizedException',
    internalMessage: 'User cannot be confirmed. Current status is CONFIRMED',
    kind: ConfirmSignUpErrorKind.userAlreadyConfirmed,
}

/** An error that may occur when confirming registration. */
export interface ConfirmSignUpError extends CognitoError {
    kind: ConfirmSignUpErrorKind
    message: string
}

/** Convert an {@link AmplifyError} into a {@link ConfirmSignUpError} if it is a known error,
 * else re-throws the error.
 * @throws {Error} If the error is not recognized. */
function intoConfirmSignUpErrorOrThrow(error: AmplifyError): ConfirmSignUpError {
    if (
        error.code === CONFIRM_SIGN_UP_USER_ALREADY_CONFIRMED_ERROR.internalCode &&
        error.message === CONFIRM_SIGN_UP_USER_ALREADY_CONFIRMED_ERROR.internalMessage
    ) {
        return {
            /** Don't re-use the original `error.code` here because Amplify overloads the same code
             * for multiple kinds of errors. We replace it with a custom code that has no
             * ambiguity. */
            kind: CONFIRM_SIGN_UP_USER_ALREADY_CONFIRMED_ERROR.kind,
            message: error.message,
        }
    } else {
        throw error
    }
}

// ========================
// === SignInWithGoogle ===
// ========================

/** A wrapper around the Amplify "sign in with Google" endpoint. */
async function signInWithGoogle(_customState: string | null) {
    await Promise.resolve(null)
}

// ========================
// === SignInWithGoogle ===
// ========================

/** A wrapper around the Amplify confirm "sign in with GitHub" endpoint. */
function signInWithGitHub() {
    return Promise.resolve({
        accessKeyId: 'access key id',
        sessionToken: 'session token',
        secretAccessKey: 'secret access key',
        identityId: 'identity id',
        authenticated: true,
        expiration: new Date(Number(new Date()) + DAY_MS),
    })
}

// ==========================
// === SignInWithPassword ===
// ==========================

/** A wrapper around the Amplify "sign in with password" endpoint that converts known errors
 * to {@link SignInWithPasswordError}s. */
async function signInWithPassword(_username: string, _password: string) {
    const result = await results.Result.wrapAsync(async () => {
        await Promise.resolve(null)
    })
    return result.mapErr(intoAmplifyErrorOrThrow).mapErr(intoSignInWithPasswordErrorOrThrow)
}

/** Internal IDs of errors that may occur when signing in with a password. */
export enum SignInWithPasswordErrorKind {
    notAuthorized = 'NotAuthorized',
    userNotConfirmed = 'UserNotConfirmed',
    userNotFound = 'UserNotFound',
}

/** An error that may occur when signing in with a password. */
export interface SignInWithPasswordError extends CognitoError {
    kind: SignInWithPasswordErrorKind
    message: string
}

/** Convert an {@link AmplifyError} into a {@link SignInWithPasswordError} if it is a known error,
 * else re-throws the error.
 * @throws {Error} If the error is not recognized. */
function intoSignInWithPasswordErrorOrThrow(error: AmplifyError): SignInWithPasswordError {
    switch (error.code) {
        case 'UserNotFoundException':
            return {
                kind: SignInWithPasswordErrorKind.userNotFound,
                message: MESSAGES.signInWithPassword.userNotFound,
            }
        case 'UserNotConfirmedException':
            return {
                kind: SignInWithPasswordErrorKind.userNotConfirmed,
                message: MESSAGES.signInWithPassword.userNotConfirmed,
            }
        case 'NotAuthorizedException':
            return {
                kind: SignInWithPasswordErrorKind.notAuthorized,
                message: MESSAGES.signInWithPassword.incorrectUsernameOrPassword,
            }
        default:
            throw error
    }
}

// ======================
// === ForgotPassword ===
// ======================

/** Internal IDs of errors that may occur when requesting a password reset. */
export enum ForgotPasswordErrorKind {
    userNotConfirmed = 'UserNotConfirmed',
    userNotFound = 'UserNotFound',
}

const FORGOT_PASSWORD_USER_NOT_CONFIRMED_ERROR = {
    internalCode: 'InvalidParameterException',
    message: `Cannot reset password for the user as there is no registered/verified email or \
phone_number`,
    kind: ForgotPasswordErrorKind.userNotConfirmed,
}

const FORGOT_PASSWORD_USER_NOT_FOUND_ERROR = {
    internalCode: 'UserNotFoundException',
    kind: ForgotPasswordErrorKind.userNotFound,
}

/** A wrapper around the Amplify "forgot password" endpoint that converts known errors
 * to {@link ForgotPasswordError}s. */
async function forgotPassword(email: string) {
    return results.Result.wrapAsync(async () => {
        await amplify.Auth.forgotPassword(email)
    }).then(result => result.mapErr(intoAmplifyErrorOrThrow).mapErr(intoForgotPasswordErrorOrThrow))
}

/** An error that may occur when requesting a password reset. */
export interface ForgotPasswordError extends CognitoError {
    kind: ForgotPasswordErrorKind
    message: string
}

/** Convert an {@link AmplifyError} into a {@link ForgotPasswordError} if it is a known error,
 * else re-throws the error.
 * @throws {Error} If the error is not recognized. */
function intoForgotPasswordErrorOrThrow(error: AmplifyError): ForgotPasswordError {
    if (error.code === FORGOT_PASSWORD_USER_NOT_FOUND_ERROR.internalCode) {
        return {
            kind: FORGOT_PASSWORD_USER_NOT_FOUND_ERROR.kind,
            message: MESSAGES.forgotPassword.userNotFound,
        }
    } else if (
        error.code === FORGOT_PASSWORD_USER_NOT_CONFIRMED_ERROR.internalCode &&
        error.message === FORGOT_PASSWORD_USER_NOT_CONFIRMED_ERROR.message
    ) {
        return {
            kind: FORGOT_PASSWORD_USER_NOT_CONFIRMED_ERROR.kind,
            message: MESSAGES.forgotPassword.userNotConfirmed,
        }
    } else {
        throw error
    }
}

// ============================
// === ForgotPasswordSubmit ===
// ============================

/** A wrapper around the Amplify "forgot password submit" endpoint that converts known errors
 * to {@link ForgotPasswordSubmitError}s. */
async function forgotPasswordSubmit(_email: string, _code: string, _password: string) {
    const result = await results.Result.wrapAsync(async () => {
        await Promise.resolve(null)
    })
    return result.mapErr(intoForgotPasswordSubmitErrorOrThrow)
}

/** Internal IDs of errors that may occur when resetting a password. */
export enum ForgotPasswordSubmitErrorKind {
    amplifyError = 'AmplifyError',
    authError = 'AuthError',
}

/** An error that may occur when resetting a password. */
export interface ForgotPasswordSubmitError extends CognitoError {
    kind: ForgotPasswordSubmitErrorKind
    message: string
}

/** Convert an {@link AmplifyError} into a {@link ForgotPasswordSubmitError}
 * if it is a known error, else re-throws the error.
 * @throws {Error} If the error is not recognized. */
function intoForgotPasswordSubmitErrorOrThrow(error: unknown): ForgotPasswordSubmitError {
    if (isAuthError(error)) {
        return {
            kind: ForgotPasswordSubmitErrorKind.authError,
            message: error.log,
        }
    } else if (isAmplifyError(error)) {
        return {
            kind: ForgotPasswordSubmitErrorKind.amplifyError,
            message: error.message,
        }
    } else {
        throw error
    }
}

// ======================
// === ChangePassword ===
// ======================

/** A wrapper around the Amplify "current authenticated user" endpoint that converts known errors
 * to {@link AmplifyError}s. */
async function currentAuthenticatedUser() {
    const result = await results.Result.wrapAsync(
        /** The interface provided by Amplify declares that the return type is
         * `Promise<CognitoUser | any>`, but TypeScript automatically converts it to `Promise<any>`.
         * Therefore, it is necessary to use `as` to narrow down the type to
         * `Promise<CognitoUser>`. */
        // eslint-disable-next-line no-restricted-syntax
        () => amplify.Auth.currentAuthenticatedUser() as Promise<amplify.CognitoUser>
    )
    return result.mapErr(intoAmplifyErrorOrThrow)
}

/** A wrapper around the Amplify "change password submit" endpoint that converts known errors
 * to {@link AmplifyError}s. */
async function changePassword(oldPassword: string, newPassword: string) {
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
