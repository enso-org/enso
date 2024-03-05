/** @file Provides an {@link AuthService} which consists of an underyling `Cognito` API
 * wrapper, along with some convenience callbacks to make URL redirects for the authentication flows
 * work with Electron. */
import * as amplify from '@aws-amplify/auth'

import * as common from 'enso-common'
import * as detect from 'enso-common/src/detect'

import * as appUtils from '#/appUtils'

import type * as loggerProvider from '#/providers/LoggerProvider'

import * as cognitoModule from '#/authentication/cognito'
import * as listen from '#/authentication/listen'

// =====================
// === AmplifyConfig ===
// =====================

/** Configuration for the AWS Amplify library.
 *
 * This details user pools, federated identity providers, etc. that are used to authenticate users.
 * The values in this object are not secret, and can be swapped out for testing values to avoid
 * creating authenticated users in the production environment. */
export interface AmplifyConfig {
  readonly region: string
  readonly userPoolId: string
  readonly userPoolWebClientId: string
  readonly urlOpener: ((url: string, redirectUrl: string) => void) | null
  readonly saveAccessToken: ((accessToken: SaveAccessTokenPayload | null) => void) | null
  readonly domain: string
  readonly scope: string[]
  readonly redirectSignIn: string
  readonly redirectSignOut: string
  readonly responseType: string
}

// ===========================
// === NestedAmplifyConfig ===
// ===========================

/** Configuration options for a {@link OauthAmplifyConfig}. */
interface OauthAmplifyConfigOptions {
  readonly urlOpener?: (url: string, redirectUrl: string) => void
}

/** OAuth configuration for a {@link NestedAmplifyConfig}. */
interface OauthAmplifyConfig {
  readonly options: OauthAmplifyConfigOptions
  readonly domain: string
  readonly scope: string[]
  readonly redirectSignIn: string
  readonly redirectSignOut: string
  readonly responseType: string
}

/** Same as {@link AmplifyConfig}, but in a format recognized by the AWS Amplify library. */
export interface NestedAmplifyConfig {
  readonly region: string
  readonly userPoolId: string
  readonly userPoolWebClientId: string
  readonly oauth: OauthAmplifyConfig
}

/** Convert the flattened `AmplifyConfig` struct to a form recognizable to the AWS Amplify library.
 *
 * We use a flattened form of the config for easier object manipulation, but the AWS Amplify library
 * expects a nested form. */
export function toNestedAmplifyConfig(config: AmplifyConfig): NestedAmplifyConfig {
  return {
    region: config.region,
    userPoolId: config.userPoolId,
    userPoolWebClientId: config.userPoolWebClientId,
    oauth: {
      options: config.urlOpener ? { urlOpener: config.urlOpener } : {},
      domain: config.domain,
      scope: config.scope,
      redirectSignIn: config.redirectSignIn,
      redirectSignOut: config.redirectSignOut,
      responseType: config.responseType,
    },
  }
}

// ==================
// === AuthConfig ===
// ==================

/** Configuration for the authentication service. */
export interface AuthConfig {
  /** Logger for the authentication service. */
  readonly logger: loggerProvider.Logger
  /** Whether the application supports deep links. This is only true when using
   * the installed app on macOS and Windows. */
  readonly supportsDeepLinks: boolean
  /** Function to navigate to a given (relative) URL.
   *
   * Used to redirect to pages like the password reset page with the query parameters set in the
   * URL (e.g., `?verification_code=...`). */
  readonly navigate: (url: string) => void
}

// ===================
// === AuthService ===
// ===================

/** API for the authentication service. */
export interface AuthService {
  /** @see {@link cognitoModule.Cognito}. */
  readonly cognito: cognitoModule.Cognito
  /** @see {@link listen.ListenFunction}. */
  readonly registerAuthEventListener: listen.ListenFunction
}

/** Create an instance of the authentication service.
 *
 * # Warning
 *
 * This function should only be called once, and the returned service should be used throughout the
 * application. This is because it performs global configuration of the Amplify library. */
export function initAuthService(authConfig: AuthConfig): AuthService | null {
  const { logger, supportsDeepLinks, navigate } = authConfig
  const amplifyConfig = loadAmplifyConfig(logger, supportsDeepLinks, navigate)
  const cognito =
    amplifyConfig == null
      ? null
      : new cognitoModule.Cognito(logger, supportsDeepLinks, amplifyConfig)

  return cognito == null
    ? null
    : { cognito, registerAuthEventListener: listen.registerAuthEventListener }
}

/** Return the appropriate Amplify configuration for the current platform. */
function loadAmplifyConfig(
  logger: loggerProvider.Logger,
  supportsDeepLinks: boolean,
  navigate: (url: string) => void
): AmplifyConfig | null {
  let urlOpener: ((url: string) => void) | null = null
  let saveAccessToken: ((accessToken: SaveAccessTokenPayload | null) => void) | null = null
  if ('authenticationApi' in window) {
    // When running on desktop we want to have option to save access token to a file,
    // so it can be reused later when issuing requests to the Cloud API.
    //
    // Note: Wrapping this function in an arrow function ensures that the current Authentication API
    // is always used.
    saveAccessToken = (accessToken: SaveAccessTokenPayload | null) => {
      window.authenticationApi.saveAccessToken(accessToken)
    }
  }
  if (supportsDeepLinks) {
    // The default URL opener opens the URL in the desktop app, but the user should be sent to
    // their system browser instead, because:
    // - users trust their system browser with their credentials more than they trust the app;
    // - the app can keep itself on the relevant page until the user is sent back to it, avoiding
    // unnecessary reloading caused by redirects.
    //
    // Note: Wrapping this function in an arrow function ensures that the current Authentication API
    // is always used.
    urlOpener = (url: string) => {
      window.authenticationApi.openUrlInSystemBrowser(url)
    }
  }
  if (detect.isOnElectron()) {
    // To handle redirects back to the application from the system browser, a custom URL handler
    // needs to be registered.
    setDeepLinkHandler(logger, navigate)
  }
  /** Load the platform-specific Amplify configuration. */
  const signInOutRedirect = supportsDeepLinks
    ? `${common.DEEP_LINK_SCHEME}://auth`
    : process.env.ENSO_CLOUD_REDIRECT
  return process.env.ENSO_CLOUD_COGNITO_USER_POOL_ID == null ||
    process.env.ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID == null ||
    process.env.ENSO_CLOUD_COGNITO_DOMAIN == null ||
    process.env.ENSO_CLOUD_COGNITO_REGION == null
    ? null
    : {
        userPoolId: process.env.ENSO_CLOUD_COGNITO_USER_POOL_ID,
        userPoolWebClientId: process.env.ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID,
        domain: process.env.ENSO_CLOUD_COGNITO_DOMAIN,
        region: process.env.ENSO_CLOUD_COGNITO_REGION,
        redirectSignIn: signInOutRedirect,
        redirectSignOut: signInOutRedirect,
        scope: ['email', 'openid', 'aws.cognito.signin.user.admin'],
        responseType: 'code',
        urlOpener,
        saveAccessToken,
      }
}

/** Set the callback that will be invoked when a deep link to the application is opened.
 *
 * Typically this callback is invoked when the user is redirected back to the app after:
 *
 * 1. Authenticating with a federated identity provider; or
 * 2. Clicking a "reset password" link in a password reset email.
 *
 * For example, when the user completes an OAuth sign in flow (e.g., through Google), they are
 * redirected to a URL like `enso://authentication/register?code=...`. This listener will intercept
 * that URL and open the page `register?code=...` in the application window.
 *
 * This is only used when running on the desktop, as the browser version of the app lets Amplify
 * handle the redirect for us. On the desktop however, we need to handle the redirect ourselves,
 * because it's a deep link into the app, and Amplify doesn't handle deep links.
 *
 * All URLs that don't have a pathname that starts with `AUTHENTICATION_PATHNAME_BASE` will be
 * ignored by this handler. */
function setDeepLinkHandler(logger: loggerProvider.Logger, navigate: (url: string) => void) {
  window.authenticationApi.setDeepLinkHandler((urlString: string) => {
    const url = new URL(urlString)
    logger.log(`Parsed pathname: ${url.pathname}`)
    // Remove the trailing slash in the pathname - it is present on Windows but not on macOS.
    const pathname = url.pathname.replace(/\/$/, '')
    switch (pathname) {
      // If the user is being redirected after clicking the registration confirmation link in their
      // email, then the URL will be for the confirmation page path.
      case '//auth/confirmation': {
        const redirectUrl = `${appUtils.CONFIRM_REGISTRATION_PATH}${url.search}`
        navigate(redirectUrl)
        break
      }
      case '//auth': {
        if (url.search === '') {
          // Signing out.
          navigate(appUtils.LOGIN_PATH)
        } else {
          // Signing in.
          void (async () => {
            // Temporarily override the `history` object so that Amplify doesn't try to call
            // `history.replaceState` (which doesn't work in the renderer process because of
            // Electron's `webSecurity`). This is a hack, but it is the only way to get Amplify to
            // work with a custom URL protocol in Electron.
            // `history.replaceState` is only being saved here to be restored later.
            // It will never be called without a bound `this`.
            // eslint-disable-next-line @typescript-eslint/unbound-method
            const replaceState = history.replaceState
            history.replaceState = () => false
            try {
              // @ts-expect-error `_handleAuthResponse` is a private method without typings.
              // eslint-disable-next-line @typescript-eslint/no-unsafe-call
              await amplify.Auth._handleAuthResponse(url)
            } finally {
              // Restore the original `history.replaceState` function.
              history.replaceState = replaceState
            }
          })()
        }
        break
      }
      // If the user is being redirected after finishing the password reset flow, then the URL will
      // be for the login page.
      case '//auth/login': {
        navigate(appUtils.LOGIN_PATH)
        break
      }
      case '//auth/registration': {
        navigate(`${appUtils.REGISTRATION_PATH}${url.search}`)
        break
      }
      // If the user is being redirected from a password reset email, navigate to the password
      // reset page, with the verification code and email prefilled.
      case appUtils.RESET_PASSWORD_PATH: {
        navigate(`${appUtils.RESET_PASSWORD_PATH}${url.search}`)
        break
      }
      default: {
        logger.error(`Ignoring unknown deep link '${urlString}'.`)
        break
      }
    }
  })
}
