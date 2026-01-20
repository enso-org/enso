/**
 * @file Provides an {@link AuthService} which consists of an underyling `Cognito` API
 * wrapper, along with some convenience callbacks to make URL redirects for the authentication flows
 * work with Electron.
 */
import type { Logger } from '#/providers/LoggerProvider'
import * as appUtils from '$/appUtils'
import { Cognito } from '$/authentication/cognito'
import * as listen from '$/authentication/listen'
import { useFeatureFlag } from '$/providers/featureFlags'
import { useText } from '$/providers/text'
import { parseEnsoDeeplink } from '@/util/url'
import * as amplify from '@aws-amplify/auth'
import type * as saveAccessTokenModule from 'enso-common/src/accessToken'
import * as common from 'enso-common/src/constants'
import * as detect from 'enso-common/src/utilities/detect'
import * as toastify from 'react-toastify'
import { useRouter } from 'vue-router'

/**
 * Configuration for the AWS Amplify library.
 *
 * This details user pools, federated identity providers, etc. that are used to authenticate users.
 * The values in this object are not secret, and can be swapped out for testing values to avoid
 * creating authenticated users in the production environment.
 */
export interface AmplifyConfig {
  readonly region: string
  readonly endpoint: string | undefined
  readonly userPoolId: string
  readonly userPoolWebClientId: string
  readonly urlOpener: ((url: string, redirectUrl: string) => void) | null
  readonly saveAccessToken: ((accessToken: saveAccessTokenModule.AccessToken | null) => void) | null
  readonly domain: string
  readonly scope: string[]
  readonly redirectSignIn: string
  readonly redirectSignOut: string
  readonly responseType: string
}

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
  readonly endpoint: string | undefined
  readonly userPoolId: string
  readonly userPoolWebClientId: string
  readonly oauth: OauthAmplifyConfig
}

/**
 * Convert the flattened `AmplifyConfig` struct to a form recognizable to the AWS Amplify library.
 *
 * We use a flattened form of the config for easier object manipulation, but the AWS Amplify library
 * expects a nested form.
 */
export function toNestedAmplifyConfig(config: AmplifyConfig): NestedAmplifyConfig {
  return {
    region: config.region,
    // endpoint: config.endpoint,
    // TODO: Use the endpoint when it is working.
    endpoint: undefined,
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

/** Configuration for the authentication service. */
export interface AuthConfig {
  /**
   * Whether the application supports deep links. This is only true when using
   * the installed app on macOS and Windows.
   */
  readonly supportsDeepLinks: boolean
}

/** API for the authentication service. */
export interface AuthService {
  /** @see {@link Cognito}. */
  readonly cognito: Cognito
  /** @see {@link listen.ListenFunction}. */
  readonly registerAuthEventListener: listen.ListenFunction
}

/**
 * Create an instance of the authentication service.
 *
 * # Warning
 *
 * This hook should only be called in a single place, as it performs global configuration of the
 * Amplify library.
 */
export function useInitAuthService(): AuthService {
  const enableDeepLinks = useFeatureFlag('enableDeepLinks')
  const router = useRouter()

  const amplifyConfig = loadAmplifyConfig(
    console,
    enableDeepLinks.value,
    (url) => void router.push(url),
  )
  const cognito = new Cognito(console, enableDeepLinks.value, amplifyConfig)

  return { cognito, registerAuthEventListener: listen.registerAuthEventListener }
}

/** Return the appropriate Amplify configuration for the current platform. */
function loadAmplifyConfig(
  logger: Logger,
  supportsDeepLinks: boolean,
  navigate: (url: string) => void,
): AmplifyConfig {
  let urlOpener: ((url: string) => void) | null = null
  let saveAccessToken: ((accessToken: saveAccessTokenModule.AccessToken | null) => void) | null =
    null
  if (window.api != null) {
    const { authentication } = window.api
    // When running on desktop we want to have option to save access token to a file,
    // so it can be reused later when issuing requests to the Cloud API.
    //
    // Note: Wrapping this function in an arrow function ensures that the current Authentication API
    // is always used.
    saveAccessToken = (accessToken: saveAccessTokenModule.AccessToken | null) => {
      authentication.saveAccessToken(accessToken)
    }
  }
  if (supportsDeepLinks && window.api != null) {
    const { authentication } = window.api
    // The default URL opener opens the URL in the desktop app, but the user should be sent to
    // their system browser instead, because:
    // - users trust their system browser with their credentials more than they trust the app;
    // - the app can keep itself on the relevant page until the user is sent back to it, avoiding
    // unnecessary reloading caused by redirects.
    //
    // Note: Wrapping this function in an arrow function ensures that the current Authentication API
    // is always used.
    urlOpener = (url: string) => {
      authentication.openUrlInSystemBrowser(url)
    }
  }
  if (detect.isOnElectron()) {
    // To handle redirects back to the application from the system browser, a custom URL handler
    // needs to be registered.
    setDeepLinkHandler(logger, navigate)
  }

  /** Load the platform-specific Amplify configuration. */
  const signInOutRedirect =
    supportsDeepLinks ? `${common.DEEP_LINK_SCHEME}://auth` : window.location.origin
  return {
    endpoint: $config.AUTH_ENDPOINT,
    userPoolId: $config.COGNITO_USER_POOL_ID ?? '',
    userPoolWebClientId: $config.COGNITO_USER_POOL_WEB_CLIENT_ID ?? '',
    domain: $config.COGNITO_DOMAIN ?? '',
    region: $config.COGNITO_REGION ?? '',
    redirectSignIn: signInOutRedirect,
    redirectSignOut: signInOutRedirect,
    scope: ['email', 'openid', 'aws.cognito.signin.user.admin'],
    responseType: 'code',
    urlOpener,
    saveAccessToken,
  }
}

/**
 * Set the callback that will be invoked when a deep link to the application is opened.
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
 * ignored by this handler.
 */
function setDeepLinkHandler(logger: Logger, navigate: (url: string) => void) {
  window.api?.authentication.setDeepLinkHandler((urlString: string) => {
    const result = parseEnsoDeeplink(urlString)
    if (!result.ok) {
      logger.log(result.error.message())
      return
    }
    const deeplink = result.value
    switch (deeplink.pathname) {
      // If the user is being redirected after clicking the registration confirmation link in their
      // email, then the URL will be for the confirmation page path.
      case 'auth/confirmation': {
        const verificationCode = deeplink.searchParams.get('verification_code')

        let redirectUrl = ''

        // In case if the verifaction code is present, then we need to navigate to the confirmation
        // page, because the URL is a deep link for confirmation page and user is not yet confirmed.
        if (verificationCode != null) {
          redirectUrl = `${appUtils.CONFIRM_REGISTRATION_PATH}${deeplink.search}`
        } else {
          // Otherwise, we need to navigate to the setup page, because user is already confirmed.
          // but the redirect link navigates to the confirmation page, for some reason.
          redirectUrl = `${appUtils.DASHBOARD_PATH}${deeplink.search}`
        }
        navigate(redirectUrl)

        break
      }
      case 'auth': {
        if (deeplink.search === '') {
          // Signing out.
          navigate(appUtils.LOGIN_PATH)
        } else {
          // Signing in.
          void (async () => {
            // Try to find `error_description` and `error` in search params. This means something went wrong e.g
            // missing user email address while using microsoft account.
            const queryParams = new URLSearchParams(deeplink.search)
            const error = queryParams.get('error')
            const errorDescription = queryParams.get('error_description')
            const text = useText()
            if (error && errorDescription) {
              if (errorDescription?.includes('Missing required user email value')) {
                toastify.toast.error(text.getText('missingEmailError'))
              } else {
                toastify.toast.error(text.getText('registrationError'))
              }
            } else {
              // Temporarily override the `history` object so that Amplify doesn't try to call
              // `history.replaceState` (which doesn't work in the renderer process because of
              // Electron's `webSecurity`). This is a hack, but it is the only way to get Amplify to
              // work with a custom URL protocol in Electron.
              // `history.replaceState` is only being saved here to be restored later.
              // It will never be called without a bound `this`.
              const replaceState = history.replaceState
              history.replaceState = () => false
              try {
                // `_handleAuthResponse` is a private method without typings.
                await amplify.Auth['_handleAuthResponse'](urlString)

                navigate(appUtils.DASHBOARD_PATH)
              } finally {
                // Restore the original `history.replaceState` function.
                history.replaceState = replaceState
              }
            }
          })()
        }
        break
      }
      // If the user is being redirected after finishing the password reset flow, then the URL will
      // be for the login page.
      case 'auth/login': {
        navigate(appUtils.LOGIN_PATH)
        break
      }
      case 'auth/registration': {
        navigate(`${appUtils.REGISTRATION_PATH}${deeplink.search}`)
        break
      }
      case 'payments/success': {
        navigate(`${appUtils.PAYMENTS_SUCCESS_PATH}${deeplink.search}`)
        break
      }
      // If the user is being redirected from a password reset email, navigate to the password
      // reset page, with the verification code and email prefilled.
      case 'password-reset': {
        navigate(`${appUtils.RESET_PASSWORD_PATH}${deeplink.search}`)
        break
      }
      default: {
        navigate(`/${deeplink.pathname}`)
        break
      }
    }
  })
}
