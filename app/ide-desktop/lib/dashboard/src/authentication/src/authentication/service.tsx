/** @file Provides an {@link AuthService} which consists of an underyling {@link Cognito} API
 * wrapper, along with some convenience callbacks to make URL redirects for the authentication flows
 * work with Electron. */
import * as amplify from "@aws-amplify/auth";

import * as common from "enso-common";

import * as app from "../components/app";
import * as auth from "./config";
import * as cognito from "./cognito";
import * as config from "../config";
import * as listen from "./listen";
import * as loggerProvider from "../providers/logger";
import * as platformModule from "../platform";
import * as utils from "../utils";

// =================
// === Constants ===
// =================

/** Pathname of the {@link URL} for deep links to the sign in page, after a redirect from a
 * federated identity provider. */
const SIGN_IN_PATHNAME = "//auth";
/** Pathname of the {@link URL} for deep links to the registration confirmation page, after a
 * redirect from an account verification email. */
const CONFIRM_REGISTRATION_PATHNAME = "//auth/confirmation";
/** URL used as the OAuth redirect when running in the desktop app. */
const DESKTOP_REDIRECT = utils.brand<auth.OAuthRedirect>(
  `${common.DEEP_LINK_SCHEME}://auth`
);
/** Map from platform to the OAuth redirect URL that should be used for that platform. */
const PLATFORM_TO_CONFIG: Record<
  platformModule.Platform,
  Pick<auth.AmplifyConfig, "redirectSignIn" | "redirectSignOut">
> = {
  [platformModule.Platform.desktop]: {
    redirectSignIn: DESKTOP_REDIRECT,
    redirectSignOut: DESKTOP_REDIRECT,
  },
  [platformModule.Platform.cloud]: {
    redirectSignIn: config.ACTIVE_CONFIG.cloudRedirect,
    redirectSignOut: config.ACTIVE_CONFIG.cloudRedirect,
  },
};

const BASE_AMPLIFY_CONFIG = {
  region: auth.AWS_REGION,
  scope: auth.OAUTH_SCOPES,
  responseType: auth.OAUTH_RESPONSE_TYPE,
} satisfies Partial<auth.AmplifyConfig>;

/** Collection of configuration details for Amplify user pools, sorted by deployment environment. */
const AMPLIFY_CONFIGS = {
  /** Configuration for @pbuchu's Cognito user pool. */
  pbuchu: {
    userPoolId: utils.brand<auth.UserPoolId>("eu-west-1_jSF1RbgPK"),
    userPoolWebClientId: utils.brand<auth.UserPoolWebClientId>(
      "1bnib0jfon3aqc5g3lkia2infr"
    ),
    domain: utils.brand<auth.OAuthDomain>(
      "pb-enso-domain.auth.eu-west-1.amazoncognito.com"
    ),
    ...BASE_AMPLIFY_CONFIG,
  } satisfies Partial<auth.AmplifyConfig>,
  /** Configuration for the production Cognito user pool. */
  production: {
    userPoolId: utils.brand<auth.UserPoolId>("eu-west-1_9Kycu2SbD"),
    userPoolWebClientId: utils.brand<auth.UserPoolWebClientId>(
      "4j9bfs8e7415erf82l129v0qhe"
    ),
    domain: utils.brand<auth.OAuthDomain>(
      "production-enso-domain.auth.eu-west-1.amazoncognito.com"
    ),
    ...BASE_AMPLIFY_CONFIG,
  } satisfies Partial<auth.AmplifyConfig>,
};

// ==================
// === AuthConfig ===
// ==================

/** Configuration for the authentication service. */
export interface AuthConfig {
  /** Logger for the authentication service. */
  logger: loggerProvider.Logger;
  /** Whether the application is running on a desktop (i.e., versus in the Cloud). */
  platform: platformModule.Platform;
  /** Function to navigate to a given (relative) URL.
   *
   * Used to redirect to pages like the password reset page with the query parameters set in the
   * URL (e.g., `?verification_code=...`). */
  navigate: (url: string) => void;
}

// ===================
// === AuthService ===
// ===================

/** API for the authentication service. */
export interface AuthService {
  /** @see {@link cognito.Cognito}. */
  cognito: cognito.Cognito;
  /** @see {@link listen.ListenFunction} */
  registerAuthEventListener: listen.ListenFunction;
}

/** Creates an instance of the authentication service.
 *
 * # Warning
 *
 * This function should only be called once, and the returned service should be used throughout the
 * application. This is because it performs global configuration of the Amplify library. */
export function initAuthService(authConfig: AuthConfig): AuthService {
  const { logger, platform, navigate } = authConfig;
  const amplifyConfig = loadAmplifyConfig(logger, platform, navigate);
  const cognitoClient = new cognito.Cognito(logger, platform, amplifyConfig);
  return {
    cognito: cognitoClient,
    registerAuthEventListener: listen.registerAuthEventListener,
  };
}

function loadAmplifyConfig(
  logger: loggerProvider.Logger,
  platform: platformModule.Platform,
  navigate: (url: string) => void
): auth.AmplifyConfig {
  /** Load the environment-specific Amplify configuration. */
  const baseConfig = AMPLIFY_CONFIGS[config.ENVIRONMENT];
  let urlOpener;
  if (platform === platformModule.Platform.desktop) {
    /** If we're running on the desktop, we want to override the default URL opener for OAuth
     * flows.  This is because the default URL opener opens the URL in the desktop app itself,
     * but we want the user to be sent to their system browser instead. The user should be sent
     * to their system browser because:
     *
     * - users trust their system browser with their credentials more than they trust our app;
     * - our app can keep itself on the relevant page until the user is sent back to it (i.e.,
     * we avoid unnecessary reloads/refreshes caused by redirects. */
    urlOpener = openUrlWithExternalBrowser;

    /** To handle redirects back to the application from the system browser, we also need to
     * register a custom URL handler. */
    setDeepLinkHandler(logger, navigate);
  }
  /** Load the platform-specific Amplify configuration. */
  const platformConfig = PLATFORM_TO_CONFIG[platform];
  return {
    ...baseConfig,
    ...platformConfig,
    ...(urlOpener ? { urlOpener } : {}),
  };
}

function openUrlWithExternalBrowser(url: string) {
  window.authenticationApi.openUrlInSystemBrowser(url);
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
 * All URLs that don't have a pathname that starts with {@link AUTHENTICATION_PATHNAME_BASE} will be
 * ignored by this handler. */
function setDeepLinkHandler(
  logger: loggerProvider.Logger,
  navigate: (url: string) => void
) {
  const onDeepLink = (url: string) => {
    const parsedUrl = new URL(url);

    if (isConfirmRegistrationRedirect(parsedUrl)) {
      /** Navigate to a relative URL to handle the confirmation link. */
      const redirectUrl = `${app.CONFIRM_REGISTRATION_PATH}${parsedUrl.search}`;
      navigate(redirectUrl);
    } else if (isSignInRedirect(parsedUrl)) {
      handleAuthResponse(url);
    } else {
      logger.error(`${url} is an unrecognized deep link. Ignoring.`);
    }
  };

  window.authenticationApi.setDeepLinkHandler(onDeepLink);
}

/** If the user is being redirected after clicking the registration confirmation link in their
 * email, then the URL will be for the confirmation page path. */
function isConfirmRegistrationRedirect(url: URL) {
  return url.pathname === CONFIRM_REGISTRATION_PATHNAME;
}

/** If the user is being redirected after a sign-out, then query args will be present. */
function isSignInRedirect(url: URL) {
  return url.pathname === SIGN_IN_PATHNAME && url.search !== "";
}

/** When the user is being redirected from a federated identity provider, then we need to pass the
 * URL to the Amplify library, which will parse the URL and complete the OAuth flow. */
function handleAuthResponse(url: string) {
  void (async () => {
    /** Temporarily override the `window.location` object so that Amplify doesn't try to call
     * `window.location.replaceState` (which doesn't work in the renderer process because of
     * Electron's `webSecurity`). This is a hack, but it's the only way to get Amplify to work
     * with a custom URL protocol in Electron.
     *
     * # Safety
     *
     * It is safe to disable the `unbound-method` lint here because we intentionally want to use
     * the original `window.history.replaceState` function, which is not bound to the
     * `window.history` object. */
    // eslint-disable-next-line @typescript-eslint/unbound-method
    const replaceState = window.history.replaceState;
    window.history.replaceState = () => false;
    try {
      /** # Safety
       *
       * It is safe to disable the `no-unsafe-call` lint here
       * because we know that the `Auth` object has the `_handleAuthResponse` method, and we
       * know that it is safe to call it with the `url` argument. There is no way to prove
       * this to the TypeScript compiler, because these methods are intentionally not part of
       * the public AWS Amplify API. */
      // @ts-expect-error `_handleAuthResponse` is a private method without typings.
      // eslint-disable-next-line @typescript-eslint/no-unsafe-call
      await amplify.Auth._handleAuthResponse(url);
    } finally {
      /** Restore the original `window.location.replaceState` function. */
      window.history.replaceState = replaceState;
    }
  })();
}
