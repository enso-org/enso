/** @file Provides an {@link AuthService} which consists of an underyling {@link cognito.Cognito}
 * API wrapper, along with some convenience callbacks to make URL redirects for the authentication
 * flows work with Electron. */

import * as common from "enso-studio-common";

import * as loggerProvider from "../providers/logger";
import * as cognito from "./cognito";
import * as authConfig from "./config";
import * as config from "../config";
import * as app from "../components/app";

/** Augment the built-in window object with the `authenticationApi` context bridge. This makes it so
 * that accessing authentication-related functionality from the renderer process is as simple as
 * calling `window.authenticationApi`. */
declare global {
  interface Window {
    authenticationApi: AuthenticationApi;
  }
}

// =================
// === Constants ===
// =================

/** Pathname of the {@link URL} for deep links to the registration confirmation page, after a
 * redirect from an account verification email. */
const CONFIRM_REGISTRATION_PATHNAME = "//auth/confirmation";
/** URL used as the OAuth redirect when running in the desktop app. */
const DESKTOP_REDIRECT =
  `${common.DEEP_LINK_SCHEME}://auth` as authConfig.OAuthRedirect;
/** Map from platform to the OAuth redirect URL that should be used for that platform. */
const PLATFORM_TO_CONFIG: Record<
  app.Platform,
  Pick<authConfig.AmplifyConfig, "redirectSignIn" | "redirectSignOut">
> = {
  [app.Platform.desktop]: {
    redirectSignIn: DESKTOP_REDIRECT,
    redirectSignOut: DESKTOP_REDIRECT,
  },
  [app.Platform.cloud]: {
    redirectSignIn: config.ACTIVE_CONFIG.cloudRedirect,
    redirectSignOut: config.ACTIVE_CONFIG.cloudRedirect,
  },
};

const BASE_AMPLIFY_CONFIG: Partial<authConfig.AmplifyConfig> = {
  region: authConfig.AWS_REGION,
  scope: authConfig.OAUTH_SCOPES,
  responseType: authConfig.OAUTH_RESPONSE_TYPE,
};

/** Collection of configuration details for Amplify user pools, sorted by deployment environment. */
const AMPLIFY_CONFIGS = {
  /** Configuration for @pbuchu's Cognito user pool. */
  pbuchu: {
    userPoolId: "eu-west-1_jSF1RbgPK",
    userPoolWebClientId: "1bnib0jfon3aqc5g3lkia2infr",
    domain: "pb-enso-domain.auth.eu-west-1.amazoncognito.com",
    ...BASE_AMPLIFY_CONFIG,
  },
  /** Configuration for the production Cognito user pool. */
  production: {
    userPoolId: "eu-west-1_9Kycu2SbD",
    userPoolWebClientId: "4j9bfs8e7415erf82l129v0qhe",
    domain: "production-enso-domain.auth.eu-west-1.amazoncognito.com",
    ...BASE_AMPLIFY_CONFIG,
  },
};

// ==========================
// === Authentication API ===
// ==========================

/** `window.authenticationApi` is a context bridge to the main process, when we're running in an
 * Electron context.
 *
 * # Safety
 *
 * We're assuming that the main process has exposed the `authenticationApi` context bridge (see
 * `lib/client/src/preload.ts` for details), and that it contains the functions defined in this
 * interface. Our app can't function if these assumptions are not met, so we're disabling the
 * TypeScript checks for this interface when we use it. */
interface AuthenticationApi {
  /** Open a URL in the system browser. */
  openUrlInSystemBrowser: (url: string) => void;
  /** Set the callback to be called when the system browser redirects back to a URL in the app,
   * via a deep link. See {@link setDeepLinkHandler} for details. */
  setDeepLinkHandler: (callback: (url: string) => void) => void;
}

// ==================
// === AuthConfig ===
// ==================

/** Configuration for the authentication service. */
export interface AuthConfig {
  /** Logger for the authentication service. */
  logger: loggerProvider.Logger;
  /** Whether the application is running on a desktop (i.e., versus in the Cloud). */
  platform: app.Platform;
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
  /** @see {@link cognito.Cognito} */
  cognito: cognito.Cognito;
}

/** Creates an instance of the authentication service.
 *
 * # Warning
 *
 * This function should only be called once, and the returned service should be used throughout the
 * application. This is because it performs global configuration of the Amplify library. */
export const initAuthService = (authConfig: AuthConfig): AuthService => {
  const { logger, platform, navigate } = authConfig;
  const amplifyConfig = loadAmplifyConfig(logger, platform, navigate);
  const cognitoClient = new cognito.Cognito(platform, amplifyConfig);
  return { cognito: cognitoClient };
};

const loadAmplifyConfig = (
  logger: loggerProvider.Logger,
  platform: app.Platform,
  navigate: (url: string) => void
): authConfig.AmplifyConfig => {
  /** Load the environment-specific Amplify configuration. */
  const baseConfig = AMPLIFY_CONFIGS[config.ENVIRONMENT];

  if (platform === app.Platform.desktop) {
    /** If we're running on the desktop, we want to override the default URL opener for OAuth
     * flows.  This is because the default URL opener opens the URL in the desktop app itself,
     * but we want the user to be sent to their system browser instead. The user should be sent
     * to their system browser because:
     *
     * - users trust their system browser with their credentials more than they trust our app;
     * - our app can keep itself on the relevant page until the user is sent back to it (i.e.,
     *   we avoid unnecessary reloads/refreshes caused by redirects. */
    baseConfig.urlOpener = openUrlWithExternalBrowser;

    /** To handle redirects back to the application from the system browser, we also need to
     * register a custom URL handler. */
    setDeepLinkHandler(logger, navigate);
  }

  /** Load the platform-specific Amplify configuration. */
  const platformConfig = PLATFORM_TO_CONFIG[platform];
  return { ...baseConfig, ...platformConfig } as authConfig.AmplifyConfig;
};

const openUrlWithExternalBrowser = (url: string) => {
  window.authenticationApi.openUrlInSystemBrowser(url);
};

/** Set the callback that will be invoked when a deep link to the application is opened.
 *
 * Typically this callback is invoked when the user is redirected back to the app after:
 *
 * 1. authenticating with a federated identity provider; or
 * 2. clicking a "reset password" link in a password reset email.
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
const setDeepLinkHandler = (
  logger: loggerProvider.Logger,
  navigate: (url: string) => void
) => {
  const onDeepLink = (url: string) => {
    const parsedUrl = new URL(url);

    if (isConfirmRegistrationRedirect(parsedUrl)) {
      /** Navigate to a relative URL to handle the confirmation link. */
      const redirectUrl = `${app.CONFIRM_REGISTRATION_PATH}${parsedUrl.search}`;
      navigate(redirectUrl);
    } else {
      logger.error(`${url} is an unrecognized deep link. Ignoring.`);
    }
  };

  window.authenticationApi.setDeepLinkHandler(onDeepLink);
};

/** If the user is being redirected after clicking the registration confirmation link in their
 * email, then the URL will be for the confirmation page path. */
const isConfirmRegistrationRedirect = (url: URL) =>
  url.pathname === CONFIRM_REGISTRATION_PATHNAME;
