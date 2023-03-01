/** @file Provides an {@link AuthService} which consists of an underyling {@link Cognito} API
 * wrapper, along with some convenience callbacks to make URL redirects for the authentication flows
 * work with Electron. */
import * as amplify from "@aws-amplify/auth";
import * as app from "../components/app";

import * as listen from "./listen";
import * as loggerProvider from "../providers/logger";
import * as cognito from "./cognito";
import * as config from "./config";



// =================
// === Constants ===
// =================

const BASE_AMPLIFY_CONFIG: Partial<config.AmplifyConfig> = {
  region: config.AWS_REGION,
  scope: config.OAUTH_SCOPES,
  responseType: config.OAUTH_RESPONSE_TYPE,
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
};



// ================
// === LoginApi ===
// ================

/** `window.loginApi` is a context bridge to the main process, when we're running in an Electron
 * context.
 *
 * # Safety
 *
 * We're assuming that the main process has exposed the `loginApi` context bridge (see
 * `lib/client/src/preload.ts` for details), and that it contains the functions defined in this
 * interface. Our app can't function if these assumptions are not met, so we're disabling the
 * TypeScript checks for this interface when we use it. */
interface LoginApi {
  /** Open a URL in the system browser. */
  openExternalUrl: (url: string) => void;
  /** Set the callback to be called when the system browser redirects back to a URL in the app,
   * via a deep link. See {@link registerOpenAuthenticationUrlCallback} for details. */
  setOpenAuthenticationUrlCallback: (callback: (url: string) => void) => void;
}



// ==================
// === AuthConfig ===
// ==================

/** Configuration for the authentication service. */
export interface AuthConfig {
  /** Logger for the authentication service. */
  logger: loggerProvider.Logger;
  /** Whether the application is running on a desktop (i.e., versus in the Cloud). */
  runningOnDesktop: boolean;
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
  /** @see {@link Cognito} */
  cognito: cognito.Cognito;
  /** @see {@link listen.ListenFunction} */
  registerAuthEventListener: listen.ListenFunction;
}

/** Creates an instance of the authentication service.
 *
 * # Warning
 *
 * This function should only be called once, and the returned service should be used throughout the
 * application. This is because it performs global configuration of the Amplify library.
 *
 * @param authConfig - Configuration for the authentication service.
 * @returns An instance of the authentication service. */
export const initAuthService = (authConfig: AuthConfig): AuthService => {
  const { logger, runningOnDesktop, navigate } = authConfig;

  const amplifyConfig = loadAmplifyConfig(logger, runningOnDesktop, navigate);
  const cognitoClient = new cognito.CognitoImpl(
    logger,
    runningOnDesktop,
    amplifyConfig
  );

  return {
    cognito: cognitoClient,
    registerAuthEventListener: listen.registerAuthEventListener,
  };
};

const loadAmplifyConfig = (
  logger: loggerProvider.Logger,
  runningOnDesktop: boolean,
  navigate: (url: string) => void
): config.AmplifyConfig => {
  // Load the environment-specific Amplify configuration.
  const baseConfig = AMPLIFY_CONFIGS.pbuchu;

  if (runningOnDesktop) {
    // If we're running on the desktop, we want to override the default URL opener for OAuth
    // flows.  This is because the default URL opener opens the URL in the desktop app itself,
    // but we want the user to be sent to their system browser instead. The user should be sent
    // to their system browser because:
    //
    // - users trust their system browser with their credentials more than they trust our app;
    // - our app can keep itself on the relevant page until the user is sent back to it (i.e.,
    //   we avoid unnecessary reloads/refreshes caused by redirects.
    baseConfig.urlOpener = openUrlWithExternalBrowser;

    // To handle redirects back to the application from the system browser, we also need to
    // register a custom URL handler.
    registerOpenAuthenticationUrlCallback(logger, navigate);
  }

  // Set the redirect URLs for the OAuth flows, depending on our environment.
  baseConfig.redirectSignIn = runningOnDesktop
    ? config.DESKTOP_REDIRECT
    : config.CLOUD_REDIRECT;
  baseConfig.redirectSignOut = runningOnDesktop
    ? config.DESKTOP_REDIRECT
    : config.CLOUD_REDIRECT;

  return baseConfig as config.AmplifyConfig;
};

const openUrlWithExternalBrowser = (url: string) => {
  /** See {@link LoginApi} for safety details. */
  // @ts-expect-error
  // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
  const loginApi: LoginApi = window.loginApi;
  loginApi.openExternalUrl(url);
};

/** Register the callback that will be invoked when an `enso://` schema URL is opened in the app.
 *
 * Typically this callback is invoked when the user is redirected back to the app after:
 *
 * 1. authenticating with a federated identity provider; or
 * 2. clicking a "reset password" link in a password reset email.
 *
 * This is only used when running on the desktop, as the browser version of the app lets Amplify
 * handle the redirect for us. On the desktop however, we need to handle the redirect ourselves,
 * because it's a deep link into the app, and Amplify doesn't handle deep links. */
const registerOpenAuthenticationUrlCallback = (
  logger: loggerProvider.Logger,
  navigate: (url: string) => void
) => {
  const openAuthenticationUrlCallback = (url: string) => {
    const parsedUrl = new URL(url);

    if (isConfirmRegistrationRedirect(parsedUrl)) {
      // Navigate to a relative URL to handle the confirmation link.
      const redirectUrl = `${app.CONFIRM_REGISTRATION_PATH}${parsedUrl.search}`;
      navigate(redirectUrl);
    } else if (isSignOutRedirect(parsedUrl)) {
      navigate(app.LOGIN_PATH);
    } else if (isSignInRedirect(parsedUrl)) {
      handleAuthResponse(url);
      // If the user is being redirected from a password reset email, then we need to navigate to
      // the password reset page, with the verification code and email passed in the URL so they
      // can be filled in automatically.
    } else if (isResetPasswordRedirect(parsedUrl)) {
      // Navigate to a relative URL to handle the password reset.
      const redirectUrl = `${app.RESET_PASSWORD_PATH}${parsedUrl.search}`;
      navigate(redirectUrl);
    }
  };

  /** See {@link LoginApi} for safety details. */
  // @ts-expect-error
  // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
  const loginApi: LoginApi = window.loginApi;
  loginApi.setOpenAuthenticationUrlCallback(openAuthenticationUrlCallback);
};

/** If the user is being redirected after clicking the registration confirmation link in their
 * email, then the URL will be for the confirmation page path. */
const isConfirmRegistrationRedirect = (url: URL) =>
  url.pathname === app.CONFIRM_REGISTRATION_PATH;

/** If the user is being redirected after a sign-out, then no query args will be present. */
// FIXME [NP2]: don't use `enso://auth` for both authentication redirect & signout redirect so we
// don't have to disambiguate between the two on the `DASHBOARD_PATH`.
const isSignOutRedirect = (url: URL) =>
  url.pathname === app.DASHBOARD_PATH && url.search === "";

/** If the user is being redirected after a sign-out, then query args will be present. */
const isSignInRedirect = (url: URL) =>
  url.pathname === app.DASHBOARD_PATH && url.search !== "";

/** If the user is being redirected after clicking the reset password confirmation link in their
 * email, then the URL will be for the confirm password reset path. */
const isResetPasswordRedirect = (url: URL) =>
  url.pathname === app.RESET_PASSWORD_PATH;

/** When the user is being redirected from a federated identity provider, then we need to pass the
 * URL to the Amplify library, which will parse the URL and complete the OAuth flow. */
const handleAuthResponse = (url: string) => {
  void (async () => {
    // Temporarily override the `window.location` object so that Amplify doesn't try
    // to call `window.location.replaceState` (which doesn't work in the renderer
    // process because of Electron's `webSecurity`). This is a hack, but it's the
    // only way to get Amplify to work with a custom URL protocol in Electron.
    //
    // Note that this entire hack must happen within an async IIFE block, because we
    // need to be sure to restore the original `window.location.replaceState`
    // function before any non-Amplify code runs, which we can't guarantee if
    // `_handleAuthResponse` is allowed to complete asynchronously.
    //
    // # Safety
    //
    // It is safe to disable the `unbound-method` lint here because we intentionally want to use
    // the original `window.history.replaceState` function, which is not bound to the
    // `window.history` object.
    // eslint-disable-next-line @typescript-eslint/unbound-method
    const replaceState = window.history.replaceState;
    window.history.replaceState = () => false;
    try {
      // # Safety
      //
      // It is safe to disable the `no-unsafe-member-access` and `no-unsafe-call` lints here
      // because we know that the `Auth` object has the `_handleAuthResponse` method, and we
      // know that it is safe to call it with the `url` argument. There is no way to prove
      // this to the TypeScript compiler, because these methods are intentionally not part of
      // the public AWS Amplify API.
      // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
      await (amplify.Auth as any)._handleAuthResponse(url);
    } finally {
      // Restore the original `window.location.replaceState` function.
      window.history.replaceState = replaceState;
    }
  })();
};
