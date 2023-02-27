// FIXME [NP]: find and resolve all typescript/eslint errors (including silenced ones)
import { Auth } from "@aws-amplify/auth";
import { CONFIRM_REGISTRATION_PATH, LOGIN_PATH, RESET_PASSWORD_PATH } from "../components/app";
import registerAuthEventListener, { ListenFunction } from "./listen";
import { Logger } from "../providers/logger";
import { Cognito, CognitoImpl } from "./cognito";
import { AmplifyConfig, AWS_REGION, CLOUD_REDIRECT, DESKTOP_REDIRECT, OAUTH_RESPONSE_TYPE, OAUTH_SCOPES } from "./config";



// =================
// === Constants ===
// =================

const BASE_AMPLIFY_CONFIG: Partial<AmplifyConfig> = {
    region: AWS_REGION,
    scope: OAUTH_SCOPES,
    responseType: OAUTH_RESPONSE_TYPE,
}

/** Collection of configuration details for Amplify user pools, sorted by deployment environment. */
const AMPLIFY_CONFIGS = {
    /** Configuration for @pbuchu's Cognito user pool. */
    pbuchu: {
        userPoolId: "eu-west-1_jSF1RbgPK",
        userPoolWebClientId: "1bnib0jfon3aqc5g3lkia2infr",
        domain: "pb-enso-domain.auth.eu-west-1.amazoncognito.com",
        ...BASE_AMPLIFY_CONFIG,
    },
}



// ==================
// === AuthConfig ===
// ==================


// === AuthConfig ===

/** Configuration for the authentication service. */
export interface AuthConfig {
    /** Logger for the authentication service. */
    logger: Logger;
    /** Whether the application is running on a desktop (i.e., versus in the Cloud). */
    runningOnDesktop: boolean;
    /**
     * Function to navigate to a given (relative) URL.
     *
     * Used to redirect to pages like the password reset page with the query parameters set in the
     * URL (e.g., `?verification_code=...`).
     */
    navigate: (url: string) => void;
}



// ===================
// === AuthService ===
// ===================

/**
 * API for the authentication service.
 */
export interface AuthService {
    // FIXME [NP]: docs
    cognito: Cognito,
    /** @see {@link ListenFunction} */
    registerAuthEventListener: ListenFunction;
}

/**
 * Creates an instance of the authentication service.
 * 
 * # Warning
 * 
 * This function should only be called once, and the returned service should be used throughout the
 * application. This is because it performs global configuration of the Amplify library.
 * 
 * @param authConfig - Configuration for the authentication service.
 * @returns An instance of the authentication service.
 */
// FIXME [NP]: make this type safe
const authService = (authConfig: AuthConfig): AuthService => {
    const { logger, runningOnDesktop, navigate } = authConfig;

    const amplifyConfig = loadAmplifyConfig(logger, runningOnDesktop, navigate);
    const cognito = new CognitoImpl(logger, runningOnDesktop, amplifyConfig);

    return {
        cognito,
        registerAuthEventListener,
    }
}

const loadAmplifyConfig = (logger: Logger, runningOnDesktop: boolean, navigate: (url: string) => void): AmplifyConfig => {
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
    baseConfig.redirectSignIn = runningOnDesktop ? DESKTOP_REDIRECT : CLOUD_REDIRECT;
    baseConfig.redirectSignOut = runningOnDesktop ? DESKTOP_REDIRECT : CLOUD_REDIRECT;

    return baseConfig as AmplifyConfig
}

// Ensure that you have the `loginApi` context bridge exposed in the main process. See the
// `exposeLoginApi` function for more details.
// FIXME [NP]: is the above doc OK?
const openUrlWithExternalBrowser = (url: string) => {
    // # Safety
    //
    // We're using `window.loginApi` here, which is a context bridge to the main process.
    // We're assuming that the main process has exposed the `loginApi` context bridge, and
    // that it contains the `openExternalUrl` function.
    // @ts-expect-error
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
    window.loginApi.openExternalUrl(url)
}

/**
 * Register the callback that will be invoked when an `enso://` schema URL is opened in the app.
 * 
 * Typically this callback is invoked when the user is redirected back to the app after:
 *
 * 1. authenticating with a federated identity provider; or
 * 2. clicking a "reset password" link in a password reset email.
 *
 * This is only used when running on the desktop, as the browser version of the app lets Amplify
 * handle the redirect for us. On the desktop however, we need to handle the redirect ourselves,
 * because it's a deep link into the app, and Amplify doesn't handle deep links.
 */
// FIXME [NP]: type this?
const registerOpenAuthenticationUrlCallback = (
    logger: Logger,
    navigate: (url: string) => void,
) => {
    const openAuthenticationUrlCallback = (url: string) => {
        const parsedUrl = new URL(url)
        // FIXME [NP]: remove log
        logger.log("openAuthenticationUrlCallback::URL::", url)

        // FIXME [NP]: constantize
        if (parsedUrl.pathname === "/confirmation") {
            // Navigate to a relative URL to handle the confirmation link.
            const redirectUrl = `${CONFIRM_REGISTRATION_PATH}${parsedUrl.search}`
            navigate(redirectUrl)
        // If the user is being redirected from a federated identity provider, then we need to pass
        // the URL to the Amplify library, which will parse the URL and complete the OAuth flow.
        // FIXME [NP]: constantize
        // FIXME [NP]: pass the URL as a path, not a host
        } else if (parsedUrl.pathname === "/") {
            // FIXME [NP]: don't use `enso://auth` for both authentication redirect & signout redirect so we don't have to disambiguate here.
            if (parsedUrl.search === "") {
                logger.log("FIXME [NP]: why does the signout navigate not wor?")
                navigate(LOGIN_PATH)
            } else {
                // FIXME [NP]: remove this log
                logger.log("authenticatedRedirectCallback::Current URL::", window.location.href);
                logger.log("authenticatedRedirectCallback::URL::", url);
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
                    // eslint-disable-next-line @typescript-eslint/unbound-method
                    const replaceState = window.history.replaceState;
                    window.history.replaceState = () => false;
                    try {
                        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
                        await (Auth as any)._handleAuthResponse(url)
                    } finally {
                        // Restore the original `window.location.replaceState` function.
                        window.history.replaceState = replaceState;
                    }
                })()
            }
        // If the user is being redirected from a password reset email, then we need to navigate to
        // the password reset page, with the verification code and email passed in the URL so they
        // can be filled in automatically.
        // FIXME [NP]: constantize
        // FIXME [NP]: change from password-reset to reset-password
        } else if (parsedUrl.pathname === "/password-reset") {
            // Navigate to a relative URL to handle the password reset.
            const redirectUrl = `${RESET_PASSWORD_PATH}${parsedUrl.search}`
            navigate(redirectUrl)
        }
    }

    // @ts-expect-error
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
    window.loginApi.setOpenAuthenticationUrlCallback(openAuthenticationUrlCallback)
}




export default authService
