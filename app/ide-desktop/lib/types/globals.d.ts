/** @file Globals defined outside of TypeScript files.
 * These are from variables defined at build time, environment variables,
 * monkeypatching on `window` and generated code.
 *
 * This file MUST `export {}` for the globals to be visible to other files. */

interface StringConfig {
    [key: string]: StringConfig | string
}

interface BuildInfo {
    commit: string
    version: string
    engineVersion: string
    name: string
}

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
    openUrlInSystemBrowser: (url: string) => void
    /** Set the callback to be called when the system browser redirects back to a URL in the app,
     * via a deep link. See {@link setDeepLinkHandler} for details. */
    setDeepLinkHandler: (callback: (url: string) => void) => void
    /** Saves the access token to a file. */
    saveAccessToken: (access_token: string) => void
}

declare global {
    interface Window {
        tryStopProject: () => void
        runProject: (inputConfig?: StringConfig) => Promise<void>
        authenticationApi: AuthenticationApi
    }

    namespace NodeJS {
        interface ProcessEnv {
            /* eslint-disable @typescript-eslint/naming-convention */
            APPLEID: string
            APPLEIDPASS: string
            /* eslint-enable @typescript-eslint/naming-convention */
        }
    }

    // These are used in other files (because they're globals)
    /* eslint-disable @typescript-eslint/naming-convention */
    const BUNDLED_ENGINE_VERSION: string
    const BUILD_INFO: BuildInfo
    const PROJECT_MANAGER_IN_BUNDLE_PATH: string
    const IS_DEV_MODE: boolean
    /* eslint-disable @typescript-eslint/naming-convention */
}

export {}
