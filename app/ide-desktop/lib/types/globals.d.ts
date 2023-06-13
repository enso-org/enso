/** @file Globals defined outside of TypeScript files.
 * These are from variables defined at build time, environment variables,
 * monkeypatching on `window` and generated code. */
// This file is being imported for its types.
// eslint-disable-next-line no-restricted-syntax
import * as buildJson from './build.json' assert { type: 'json' }

// =============
// === Types ===
// =============

/** Nested configuration options with `string` values. */
interface StringConfig {
    [key: string]: StringConfig | string
}

/** The public interface exposed to `window` by the IDE. */
interface Enso {
    main: (inputConfig?: StringConfig) => Promise<void>
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

// =====================================
// === Global namespace augmentation ===
// =====================================

// JSDocs here are intentionally empty as these interfaces originate from elsewhere.
declare global {
    /** */
    interface Window {
        enso?: AppRunner & Enso
        authenticationApi: AuthenticationApi
    }

    namespace NodeJS {
        /** */
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
    const BUILD_INFO: buildJson.BuildInfo
    const PROJECT_MANAGER_IN_BUNDLE_PATH: string
    const IS_DEV_MODE: boolean
    // This will be `undefined` when it is not defined by esbuild.
    // eslint-disable-next-line no-restricted-syntax
    const REDIRECT_OVERRIDE: string | undefined
    /* eslint-disable @typescript-eslint/naming-convention */
    // This is a function.
    // eslint-disable-next-line no-restricted-syntax
    const assert: (invariant: boolean, message: string) => void
}
