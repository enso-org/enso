/** @file Globals defined outside of TypeScript files.
 * These are from variables defined at build time, environment variables,
 * monkeypatching on `window` and generated code. */
/// <reference types="vite/client" />

// This file is being imported for its types.
// eslint-disable-next-line no-restricted-syntax, @typescript-eslint/consistent-type-imports
import * as buildJson from './../../build.json' assert { type: 'json' }

// =============
// === Types ===
// =============

/** Nested configuration options with `string` values. */
interface StringConfig {
    [key: string]: StringConfig | string
}

/** The public interface exposed to `window` by the IDE. */
interface Enso {
    readonly main: (inputConfig?: StringConfig) => Promise<void>
}

// ===================
// === Backend API ===
// ===================

/** `window.backendApi` is a context bridge to the main process, when we're running in an
 * Electron context. It contains non-authentication-related functionality. */
interface BackendApi {
    /** Return the ID of the new project. */
    readonly importProjectFromPath: (openedPath: string) => Promise<string>
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
    readonly openUrlInSystemBrowser: (url: string) => void
    /** Set the callback to be called when the system browser redirects back to a URL in the app,
     * via a deep link. See `setDeepLinkHandler` for details. */
    readonly setDeepLinkHandler: (callback: (url: string) => void) => void
    /** Saves the access token to a file. */
    readonly saveAccessToken: (accessToken: string | null) => void
}

// ========================
// === File Browser API ===
// ========================

/** `window.fileBrowserApi` is a context bridge to the main process, when we're running in an
 * Electron context.
 *
 * # Safety
 *
 * We're assuming that the main process has exposed the `fileBrowserApi` context bridge (see
 * `lib/client/src/preload.ts` for details), and that it contains the functions defined in this
 * interface. Our app can't function if these assumptions are not met, so we're disabling the
 * TypeScript checks for this interface when we use it. */
interface FileBrowserApi {
  /** Select path for local file or directory using the system file browser. */
  readonly openFileBrowser: (kind: 'file' | 'directory' | 'any') => Promise<string[] | undefined>
}

// =====================================
// === Global namespace augmentation ===
// =====================================

// JSDocs here are intentionally empty as these interfaces originate from elsewhere.
declare global {
    // Documentation is already inherited.
    /** */
    interface Window {
        readonly enso?: AppRunner & Enso
        readonly backendApi?: BackendApi
        readonly authenticationApi: AuthenticationApi
        readonly fileBrowserApi: FileBrowserApi
    }

    namespace NodeJS {
        /** Environment variables. */
        interface ProcessEnv {
            // These are environment variables, and MUST be in CONSTANT_CASE.
            /* eslint-disable @typescript-eslint/naming-convention */
            readonly APPLEID?: string
            readonly APPLEIDPASS?: string
            readonly APPLETEAMID?: string
            /* eslint-enable @typescript-eslint/naming-convention */
        }
    }

    // These are used in other files (because they're globals)
    /* eslint-disable @typescript-eslint/naming-convention */
    const BUNDLED_ENGINE_VERSION: string
    const BUILD_INFO: buildJson.BuildInfo
    const PROJECT_MANAGER_IN_BUNDLE_PATH: string
    // This will be `undefined` when it is not defined by esbuild.
    // eslint-disable-next-line no-restricted-syntax
    const REDIRECT_OVERRIDE: string | undefined
    const IS_VITE: boolean
    // eslint-disable-next-line no-restricted-syntax
    const CLOUD_ENV: 'npekin' | 'pbuchu' | 'production' | undefined
}
