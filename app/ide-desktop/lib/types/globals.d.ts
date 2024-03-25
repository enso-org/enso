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
    readonly saveAccessToken: (accessToken: SaveAccessTokenPayload | null) => void
}

// ======================
// === Navigation API ===
// ======================

/** `window.navigationApi` is a context bridge to the main process, when we're running in an
 * Electron context. It contains navigation-related functionality. */
interface NavigationApi {
    /** Go back in the navigation history. */
    readonly goBack: () => void
    /** Go forward in the navigation history. */
    readonly goForward: () => void
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
        readonly navigationApi: NavigationApi
    }

    namespace NodeJS {
        /** Environment variables. */
        // `TZ` MUST NOT be `readonly`, or else `@types/node` will error.
        // eslint-disable-next-line no-restricted-syntax
        interface ProcessEnv {
            readonly [key: string]: never
            // These are environment variables, and MUST be in CONSTANT_CASE.
            /* eslint-disable @typescript-eslint/naming-convention */
            // This is declared in `@types/node`. It MUST be re-declared here to suppress the error
            // about this property conflicting with the index signature above.
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            TZ?: string
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly CI?: string
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly CSC_LINK?: string
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly APPLEID?: string
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly APPLEIDPASS?: string
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly APPLETEAMID?: string
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly ENSO_BUILD_ICONS?: string
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly npm_package_name?: string

            // === Cloud environment variables ===

            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly ENSO_CLOUD_REDIRECT: string
            // When unset, the `.env` loader tries to load `.env` rather than `.<name>.env`.
            // Set to the empty string to load `.env`.
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly ENSO_CLOUD_ENVIRONMENT: string
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly ENSO_CLOUD_API_URL?: string
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly ENSO_CLOUD_CHAT_URL?: string
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly ENSO_CLOUD_SENTRY_DSN?: string
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly ENSO_CLOUD_STRIPE_KEY?: string
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly ENSO_CLOUD_COGNITO_USER_POOL_ID?: string
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID?: string
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly ENSO_CLOUD_COGNITO_DOMAIN?: string
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly ENSO_CLOUD_COGNITO_REGION?: string
            // @ts-expect-error The index signature is intentional to disallow unknown env vars.
            readonly ENSO_SUPPORTS_VIBRANCY?: string
            /* eslint-enable @typescript-eslint/naming-convention */
        }
    }

    // These are used in other files (because they're globals)
    /* eslint-disable @typescript-eslint/naming-convention */
    const BUNDLED_ENGINE_VERSION: string
    const BUILD_INFO: buildJson.BuildInfo
    const PROJECT_MANAGER_IN_BUNDLE_PATH: string
    const IS_VITE: boolean
}
