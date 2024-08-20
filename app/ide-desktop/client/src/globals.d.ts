/** @file Globals defined outside of TypeScript files.
 * These are from variables defined at build time, environment variables,
 * monkeypatching on `window` and generated code. */
import type * as dashboard from 'enso-dashboard'

// This file is being imported for its types.
// eslint-disable-next-line no-restricted-syntax
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
  readonly importProjectFromPath: (
    openedPath: string,
    directory: string | null,
    name: string,
  ) => Promise<string>
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
  readonly saveAccessToken: (accessToken: dashboard.AccessToken | null) => void
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

// ================
// === Menu API ===
// ================

/** `window.menuApi` exposes functionality related to the system menu. */
interface MenuApi {
  /** Set the callback to be called when the "about" entry is clicked in the "help" menu. */
  readonly setShowAboutModalHandler: (callback: () => void) => void
}

// ==================
// === System API ===
// ==================

/** `window.systemApi` exposes functionality related to the operating system. */
interface SystemApi {
  readonly downloadURL: (url: string, headers?: Record<string, string>) => void
  readonly showItemInFolder: (fullPath: string) => void
}

// ========================
// === File Browser API ===
// ========================

/** `window.fileBrowserApi` exposes functionality related to the system's default file picker. */
interface FileBrowserApi {
  readonly openFileBrowser: (
    kind: 'any' | 'directory' | 'file' | 'filePath',
    defaultPath?: string,
  ) => Promise<unknown>
}

// ==============================
// === Project Management API ===
// ==============================

/** Metadata for a newly imported project. */
interface ProjectInfo {
  readonly id: string
  readonly name: string
  readonly parentDirectory: string
}

/** `window.projectManagementApi` exposes functionality related to system events related to
 * project management. */
interface ProjectManagementApi {
  readonly setOpenProjectHandler: (handler: (projectInfo: ProjectInfo) => void) => void
}

// ====================
// === Version Info ===
// ====================

/** Versions of the app, and selected software bundled with Electron. */
interface VersionInfo {
  readonly version: string
  readonly build: string
  readonly electron: string
  readonly chrome: string
}

// =====================================
// === Global namespace augmentation ===
// =====================================

// JSDocs here are intentionally empty as these interfaces originate from elsewhere.
declare global {
  // Documentation is already inherited.
  /** */
  // eslint-disable-next-line no-restricted-syntax
  interface Window {
    readonly backendApi?: BackendApi
    readonly authenticationApi: AuthenticationApi
    readonly navigationApi: NavigationApi
    readonly menuApi: MenuApi
    readonly systemApi?: SystemApi
    readonly fileBrowserApi?: FileBrowserApi
    readonly projectManagementApi?: ProjectManagementApi
    readonly versionInfo?: VersionInfo
    toggleDevtools: () => void
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
      readonly PROD?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly CSC_LINK?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly APPLEID?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly APPLEIDPASS?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly APPLETEAMID?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly ENSO_BUILD_ELECTRON_BUILDER_CONFIG?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly npm_package_name?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly PROJECT_MANAGER_IN_BUNDLE_PATH: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly ENSO_SUPPORTS_VIBRANCY?: string

      // === E2E test variables ===

      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly IS_IN_PLAYWRIGHT_TEST?: `${boolean}`
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly PWDEBUG?: '1'

      // === Electron watch script variables ===

      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly ELECTRON_DEV_MODE?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly GUI_CONFIG_PATH?: string
      /* eslint-enable @typescript-eslint/naming-convention */
    }
  }

  // These are used in other files (because they're globals)
  /* eslint-disable @typescript-eslint/naming-convention */
  const BUILD_INFO: buildJson.BuildInfo
}
