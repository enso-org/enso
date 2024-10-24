/**
 * @file Globals defined outside of TypeScript files.
 * These are from variables defined at build time, environment variables,
 * monkeypatching on `window` and generated code.
 */
/// <reference types="vite/client" />
import type * as saveAccessToken from 'enso-common/src/accessToken'

// prettier-ignore
import * as buildJson from '../../build.json' with { type: 'json' };

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

/**
 * `window.backendApi` is a context bridge to the main process, when we're running in an
 * Electron context. It contains non-authentication-related functionality.
 */
interface BackendApi {
  /** Return the ID of the new project. */
  readonly importProjectFromPath: (
    openedPath: string,
    directory: string | null,
    name: string,
  ) => Promise<ProjectInfo>
}

// ==========================
// === Authentication API ===
// ==========================

/**
 * `window.authenticationApi` is a context bridge to the main process, when we're running in an
 * Electron context.
 *
 * # Safety
 *
 * We're assuming that the main process has exposed the `authenticationApi` context bridge (see
 * `lib/client/src/preload.ts` for details), and that it contains the functions defined in this
 * interface. Our app can't function if these assumptions are not met, so we're disabling the
 * TypeScript checks for this interface when we use it.
 */
interface AuthenticationApi {
  /** Open a URL in the system browser. */
  readonly openUrlInSystemBrowser: (url: string) => void
  /**
   * Set the callback to be called when the system browser redirects back to a URL in the app,
   * via a deep link. See `setDeepLinkHandler` for details.
   */
  readonly setDeepLinkHandler: (callback: (url: string) => void) => void
  /** Saves the access token to a file. */
  readonly saveAccessToken: (accessToken: saveAccessToken.AccessToken | null) => void
}

// ======================
// === Navigation API ===
// ======================

/**
 * `window.navigationApi` is a context bridge to the main process, when we're running in an
 * Electron context. It contains navigation-related functionality.
 */
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

// ==============================
// === Project Management API ===
// ==============================

/** Metadata for a newly imported project. */
interface ProjectInfo {
  readonly id: string
  readonly name: string
  readonly parentDirectory: string
}

/**
 * `window.projectManagementApi` exposes functionality related to system events related to
 * project management.
 */
interface ProjectManagementApi {
  readonly setOpenProjectHandler: (handler: (projectInfo: ProjectInfo) => void) => void
}

// ========================
// === File Browser API ===
// ========================

/**
 * `window.fileBrowserApi` is a context bridge to the main process, when we're running in an
 * Electron context.
 *
 * # Safety
 *
 * We're assuming that the main process has exposed the `fileBrowserApi` context bridge (see
 * `app/client/src/preload.ts` for details), and that it contains the functions defined in this
 * interface.
 */
interface FileBrowserApi {
  /**
   * Select path for local file or directory using the system file browser.
   * 'filePath' is same as 'file', but allows picking non-existing files.
   */
  readonly openFileBrowser: (
    kind: 'default' | 'directory' | 'file' | 'filePath',
    defaultPath?: string,
  ) => Promise<string[] | undefined>
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
  interface Window {
    readonly backendApi?: BackendApi
    readonly authenticationApi: AuthenticationApi
    readonly navigationApi: NavigationApi
    readonly menuApi: MenuApi
    readonly systemApi?: SystemApi
    readonly projectManagementApi?: ProjectManagementApi
    readonly fileBrowserApi?: FileBrowserApi
    readonly versionInfo?: VersionInfo
    toggleDevtools: () => void
  }

  namespace NodeJS {
    /** Environment variables. */

    interface ProcessEnv {
      readonly [key: string]: never

      // This is declared in `@types/node`. It MUST be re-declared here to suppress the error
      // about this property conflicting with the index signature above.
      // MUST NOT be `readonly`, or else `@types/node` will error.
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      TZ?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly CI?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly PROD?: string

      // === Cloud environment variables ===

      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly ENSO_CLOUD_REDIRECT?: string
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
      readonly ENSO_CLOUD_COGNITO_USER_POOL_ID: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly ENSO_CLOUD_COGNITO_USER_POOL_WEB_CLIENT_ID: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly ENSO_CLOUD_COGNITO_DOMAIN: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly ENSO_CLOUD_COGNITO_REGION: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly ENSO_CLOUD_GOOGLE_ANALYTICS_TAG?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly ENSO_CLOUD_DASHBOARD_VERSION?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly ENSO_CLOUD_DASHBOARD_COMMIT_HASH?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly ENSO_CLOUD_ENSO_HOST?: string

      // === E2E test variables ===
      readonly PWDEBUG?: '1'
      readonly IS_IN_PLAYWRIGHT_TEST?: `${boolean}`

      // === Electron watch script variables ===

      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly ELECTRON_DEV_MODE?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly GUI_CONFIG_PATH?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly NODE_MODULES_PATH?: string
    }
  }

  // These are used in other files (because they're globals)
  const BUILD_INFO: buildJson.BuildInfo
  const PROJECT_MANAGER_IN_BUNDLE_PATH: StringConstructor
  const PROJECT_MANAGER_URL: string | undefined
  const YDOC_SERVER_URL: string | undefined
  const IS_CLOUD_BUILD: boolean

  interface Document {
    caretPositionFromPoint(x: number, y: number): { offsetNode: Node; offset: number } | null
  }

  interface LogEvent {
    (message: string, projectId?: string | null, metadata?: object | null): void
  }
}
