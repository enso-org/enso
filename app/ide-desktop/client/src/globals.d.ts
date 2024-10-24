/**
 * @file Globals defined outside of TypeScript files.
 * These are from variables defined at build time, environment variables,
 * monkeypatching on `window` and generated code.
 */
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
  readonly saveAccessToken: (accessToken: dashboard.AccessToken | null) => void
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

/**
 * `window.projectManagementApi` exposes functionality related to system events related to
 * project management.
 */
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

      // === Integration test variables ===

      readonly ENSO_TEST?: string
      readonly ENSO_TEST_APP_ARGS?: string
      readonly ENSO_TEST_USER?: string
      readonly ENSO_TEST_USER_PASSWORD?: string
      ENSO_TEST_EXEC_PATH?: string

      // === Electron watch script variables ===

      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly ELECTRON_DEV_MODE?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly GUI_CONFIG_PATH?: string
    }
  }

  // These are used in other files (because they're globals)
  const BUILD_INFO: buildJson.BuildInfo
}
