/**
 * @file Globals defined outside of TypeScript files.
 * These are from variables defined at build time, environment variables,
 * monkeypatching on `window` and generated code.
 */
/// <reference types="vite/client" />
import type * as saveAccessToken from 'enso-common/src/accessToken'
import { $Config } from './src/config'

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
  const $config: $Config

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

  // const PROJECT_MANAGER_IN_BUNDLE_PATH: StringConstructor
  interface Document {
    caretPositionFromPoint(x: number, y: number): { offsetNode: Node; offset: number } | null
  }

  interface LogEvent {
    (message: string, projectId?: string | null, metadata?: object | null): void
  }
}
