/**
 * @file Preload script containing code that runs before web page is loaded into the browser
 * window. It has access to both DOM APIs and Node environment, and is used to expose privileged
 * APIs to the renderer via the contextBridge API. To learn more, visit:
 * https://www.electronjs.org/docs/latest/tutorial/tutorial-preload.
 */

import type * as accessToken from 'enso-common/src/accessToken'

import * as debug from '@/debug'
import * as ipc from '@/ipc'
import type * as projectManagement from '@/projectManagement'

// Even though this is already built as an mjs module, we are "faking" cjs format on preload script
// due to missing module support. Since this is the only module that's treated as external by
// esbuild, we have to manually use "require". Switch this to an import once new electron version
// actually honours ".mjs" files for sandboxed preloading (this will likely become an error at that time).
// https://www.electronjs.org/fr/docs/latest/tutorial/esm#sandboxed-preload-scripts-cant-use-esm-imports
// eslint-disable-next-line @typescript-eslint/no-require-imports
const electron = require('electron')

// =================
// === Constants ===
// =================

const BACKEND_API_KEY = 'backendApi'
const AUTHENTICATION_API_KEY = 'authenticationApi'
const FILE_BROWSER_API_KEY = 'fileBrowserApi'
const PROJECT_MANAGEMENT_API_KEY = 'projectManagementApi'
const NAVIGATION_API_KEY = 'navigationApi'
const MENU_API_KEY = 'menuApi'
const SYSTEM_API_KEY = 'systemApi'
const VERSION_INFO_KEY = 'versionInfo'

// =========================
// === exposeInMainWorld ===
// =========================

/** A type-safe wrapper around {@link electron.contextBridge.exposeInMainWorld}. */
function exposeInMainWorld<Key extends string & keyof typeof window>(
  key: Key,
  value: NonNullable<(typeof window)[Key]>,
) {
  electron.contextBridge.exposeInMainWorld(key, value)
}

// =============================
// === importProjectFromPath ===
// =============================

const IMPORT_PROJECT_RESOLVE_FUNCTIONS = new Map<
  string,
  (projectId: projectManagement.ProjectInfo) => void
>()

exposeInMainWorld(BACKEND_API_KEY, {
  importProjectFromPath: (projectPath: string, directory: string | null = null, title: string) => {
    electron.ipcRenderer.send(ipc.Channel.importProjectFromPath, projectPath, directory, title)
    return new Promise<projectManagement.ProjectInfo>(resolve => {
      IMPORT_PROJECT_RESOLVE_FUNCTIONS.set(projectPath, resolve)
    })
  },
})

exposeInMainWorld(NAVIGATION_API_KEY, {
  goBack: () => {
    electron.ipcRenderer.send(ipc.Channel.goBack)
  },
  goForward: () => {
    electron.ipcRenderer.send(ipc.Channel.goForward)
  },
})

electron.ipcRenderer.on(
  ipc.Channel.importProjectFromPath,
  (_event, projectPath: string, projectInfo: projectManagement.ProjectInfo) => {
    const resolveFunction = IMPORT_PROJECT_RESOLVE_FUNCTIONS.get(projectPath)
    IMPORT_PROJECT_RESOLVE_FUNCTIONS.delete(projectPath)
    resolveFunction?.(projectInfo)
  },
)

// ==========================
// === Authentication API ===
// ==========================

/** A callback called when a deep link is opened. */
type OpenDeepLinkHandler = (url: string) => void

let deepLinkHandler: OpenDeepLinkHandler | null = null

electron.ipcRenderer.on(
  ipc.Channel.openDeepLink,
  (_event: Electron.IpcRendererEvent, ...args: Parameters<OpenDeepLinkHandler>) => {
    deepLinkHandler?.(...args)
  },
)

/**
 * Object exposed on the Electron main window; provides proxy functions to:
 * - open OAuth flows in the system browser, and
 * - handle deep links from the system browser or email client to the dashboard.
 *
 * Some functions (i.e., the functions to open URLs in the system browser) are not available in
 * sandboxed processes (i.e., the dashboard). So the
 * {@link exposeInMainWorld} API is used to expose these functions.
 * The functions are exposed via this "API object", which is added to the main window.
 *
 * For more details, see:
 * https://www.electronjs.org/docs/latest/api/context-bridge#api-functions.
 */
exposeInMainWorld(AUTHENTICATION_API_KEY, {
  /**
   * Open a URL in the system browser (rather than in the app).
   *
   * OAuth URLs must be opened this way because the dashboard application is sandboxed and thus
   * not privileged to do so unless we explicitly expose this functionality.
   */
  openUrlInSystemBrowser: (url: string) => {
    electron.ipcRenderer.send(ipc.Channel.openUrlInSystemBrowser, url)
  },
  /**
   * Set the callback that will be called when a deep link to the application is opened.
   *
   * The callback is intended to handle links like
   * `enso://authentication/register?code=...&state=...` from external sources like the user's
   * system browser or email client. Handling the links involves resuming whatever flow was in
   * progress when the link was opened (e.g., an OAuth registration flow).
   */
  setDeepLinkHandler: (callback: (url: string) => void) => {
    deepLinkHandler = callback
  },
  /**
   * Save the access token to a credentials file.
   *
   * The backend doesn't have access to Electron's `localStorage` so we need to save access token
   * to a file. Then the token will be used to sign cloud API requests.
   */
  saveAccessToken: (accessTokenPayload: accessToken.AccessToken | null) => {
    electron.ipcRenderer.send(ipc.Channel.saveAccessToken, accessTokenPayload)
  },
})

// ========================
// === File Browser API ===
// ========================

exposeInMainWorld(FILE_BROWSER_API_KEY, {
  openFileBrowser: (kind: 'any' | 'directory' | 'file' | 'filePath', defaultPath?: string) =>
    electron.ipcRenderer.invoke(ipc.Channel.openFileBrowser, kind, defaultPath),
})

// ==============================
// === Project management API ===
// ==============================

/** A callback when a project is opened by opening a fileusing the system's default method. */
type OpenProjectHandler = (projectInfo: projectManagement.ProjectInfo) => void
let openProjectHandler: OpenProjectHandler | undefined

electron.ipcRenderer.on(
  ipc.Channel.openProject,
  (_event: Electron.IpcRendererEvent, ...args: Parameters<OpenProjectHandler>) => {
    openProjectHandler?.(...args)
  },
)

exposeInMainWorld(PROJECT_MANAGEMENT_API_KEY, {
  setOpenProjectHandler: (handler: (projectInfo: projectManagement.ProjectInfo) => void) => {
    openProjectHandler = handler
  },
})

// ================
// === Menu API ===
// ================

let showAboutModalHandler: (() => void) | null = null

electron.ipcRenderer.on(ipc.Channel.showAboutModal, () => {
  showAboutModalHandler?.()
})

exposeInMainWorld(MENU_API_KEY, {
  setShowAboutModalHandler: (callback: () => void) => {
    showAboutModalHandler = callback
  },
})

// ==================
// === System API ===
// ==================

exposeInMainWorld(SYSTEM_API_KEY, {
  downloadURL: (url: string, headers?: Record<string, string>) => {
    electron.ipcRenderer.send(ipc.Channel.downloadURL, url, headers)
  },
  showItemInFolder: (fullPath: string) => {
    electron.ipcRenderer.send(ipc.Channel.showItemInFolder, fullPath)
  },
})

// ====================
// === Version info ===
// ====================

exposeInMainWorld(VERSION_INFO_KEY, debug.VERSION_INFO)
