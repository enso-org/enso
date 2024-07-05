/** @file Preload script containing code that runs before web page is loaded into the browser
 * window. It has access to both DOM APIs and Node environment, and is used to expose privileged
 * APIs to the renderer via the contextBridge API. To learn more, visit:
 * https://www.electronjs.org/docs/latest/tutorial/tutorial-preload. */

import * as electron from 'electron'

import * as debug from 'debug'
import * as ipc from 'ipc'

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

// =============================
// === importProjectFromPath ===
// =============================

const IMPORT_PROJECT_RESOLVE_FUNCTIONS = new Map<string, (projectId: string) => void>()

const BACKEND_API = {
    importProjectFromPath: (projectPath: string, directory: string | null = null) => {
        electron.ipcRenderer.send(ipc.Channel.importProjectFromPath, projectPath, directory)
        return new Promise<string>(resolve => {
            IMPORT_PROJECT_RESOLVE_FUNCTIONS.set(projectPath, resolve)
        })
    },
}
electron.contextBridge.exposeInMainWorld(BACKEND_API_KEY, BACKEND_API)

electron.contextBridge.exposeInMainWorld(NAVIGATION_API_KEY, {
    goBack: () => {
        electron.ipcRenderer.send(ipc.Channel.goBack)
    },
    goForward: () => {
        electron.ipcRenderer.send(ipc.Channel.goForward)
    },
})

electron.ipcRenderer.on(
    ipc.Channel.importProjectFromPath,
    (_event, projectPath: string, projectId: string) => {
        const resolveFunction = IMPORT_PROJECT_RESOLVE_FUNCTIONS.get(projectPath)
        IMPORT_PROJECT_RESOLVE_FUNCTIONS.delete(projectPath)
        resolveFunction?.(projectId)
    }
)

// ==========================
// === Authentication API ===
// ==========================

/** A callback called when a deep link is opened. */
type OpenDeepLinkHandler = (url: string) => void

let deepLinkHandler: OpenDeepLinkHandler | null = null

electron.ipcRenderer.on(
    ipc.Channel.openProject,
    (_event: Electron.IpcRendererEvent, ...args: Parameters<OpenDeepLinkHandler>) => {
        deepLinkHandler?.(...args)
    }
)

/** Object exposed on the Electron main window; provides proxy functions to:
 * - open OAuth flows in the system browser, and
 * - handle deep links from the system browser or email client to the dashboard.
 *
 * Some functions (i.e., the functions to open URLs in the system browser) are not available in
 * sandboxed processes (i.e., the dashboard). So the
 * {@link electron.contextBridge.exposeInMainWorld} API is used to expose these functions.
 * The functions are exposed via this "API object", which is added to the main window.
 *
 * For more details, see:
 * https://www.electronjs.org/docs/latest/api/context-bridge#api-functions. */
const AUTHENTICATION_API = {
    /** Open a URL in the system browser (rather than in the app).
     *
     * OAuth URLs must be opened this way because the dashboard application is sandboxed and thus
     * not privileged to do so unless we explicitly expose this functionality. */
    openUrlInSystemBrowser: (url: string) => {
        electron.ipcRenderer.send(ipc.Channel.openUrlInSystemBrowser, url)
    },
    /** Set the callback that will be called when a deep link to the application is opened.
     *
     * The callback is intended to handle links like
     * `enso://authentication/register?code=...&state=...` from external sources like the user's
     * system browser or email client. Handling the links involves resuming whatever flow was in
     * progress when the link was opened (e.g., an OAuth registration flow). */
    setDeepLinkHandler: (callback: (url: string) => void) => {
        deepLinkHandler = callback
    },
    /** Save the access token to a credentials file.
     *
     * The backend doesn't have access to Electron's `localStorage` so we need to save access token
     * to a file. Then the token will be used to sign cloud API requests. */
    saveAccessToken: (accessTokenPayload: SaveAccessTokenPayload | null) => {
        electron.ipcRenderer.send(ipc.Channel.saveAccessToken, accessTokenPayload)
    },
}
electron.contextBridge.exposeInMainWorld(AUTHENTICATION_API_KEY, AUTHENTICATION_API)

// ========================
// === File Browser API ===
// ========================

const FILE_BROWSER_API = {
    openFileBrowser: (kind: 'any' | 'directory' | 'file' | 'filePath', defaultPath?: string) =>
        electron.ipcRenderer.invoke(ipc.Channel.openFileBrowser, kind, defaultPath),
}
electron.contextBridge.exposeInMainWorld(FILE_BROWSER_API_KEY, FILE_BROWSER_API)

// ==============================
// === Project management API ===
// ==============================

/** A callback when a project is opened by opening a fileusing the system's default method. */
type OpenProjectHandler = (id: string) => void
let openProjectHandler: OpenProjectHandler | undefined

electron.ipcRenderer.on(
    ipc.Channel.openProject,
    (_event: Electron.IpcRendererEvent, ...args: Parameters<OpenProjectHandler>) => {
        openProjectHandler?.(...args)
    }
)

const PROJECT_MANAGEMENT_API = {
    setOpenProjectHandler: (handler: (id: string) => void) => {
        openProjectHandler = handler
    },
}

electron.contextBridge.exposeInMainWorld(PROJECT_MANAGEMENT_API_KEY, PROJECT_MANAGEMENT_API)

// ================
// === Menu API ===
// ================

let showAboutModalHandler: (() => void) | null = null

electron.ipcRenderer.on(ipc.Channel.showAboutModal, () => {
    showAboutModalHandler?.()
})

const MENU_API = {
    setShowAboutModalHandler: (callback: () => void) => {
        showAboutModalHandler = callback
    },
}

electron.contextBridge.exposeInMainWorld(MENU_API_KEY, MENU_API)

// ==================
// === System API ===
// ==================

const SYSTEM_API = {
    showItemInFolder: (fullPath: string) => {
        electron.ipcRenderer.send(ipc.Channel.showItemInFolder, fullPath)
    },
}

electron.contextBridge.exposeInMainWorld(SYSTEM_API_KEY, SYSTEM_API)

// ====================
// === Version info ===
// ====================

electron.contextBridge.exposeInMainWorld(VERSION_INFO_KEY, debug.VERSION_INFO)
