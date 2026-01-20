/**
 * @file Preload script containing code that runs before web page is loaded into the browser
 * window. It has access to both DOM APIs and Node environment, and is used to expose privileged
 * APIs to the renderer via the contextBridge API. To learn more, visit:
 * https://www.electronjs.org/docs/latest/tutorial/tutorial-preload.
 */
import * as debug from '@/debug'
import * as ipc from '@/ipc'
import type * as accessToken from 'enso-common/src/accessToken'
import type { ElectronApi } from 'enso-gui/src/electronApi'
import type { MenuItem, MenuItemHandler } from 'enso-gui/src/project-view/util/menuItems'
import type * as projectManagement from 'project-manager-shim'
import type { FileFilter } from './fileBrowser'

// Even though this is already built as an mjs module, we are "faking" cjs format on preload script
// due to missing module support. Since this is the only module that's treated as external by
// esbuild, we have to manually use "require". Switch this to an import once new electron version
// actually honours ".mjs" files for sandboxed preloading (this will likely become an error at that time).
// https://www.electronjs.org/fr/docs/latest/tutorial/esm#sandboxed-preload-scripts-cant-use-esm-imports
// eslint-disable-next-line @typescript-eslint/no-require-imports
const electron = require('electron')

const API_KEY = 'api'

/** A type-safe wrapper around {@link electron.contextBridge.exposeInMainWorld}. */
function exposeInMainWorld<Key extends string & keyof Window>(
  key: Key,
  value: NonNullable<(typeof window)[Key]>,
) {
  electron.contextBridge.exposeInMainWorld(key, value)
}

const IMPORT_PROJECT_RESOLVE_FUNCTIONS = new Map<
  string,
  (projectId: projectManagement.ProjectInfo) => void
>()

const navigation: ElectronApi['navigation'] = {
  goBack: () => {
    electron.ipcRenderer.send(ipc.Channel.goBack)
  },
  goForward: () => {
    electron.ipcRenderer.send(ipc.Channel.goForward)
  },
}

electron.ipcRenderer.on(
  ipc.Channel.importProjectFromPath,
  (
    _event: Electron.IpcRendererEvent,
    projectPath: string,
    projectInfo: projectManagement.ProjectInfo,
  ) => {
    const resolveFunction = IMPORT_PROJECT_RESOLVE_FUNCTIONS.get(projectPath)
    IMPORT_PROJECT_RESOLVE_FUNCTIONS.delete(projectPath)
    resolveFunction?.(projectInfo)
  },
)

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
const authentication: ElectronApi['authentication'] = {
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
}

const fileBrowser: NonNullable<ElectronApi['fileBrowser']> = {
  openFileBrowser: (
    kind: 'default' | 'directory' | 'file' | 'filePath',
    defaultPath?: string,
    filters?: FileFilter[],
  ) => electron.ipcRenderer.invoke(ipc.Channel.openFileBrowser, kind, defaultPath, filters),
}

/** A callback when a project is opened by opening a fileusing the system's default method. */
type OpenProjectHandler = (projectInfo: projectManagement.ProjectInfo) => void
let openProjectHandler: OpenProjectHandler | undefined

electron.ipcRenderer.on(
  ipc.Channel.openProject,
  (_event: Electron.IpcRendererEvent, ...args: Parameters<OpenProjectHandler>) => {
    openProjectHandler?.(...args)
  },
)

const projectManagementApi: NonNullable<ElectronApi['projectManagement']> = {
  setOpenProjectHandler: (handler: (projectInfo: projectManagement.ProjectInfo) => void) => {
    openProjectHandler = handler
  },
}

const menuApiHandlers: Record<MenuItem, MenuItemHandler | undefined> = {
  about: undefined,
  closeTab: undefined,
}

electron.ipcRenderer.on(
  ipc.Channel.handleMenuItem,
  (_event: Electron.IpcRendererEvent, name: MenuItem) => {
    menuApiHandlers[name]?.()
  },
)

const menu: ElectronApi['menu'] = {
  setMenuItemHandler: (name: MenuItem, handler: MenuItemHandler) => {
    menuApiHandlers[name] = handler
  },
}

const system: ElectronApi['system'] = {
  downloadURL: (options) => {
    return electron.ipcRenderer.invoke(ipc.Channel.downloadURL, options)
  },
  showItemInFolder: (fullPath: string) => {
    electron.ipcRenderer.send(ipc.Channel.showItemInFolder, fullPath)
  },
  getFilePath: (item: File) => {
    return electron.webUtils.getPathForFile(item)
  },
}

const api: ElectronApi = {
  authentication,
  navigation,
  menu,
  system,
  projectManagement: projectManagementApi,
  fileBrowser,
  versionInfo: debug.VERSION_INFO,
  mapBoxApiToken: () => process.env.ENSO_IDE_MAPBOX_API_TOKEN || '',
  log: {
    log: (msg: any[]) => {
      electron.ipcRenderer.send(ipc.Channel.log, msg)
    },
    info: (msg: any[]) => {
      electron.ipcRenderer.send(ipc.Channel.info, msg)
    },
    warn: (msg: any[]) => {
      electron.ipcRenderer.send(ipc.Channel.warn, msg)
    },
    error: (msg: any[]) => {
      electron.ipcRenderer.send(ipc.Channel.error, msg)
    },
  },
}

exposeInMainWorld(API_KEY, api)
