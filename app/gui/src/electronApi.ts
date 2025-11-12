/** @file Shared API types exposed on `window.api` for both GUI and Electron. */
import type * as saveAccessToken from 'enso-common/src/accessToken'
import type { DownloadUrlOptions } from 'enso-common/src/download'
import type { Path } from 'enso-common/src/services/Backend'
import type { FileFilter } from './project-view/util/fileFilter'
import type { MenuItem, MenuItemHandler } from './project-view/util/menuItems'

export interface AuthenticationApi {
  readonly openUrlInSystemBrowser: (url: string) => void
  readonly setDeepLinkHandler: (callback: (url: string) => void) => void
  readonly saveAccessToken: (accessToken: saveAccessToken.AccessToken | null) => void
}

export interface NavigationApi {
  readonly goBack: () => void
  readonly goForward: () => void
}

export interface MenuApi {
  readonly setMenuItemHandler: (name: MenuItem, callback: MenuItemHandler) => void
}

export interface SystemApi {
  readonly downloadURL: (options: DownloadUrlOptions) => Promise<void>
  readonly showItemInFolder: (fullPath: string) => void
  readonly getFilePath: (item: File) => string
}

export interface FileBrowserApi {
  readonly openFileBrowser: (
    kind: 'default' | 'directory' | 'file' | 'filePath',
    defaultPath?: string,
    fileTypes?: FileFilter[],
  ) => Promise<string[] | undefined>
}

export interface ProjectInfo {
  readonly id: string
  readonly name: string
  readonly projectRoot: Path
  readonly parentDirectory: string
}

export interface ProjectManagementApi {
  readonly setOpenProjectHandler: (handler: (projectInfo: ProjectInfo) => void) => void
}

export interface VersionInfo {
  readonly version: string
  readonly build: string
  readonly electron: string
  readonly chrome: string
}

export interface LogApi {
  readonly log: (msg: any[]) => void
  readonly info: (msg: any[]) => void
  readonly warn: (msg: any[]) => void
  readonly error: (msg: any[]) => void
}

export interface ElectronApi {
  readonly authentication: AuthenticationApi
  readonly navigation: NavigationApi
  readonly menu: MenuApi
  readonly system?: SystemApi
  readonly projectManagement: ProjectManagementApi
  readonly fileBrowser: FileBrowserApi
  readonly versionInfo: VersionInfo
  readonly mapBoxApiToken: () => string
  readonly log: LogApi
}
export type { FileFilter, MenuItem, MenuItemHandler }
