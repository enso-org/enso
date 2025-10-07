/**
 * @file Inter-Process communication configuration of the application. IPC allows the web-view
 * content to exchange information with the Electron application.
 */

/** Channel names used by the IPC protocol. */
export type Channel = (typeof Channel)[keyof typeof Channel]
export const Channel = {
  /** 'log' channel for passing log messages from renderer to main process. */
  log: 'log',
  /** 'info' channel for passing info messages from renderer to main process. */
  info: 'info',
  /** 'warn' channel for passing warning messages from renderer to main process. */
  warn: 'warn',
  /** 'error' channel for passing error messages from renderer to main process. */
  error: 'error',
  /** Channel for requesting that a URL be opened by the system browser. */
  openUrlInSystemBrowser: 'open-url-in-system-browser',
  /** Channel for signaling that a deep link to this application was opened. */
  openDeepLink: 'open-deep-link',
  /** Channel for signaling that access token be saved to a credentials file. */
  saveAccessToken: 'save-access-token',
  /** Channel for importing a project or project bundle from the given path. */
  importProjectFromPath: 'import-project-from-path',
  /** Channel for opening project */
  openProject: 'open-project',
  goBack: 'go-back',
  goForward: 'go-forward',
  /** Channel for selecting files and directories using the system file browser. */
  openFileBrowser: 'open-file-browser',
  /** Show a file or folder in the system file browser. */
  showItemInFolder: 'show-item-in-folder',
  /** Download a file using its URL. */
  downloadURL: 'download-url',
  handleMenuItem: 'handle-menu-item',
} as const
