/// <reference types="vite/client" />

declare const PROJECT_MANAGER_URL: string | undefined
declare const YDOC_SERVER_URL: string | undefined
declare const IS_CLOUD_BUILD: boolean

interface Document {
  caretPositionFromPoint(x: number, y: number): { offsetNode: Node; offset: number } | null
}

interface Window {
  fileBrowserApi: FileBrowserApi
}

/** `window.fileBrowserApi` is a context bridge to the main process, when we're running in an
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
    kind: 'file' | 'directory' | 'default' | 'filePath',
    defaultPath?: string,
  ) => Promise<string[] | undefined>
}

interface LogEvent {
  (message: string, projectId?: string | null, metadata?: object | null): void
}
