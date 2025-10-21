import { Channel } from '@/ipc'
import { dialog, ipcMain, shell, type BrowserWindow } from 'electron'
import { download } from 'electron-dl'
import type { DownloadUrlOptions } from 'enso-gui/src/electronApi'
import { unlinkSync } from 'node:fs'
import { basename, dirname, extname } from 'node:path'
import { importProjectFromPath, isProjectBundle, isProjectRoot } from 'project-manager-shim'
import { toElectronFileFilter, type FileFilter } from './fileBrowser'

/**
 * Initialize Inter-Process Communication between the Electron application and the served
 * website.
 */
export function initIpc(window: BrowserWindow | null) {
  ipcMain.on(Channel.error, (_event, data) => {
    console.error(...data)
  })
  ipcMain.on(Channel.warn, (_event, data) => {
    console.warn(...data)
  })
  ipcMain.on(Channel.log, (_event, data) => {
    console.log(...data)
  })
  ipcMain.on(Channel.info, (_event, data) => {
    console.info(...data)
  })
  ipcMain.on(
    Channel.importProjectFromPath,
    (event, path: string, directory: string | null, title: string) => {
      const directoryParams = directory == null ? [] : [directory]
      const info = importProjectFromPath(path, ...directoryParams, title)
      event.reply(Channel.importProjectFromPath, path, info)
    },
  )
  ipcMain.handle(Channel.downloadURL, async (_event, options: DownloadUrlOptions) => {
    const { url, path, name, shouldUnpackProject, showFileDialog } = options
    // This should never happen, but we'll check for it anyway.
    if (!window) {
      throw new Error('Window is not available.')
    }

    await download(window, url, {
      ...(path != null ? { directory: path } : {}),
      ...(name != null ? { filename: name } : {}),
      saveAs: showFileDialog != null ? showFileDialog : path == null,
      onCompleted: (file) => {
        const path = file.path
        const filenameRaw = basename(path)

        try {
          if (isProjectBundle(path) || isProjectRoot(path)) {
            if (!shouldUnpackProject) {
              return
            }
            // in case we're importing a project bundle, we need to remove the extension
            // from the filename
            const filename = filenameRaw.replace(extname(filenameRaw), '')
            const directory = dirname(path)

            importProjectFromPath(path, directory, filename)
            unlinkSync(path)
          }
        } catch (error) {
          console.error('Error downloading URL', error)
        }
      },
    })

    return
  })
  ipcMain.on(Channel.showItemInFolder, (_event, fullPath: string) => {
    shell.showItemInFolder(fullPath)
  })
  ipcMain.handle(
    Channel.openFileBrowser,
    async (
      _event,
      kind: 'default' | 'directory' | 'file' | 'filePath',
      defaultPath?: string,
      filters?: FileFilter[],
    ) => {
      console.log('Request for opening browser for ', kind, defaultPath, JSON.stringify(filters))
      let retval = null
      if (kind === 'filePath') {
        // "Accept", as the file won't be created immediately.
        const { canceled, filePath } = await dialog.showSaveDialog({
          buttonLabel: 'Accept',
          filters: filters?.map(toElectronFileFilter) ?? [],
          ...(defaultPath != null ? { defaultPath } : {}),
        })
        if (!canceled) {
          retval = [filePath]
        }
      } else {
        /** Helper for `showOpenDialog`, which has weird types by default. */
        type Properties = ('openDirectory' | 'openFile')[]
        const properties: Properties =
          kind === 'file' ? ['openFile']
          : kind === 'directory' ? ['openDirectory']
          : process.platform === 'darwin' ? ['openFile', 'openDirectory']
          : ['openFile']
        const { canceled, filePaths } = await dialog.showOpenDialog({
          properties,
          filters: filters?.map(toElectronFileFilter) ?? [],
          ...(defaultPath != null ? { defaultPath } : {}),
        })
        if (!canceled) {
          retval = filePaths
        }
      }
      return retval
    },
  )

  // Handling navigation events from renderer process
  ipcMain.on(Channel.goBack, () => {
    window?.webContents.navigationHistory.goBack()
  })

  ipcMain.on(Channel.goForward, () => {
    window?.webContents.navigationHistory.goForward()
  })
}
