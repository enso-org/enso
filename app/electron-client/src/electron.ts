import { Channel } from '@/ipc'
import { dialog, ipcMain, shell, type BrowserWindow } from 'electron'
import { download } from 'electron-dl'
import type { DownloadUrlOptions } from 'enso-common/src/download'
import { unlinkSync } from 'node:fs'
import { basename, dirname, extname } from 'node:path'
import { importProjectFromPath, isProjectBundle, isProjectRoot } from 'project-manager-shim'
import { toElectronFileFilter, type FileFilter } from './fileBrowser'

export type Electron = typeof import('electron')

/**
 * Set Chrome options based on the app configuration. For comprehensive list of available
 * Chrome options refer to: https://peter.sh/experiments/chromium-command-line-switches.
 */
export function setChromeOptions(electron: Electron) {
  // Needed to accept localhost self-signed cert
  electron.app.commandLine.appendSwitch('ignore-certificate-errors')
  // Enable native CPU-mappable GPU memory buffer support on Linux.
  electron.app.commandLine.appendSwitch('enable-native-gpu-memory-buffers')
  // Override the list of blocked GPU hardware, allowing for GPU acceleration on system configurations
  // that do not inherently support it. It should be noted that some hardware configurations may have
  // driver issues that could result in rendering discrepancies. Despite this, the utilization of GPU
  // acceleration has the potential to significantly enhance the performance of the application in our
  // specific use cases. This behavior can be observed in the following example:
  // https://groups.google.com/a/chromium.org/g/chromium-dev/c/09NnO6jYT6o.
  electron.app.commandLine.appendSwitch('ignore-gpu-blocklist')
}

/** Register keyboard shortcuts that should be handled by Electron. */
export function registerShortcuts(electron: Electron) {
  electron.app.on('web-contents-created', (_webContentsCreatedEvent, webContents) => {
    webContents.on('before-input-event', (_beforeInputEvent, input) => {
      const { code, alt, control, shift, meta, type } = input
      if (type === 'keyDown') {
        const focusedWindow = electron.BrowserWindow.getFocusedWindow()
        if (focusedWindow) {
          if (control && alt && shift && !meta && code === 'KeyI') {
            focusedWindow.webContents.toggleDevTools()
          }
          if (control && alt && shift && !meta && code === 'KeyR') {
            focusedWindow.reload()
          }
        }
      }
    })
  })
}

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
