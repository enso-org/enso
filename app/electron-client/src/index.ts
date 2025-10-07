/**
 * @file Definition of an Electron application, which entails the creation of a rudimentary HTTP
 * server and the presentation of a Chrome web view, designed for optimal performance and
 * compatibility across a wide range of hardware configurations. The application's web component
 * is then served and showcased within the web view, complemented by the establishment of an
 * Inter-Process Communication channel, which enables seamless communication between the served web
 * application and the Electron process.
 */

import './cjs-shim' // must be imported first

import * as fsSync from 'node:fs'
import * as fs from 'node:fs/promises'
import * as os from 'node:os'
import * as pathModule from 'node:path'
import process from 'node:process'

import * as electron from 'electron'
import * as portfinder from 'portfinder'

import * as common from 'enso-common'
import GLOBAL_CONFIG from 'enso-common/src/config.json' with { type: 'json' }
import {
  buildWebAppURLSearchParamsFromArgs,
  defaultOptions,
  type Options,
} from 'enso-common/src/options'

import * as authentication from '@/authentication'
import * as configParser from '@/configParser'
import * as contentConfig from '@/contentConfig'
import * as debug from '@/debug'
import * as fileAssociations from '@/fileAssociations'
import * as ipc from '@/ipc'
import * as log from '@/log'
import * as naming from '@/naming'
import * as paths from '@/paths'
import * as projectManager from '@/projectManager'
import * as security from '@/security'
import * as server from '@/server'
import * as urlAssociations from '@/urlAssociations'
import * as projectManagement from 'project-manager-shim'
import { toElectronFileFilter, type FileFilter } from './fileBrowser'

import * as download from 'electron-dl'
import type { DownloadUrlOptions } from 'enso-gui/src/electronApi'
import { filterByRole, inheritMenuItem, makeMenuItem, replaceMenuItems } from './menuItems'

const DEFAULT_WINDOW_WIDTH = 1380
const DEFAULT_WINDOW_HEIGHT = 900

/** Convert path to proper `file://` URL. */
function pathToURL(path: string): URL {
  if (process.platform === 'win32') {
    return new URL(encodeURI(`file:///${path.replaceAll('\\', '/')}`))
  } else {
    return new URL(encodeURI(`file://${path}`))
  }
}

// ===========
// === App ===
// ===========

/**
 * The Electron application. It is responsible for starting all the required services, and
 * displaying and managing the app window.
 */
class App {
  window: electron.BrowserWindow | null = null
  server: server.Server | null = null
  webOptions: Options = defaultOptions()
  projectManagerHost: string | null = null
  projectManagerPort: number | null = null
  isQuitting = false

  /** Initialize and run the Electron application. */
  async run() {
    log.setupLogger()
    urlAssociations.registerAssociations()
    // Register file associations for macOS.
    fileAssociations.setOpenFileEventHandler((path) => {
      if (electron.app.isReady()) {
        const project = fileAssociations.handleOpenFile(path)
        this.window?.webContents.send(ipc.Channel.openProject, project)
      } else {
        this.setProjectToOpenOnStartup(pathToURL(path))
      }
    })
    const { args, fileToOpen, urlToOpen } = this.processArguments()
    if (args.version) {
      await this.printVersion(args)
      electron.app.quit()
    } else if (args.debug.info) {
      await electron.app.whenReady().then(async () => {
        await debug.printInfo()
        electron.app.quit()
      })
    } else {
      const isOriginalInstance = electron.app.requestSingleInstanceLock({
        fileToOpen,
        urlToOpen,
      })
      if (isOriginalInstance) {
        this.handleItemOpening(fileToOpen, urlToOpen)
        this.setChromeOptions()
        security.enableAll()

        this.onStart().catch((err) => {
          console.error(err)
        })

        electron.app.on('before-quit', () => {
          this.isQuitting = true
        })

        electron.app.on('second-instance', (_event, argv) => {
          console.error(`Got data from 'second-instance' event: '${argv.toString()}'.`)

          const isWin = os.platform() === 'win32'

          if (isWin) {
            const ensoLinkInArgs = argv.find((arg) => arg.startsWith(common.DEEP_LINK_SCHEME))

            if (ensoLinkInArgs != null) {
              electron.app.emit('open-url', new CustomEvent('open-url'), ensoLinkInArgs)
            }
          }

          // The second instances will close themselves, but our window likely is not in the
          // foreground - the focus went to the "second instance" of the application.
          if (this.window) {
            if (this.window.isMinimized()) {
              this.window.restore()
            }
            this.window.focus()
          } else {
            console.error('No window found after receiving URL from second instance.')
          }
        })
        electron.app.whenReady().then(
          async () => {
            console.log('Electron application is ready.')

            electron.protocol.handle('enso', (request) =>
              projectManager.handleProjectProtocol(
                decodeURIComponent(request.url.replace('enso://', '')),
              ),
            )

            await this.main(args)
          },
          (error) => {
            console.error('Failed to initialize Electron.', error)
          },
        )
        this.registerShortcuts()
      } else {
        console.log('Another instance of the application is already running, exiting.')
        electron.app.quit()
      }
    }
  }

  /** Background tasks scheduled on the application startup. */
  async onStart() {
    const userData = electron.app.getPath('userData')
    const versionInfoPath = pathModule.join(userData, 'version_info.json')
    const versionInfoPathExists = await fs
      .access(versionInfoPath, fs.constants.F_OK)
      .then(() => true)
      .catch(() => false)

    if (versionInfoPathExists) {
      const versionInfoText = await fs.readFile(versionInfoPath, 'utf8')
      const versionInfoJson = JSON.parse(versionInfoText)

      if (debug.VERSION_INFO.version === versionInfoJson.version && !contentConfig.VERSION.isDev())
        return
    }

    const writeVersionInfoPromise = fs.writeFile(
      versionInfoPath,
      JSON.stringify(debug.VERSION_INFO),
      'utf8',
    )
    const downloadSamplesPromise = projectManagement.downloadSamples()

    return Promise.allSettled([writeVersionInfoPromise, downloadSamplesPromise])
  }

  /** Process the command line arguments. */
  processArguments(args = fileAssociations.CLIENT_ARGUMENTS) {
    // We parse only "client arguments", so we don't have to worry about the Electron-Dev vs
    // Electron-Proper distinction.
    const fileToOpen = fileAssociations.argsDenoteFileOpenAttempt(args)
    const urlToOpen = urlAssociations.argsDenoteUrlOpenAttempt(args)
    // If we are opening a file (i.e. we were spawned with just a path of the file to open as
    // the argument) or URL, it means that effectively we don't have any non-standard arguments.
    // We just need to let caller know that we are opening a file.
    const argsToParse = fileToOpen != null || urlToOpen != null ? [] : args
    return { args: configParser.parseArgs(argsToParse), fileToOpen, urlToOpen }
  }

  /**
   * Set the project to be opened on application startup.
   *
   * This method should be called before the application is ready, as it only
   * modifies the startup options. If the application is already initialized,
   * an error will be logged, and the method will have no effect.
   * @param projectUrl - The `file://` url of project to be opened on startup.
   */
  setProjectToOpenOnStartup(projectUrl: URL) {
    // Make sure that we are not initialized yet, as this method should be called before the
    // application is ready.
    if (!electron.app.isReady()) {
      console.log(`Setting the project to open on startup to '${projectUrl.toString()}'.`)
      this.webOptions.startup.project = projectUrl.toString()
    } else {
      console.error(
        "Cannot set the project to open on startup to '" +
          projectUrl.toString() +
          "', as the application is already initialized.",
      )
    }
  }

  /**
   * This method is invoked when the application was spawned due to being a default application
   * for a URL protocol or file extension.
   */
  handleItemOpening(fileToOpen: string | null, urlToOpen: URL | null) {
    console.log('Opening file or URL.', { fileToOpen, urlToOpen })
    try {
      if (fileToOpen != null) {
        // The IDE must receive the project path, otherwise if the IDE has a custom root directory
        // set then it is added to the (incorrect) default root directory.
        this.setProjectToOpenOnStartup(pathToURL(fileToOpen))
      }

      if (urlToOpen != null) {
        urlAssociations.handleOpenUrl(urlToOpen)
      }
    } catch {
      // If we failed to open the file, we should enter the usual welcome screen.
      // The `handleOpenFile` function will have already displayed an error message.
    }
  }

  /**
   * Set Chrome options based on the app configuration. For comprehensive list of available
   * Chrome options refer to: https://peter.sh/experiments/chromium-command-line-switches.
   */
  setChromeOptions() {
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

  /** Main app entry point. */
  async main(args: Options) {
    // We catch all errors here. Otherwise, it might be possible that the app will run partially
    // and enter a "zombie mode", where user is not aware of the app still running.
    try {
      console.log('Starting the application')
      // Note that we want to do all the actions synchronously, so when the window
      // appears, it serves the website immediately.
      await this.startContentServerIfEnabled(args)
      await this.startBackendIfEnabled(args)
      await this.createWindowIfEnabled(args)
      this.initIpc()
      await this.loadWindowContent(args)
      /**
       * The non-null assertion on the following line is safe because the window
       * initialization is guarded by the `createWindowIfEnabled` method. The window is
       * not yet created at this point, but it will be created by the time the
       * authentication module uses the lambda providing the window.
       */
      authentication.initAuthentication(() => this.window!)
    } catch (err) {
      console.error('Failed to initialize the application, shutting down. Error: ', err)
      electron.app.quit()
    }
  }

  /** Run the provided function if the provided option was enabled. Log a message otherwise. */
  async runIfEnabled(option: boolean, fn: () => Promise<void> | void) {
    if (option) {
      await fn()
    }
  }

  /** Start the backend processes. */
  async startBackendIfEnabled(args: Options) {
    await this.runIfEnabled(args.engineEnabled, async () => {
      // The first return value is the original string, which is not needed.
      // These all cannot be null as the format is known at runtime.
      const [, projectManagerHost, projectManagerPort] =
        GLOBAL_CONFIG.projectManagerEndpoint.match(/^ws:\/\/(.+):(.+)$/)!
      this.projectManagerHost ??= projectManagerHost!
      this.projectManagerPort ??= await portfinder.getPortPromise({
        port: parseInt(projectManagerPort!),
      })
      const projectManagerUrl = `ws://${this.projectManagerHost}:${this.projectManagerPort}`
      this.webOptions.engine.projectManagerUrl = projectManagerUrl
      const backendVerboseOpts = args.debug.verbose ? ['-vv'] : []
      const backendProfileTime = ['--profiling-time', String(args.debug.profileTime)]
      const backendProfileOpts =
        args.debug.profile ? ['--profiling-path', 'profiling.npss', ...backendProfileTime] : []
      const backendJvmOpts = args.useJvm ? ['--jvm'] : []
      const backendOpts = [...backendVerboseOpts, ...backendProfileOpts, ...backendJvmOpts]
      const backendEnv = Object.assign({}, process.env, {
        SERVER_HOST: this.projectManagerHost,
        SERVER_PORT: `${this.projectManagerPort}`,
      })
      projectManager.spawn(args, backendOpts, backendEnv)
    })
  }

  /** Start the content server, which will serve the application content (HTML) to the window. */
  async startContentServerIfEnabled(args: Options) {
    await this.runIfEnabled(args.useServer, async () => {
      console.log('Starting the content server.')
      const serverCfg = new server.Config({
        dir: paths.ASSETS_PATH,
        port: args.server.port,
        externalFunctions: {
          runProjectManagerCommand: (cliArguments, body?: NodeJS.ReadableStream) =>
            projectManager.runCommand(args, cliArguments, body),
        },
      })
      this.server = await server.Server.create(serverCfg)
      console.log('Content server started.')
    })
  }

  /** Create the Electron window and display it on the screen. */
  async createWindowIfEnabled(args: Options) {
    await this.runIfEnabled(args.displayWindow, () => {
      console.log('Creating the window.')
      const webPreferences: electron.WebPreferences = {
        preload: pathModule.join(paths.APP_PATH, 'preload.mjs'),
        sandbox: true,
        spellcheck: false,
        ...(process.env.ENSO_TEST ? { partition: 'test' } : {}),
      }
      const windowPreferences: electron.BrowserWindowConstructorOptions = {
        webPreferences,
        width: DEFAULT_WINDOW_WIDTH,
        height: DEFAULT_WINDOW_HEIGHT,
        frame: true,
        titleBarStyle: 'default',
        ...(process.env.DEV_DARK_BACKGROUND ? { backgroundColor: '#36312c' } : {}),
      }
      const window = new electron.BrowserWindow(windowPreferences)

      const oldMenu = electron.Menu.getApplicationMenu()
      if (oldMenu != null) {
        const newMenu = replaceMenuItems(oldMenu.items, [
          {
            filter: [filterByRole('help')],
            replacement: (item) =>
              inheritMenuItem(item, undefined, [
                makeMenuItem(window, `About ${common.PRODUCT_NAME}`, 'about'),
              ]),
          },
          {
            filter: [filterByRole('fileMenu'), filterByRole('close')],
            replacement: () => makeMenuItem(window, 'Close Tab', 'closeTab', 'CmdOrCtrl+W'),
          },
          {
            filter: [filterByRole('appMenu'), filterByRole('about')],
            replacement: () => undefined,
          },
          {
            filter: [filterByRole('appMenu'), filterByRole('hide')],
            replacement: (item) => inheritMenuItem(item, `Hide ${common.PRODUCT_NAME}`),
          },
          {
            filter: [filterByRole('appMenu'), filterByRole('quit')],
            replacement: (item) => inheritMenuItem(item, `Quit ${common.PRODUCT_NAME}`),
          },
        ])
        electron.Menu.setApplicationMenu(newMenu)
      }
      window.setMenuBarVisibility(false)

      if (args.debug.devTools) {
        window.webContents.openDevTools()
      }

      const allowedPermissions = ['clipboard-read', 'clipboard-sanitized-write']
      window.webContents.session.setPermissionRequestHandler(
        (_webContents, permission, callback) => {
          if (allowedPermissions.includes(permission)) {
            callback(true)
          } else {
            console.error(`Denied permission check '${permission}'.`)
            callback(false)
          }
        },
      )

      // Quit application on window close on all platforms except Mac (it is default behavior on Mac).
      const closeToQuit = process.platform !== 'darwin'

      window.on('close', (event) => {
        if (!this.isQuitting && !closeToQuit) {
          event.preventDefault()
          window.hide()
        }
      })

      electron.app.on('activate', () => {
        if (!closeToQuit) {
          window.show()
        }
      })

      window.webContents.on('render-process-gone', (_event, details) => {
        console.error('Error, the render process crashed.', details)
      })

      this.window = window
      console.log('Window created.')
    })
  }

  /**
   * Initialize Inter-Process Communication between the Electron application and the served
   * website.
   */
  initIpc() {
    electron.ipcMain.on(ipc.Channel.error, (_event, data) => {
      console.error(...data)
    })
    electron.ipcMain.on(ipc.Channel.warn, (_event, data) => {
      console.warn(...data)
    })
    electron.ipcMain.on(ipc.Channel.log, (_event, data) => {
      console.log(...data)
    })
    electron.ipcMain.on(ipc.Channel.info, (_event, data) => {
      console.info(...data)
    })
    electron.ipcMain.on(
      ipc.Channel.importProjectFromPath,
      (event, path: string, directory: string | null, title: string) => {
        const directoryParams = directory == null ? [] : [directory]
        const info = projectManagement.importProjectFromPath(path, ...directoryParams, title)
        event.reply(ipc.Channel.importProjectFromPath, path, info)
      },
    )
    electron.ipcMain.handle(
      ipc.Channel.downloadURL,
      async (_event, options: DownloadUrlOptions) => {
        const { url, path, name, shouldUnpackProject, showFileDialog } = options
        // This should never happen, but we'll check for it anyway.
        if (!this.window) {
          throw new Error('Window is not available.')
        }

        await download.download(this.window, url, {
          ...(path != null ? { directory: path } : {}),
          ...(name != null ? { filename: name } : {}),
          saveAs: showFileDialog != null ? showFileDialog : path == null,
          onCompleted: (file) => {
            const path = file.path
            const filenameRaw = pathModule.basename(path)

            try {
              if (
                projectManagement.isProjectBundle(path) ||
                projectManagement.isProjectRoot(path)
              ) {
                if (!shouldUnpackProject) {
                  return
                }
                // in case we're importing a project bundle, we need to remove the extension
                // from the filename
                const filename = filenameRaw.replace(pathModule.extname(filenameRaw), '')
                const directory = pathModule.dirname(path)

                projectManagement.importProjectFromPath(path, directory, filename)
                fsSync.unlinkSync(path)
              }
            } catch (error) {
              console.error('Error downloading URL', error)
            }
          },
        })

        return
      },
    )
    electron.ipcMain.on(ipc.Channel.showItemInFolder, (_event, fullPath: string) => {
      electron.shell.showItemInFolder(fullPath)
    })
    electron.ipcMain.handle(
      ipc.Channel.openFileBrowser,
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
          const { canceled, filePath } = await electron.dialog.showSaveDialog({
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
          const { canceled, filePaths } = await electron.dialog.showOpenDialog({
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
    electron.ipcMain.on(ipc.Channel.goBack, () => {
      this.window?.webContents.navigationHistory.goBack()
    })

    electron.ipcMain.on(ipc.Channel.goForward, () => {
      this.window?.webContents.navigationHistory.goForward()
    })
  }

  /**
   * The server port. In case the server was not started, the port specified in the configuration
   * is returned. This might be used to connect this application window to another, existing
   * application server.
   */
  serverPort(args: Options): number {
    return this.server?.config.port ?? args.server.port
  }

  /** Redirect the web view to `localhost:<port>` to see the served website. */
  async loadWindowContent(args: Options) {
    if (this.window != null) {
      const searchParams = buildWebAppURLSearchParamsFromArgs({
        ...this.webOptions,
        ...args,
      })
      const address = new URL('https://localhost')
      address.port = this.serverPort(args).toString()
      address.search = searchParams.toString()
      console.log(`Loading the window address '${address.toString()}'.`)
      if (process.env.ELECTRON_DEV_MODE === 'true') {
        // Vite takes a while to be `import`ed, so the first load almost always fails.
        // Reload every second until Vite is ready
        // (i.e. when `index.html` has a non-empty body).
        const window = this.window
        const onLoad = () => {
          void window.webContents.mainFrame
            // Get the HTML contents of `document.body`.
            .executeJavaScript('document.body.innerHTML')
            .then((html) => {
              // If `document.body` is empty, then `index.html` failed to load.
              if (html === '') {
                console.warn('Loading failed, reloading...')
                window.webContents.once('did-finish-load', onLoad)
                setTimeout(() => {
                  void window.loadURL(address.toString())
                }, 1_000)
              }
            })
        }
        // Wait for page to load before checking content, because of course the content is
        // empty if the page isn't loaded.
        window.webContents.once('did-finish-load', onLoad)
      }
      await this.window.loadURL(address.toString())
    }
  }

  /** Print the version of the frontend and the backend. */
  async printVersion(args: Options): Promise<void> {
    const indent = '    '
    let maxNameLen = 0
    for (const name in debug.VERSION_INFO) {
      maxNameLen = Math.max(maxNameLen, name.length)
    }
    process.stdout.write('Frontend:\n')
    for (const [name, value] of Object.entries(debug.VERSION_INFO)) {
      const label = naming.capitalizeFirstLetter(name)
      const spacing = ' '.repeat(maxNameLen - name.length)
      process.stdout.write(`${indent}${label}:${spacing} ${value}\n`)
    }
    process.stdout.write('\n')
    process.stdout.write('Backend:\n')
    const backend = await projectManager.version(args)
    if (backend == null) {
      process.stdout.write(`${indent}No backend available.\n`)
    } else {
      const lines = backend.split(/\r?\n/).filter((line) => line.length > 0)
      for (const line of lines) {
        process.stdout.write(`${indent}${line}\n`)
      }
    }
  }

  registerShortcuts() {
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
}

// ===================
// === App startup ===
// ===================

process.on('uncaughtException', (err, origin) => {
  console.error(`Uncaught exception: ${err.toString()}\nException origin: ${origin}`)
  electron.dialog.showErrorBox(common.PRODUCT_NAME, err.stack ?? err.toString())
  electron.app.exit(1)
})

const APP = new App()
void APP.run()
