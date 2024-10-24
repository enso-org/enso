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
import * as buildUtils from 'enso-common/src/buildUtils'
import GLOBAL_CONFIG from 'enso-common/src/config.json' assert { type: 'json' }

import * as authentication from '@/authentication'
import * as config from '@/config'
import * as configParser from '@/configParser'
import * as contentConfig from '@/contentConfig'
import * as debug from '@/debug'
import * as detect from '@/detect'
import * as fileAssociations from '@/fileAssociations'
import * as ipc from '@/ipc'
import * as log from '@/log'
import * as naming from '@/naming'
import * as paths from '@/paths'
import * as projectManagement from '@/projectManagement'
import * as projectManager from '@/projectManager'
import * as security from '@/security'
import * as server from '@/server'
import * as urlAssociations from '@/urlAssociations'

const logger = contentConfig.logger

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
  args: config.Args = config.CONFIG
  projectManagerHost: string | null = null
  projectManagerPort: number | null = null
  isQuitting = false

  /** Initialize and run the Electron application. */
  async run() {
    log.addFileLog()
    urlAssociations.registerAssociations()
    // Register file associations for macOS.
    fileAssociations.setOpenFileEventHandler(path => {
      if (electron.app.isReady()) {
        const project = fileAssociations.handleOpenFile(path)
        this.window?.webContents.send(ipc.Channel.openProject, project)
      } else {
        this.setProjectToOpenOnStartup(pathToURL(path))
      }
    })

    const { windowSize, chromeOptions, fileToOpen, urlToOpen } = this.processArguments()
    if (this.args.options.version.value) {
      await this.printVersion()
      electron.app.quit()
    } else if (this.args.groups.debug.options.info.value) {
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
        this.setChromeOptions(chromeOptions)
        security.enableAll()

        electron.app.on('before-quit', () => {
          this.isQuitting = true
        })

        electron.app.on('second-instance', (_event, argv) => {
          logger.log(`Got data from 'second-instance' event: '${argv.toString()}'.`)

          const isWin = os.platform() === 'win32'

          if (isWin) {
            const ensoLinkInArgs = argv.find(arg => arg.startsWith(common.DEEP_LINK_SCHEME))

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
            logger.error('No window found after receiving URL from second instance.')
          }
        })
        electron.app.whenReady().then(
          async () => {
            logger.log('Electron application is ready.')
            await this.main(windowSize)
          },
          error => {
            logger.error('Failed to initialize Electron.', error)
          },
        )
        this.registerShortcuts()
      } else {
        logger.log('Another instance of the application is already running, exiting.')
        electron.app.quit()
      }
    }
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
    return { ...configParser.parseArgs(argsToParse), fileToOpen, urlToOpen }
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
      logger.log(`Setting the project to open on startup to '${projectUrl.toString()}'.`)
      this.args.groups.startup.options.project.value = projectUrl.toString()
    } else {
      logger.error(
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
    logger.log('Opening file or URL.', { fileToOpen, urlToOpen })
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
  setChromeOptions(chromeOptions: configParser.ChromeOption[]) {
    const addIf = (
      option: contentConfig.Option<boolean>,
      chromeOptName: string,
      value?: string,
    ) => {
      if (option.value) {
        const chromeOption = new configParser.ChromeOption(chromeOptName, value)
        const chromeOptionStr = chromeOption.display()
        const optionName = option.qualifiedName()
        logger.log(`Setting '${chromeOptionStr}' because '${optionName}' was enabled.`)
        chromeOptions.push(chromeOption)
      }
    }
    const add = (option: string, value?: string) => {
      const chromeOption = new configParser.ChromeOption(option, value)
      const chromeOptionStr = chromeOption.display()
      logger.log(`Setting '${chromeOptionStr}'`)
      chromeOptions.push(new configParser.ChromeOption(option, value))
    }
    logger.groupMeasured('Setting Chrome options', () => {
      const perfOpts = this.args.groups.performance.options
      // Needed to accept localhost self-signed cert
      add('ignore-certificate-errors')
      addIf(perfOpts.disableGpuSandbox, 'disable-gpu-sandbox')
      addIf(perfOpts.disableGpuVsync, 'disable-gpu-vsync')
      addIf(perfOpts.disableSandbox, 'no-sandbox')
      addIf(perfOpts.disableSmoothScrolling, 'disable-smooth-scrolling')
      addIf(perfOpts.enableNativeGpuMemoryBuffers, 'enable-native-gpu-memory-buffers')
      addIf(perfOpts.forceHighPerformanceGpu, 'force_high_performance_gpu')
      addIf(perfOpts.ignoreGpuBlocklist, 'ignore-gpu-blocklist')
      add('use-angle', perfOpts.angleBackend.value)
      chromeOptions.sort((a, b) => a.name.localeCompare(b.name))
      if (chromeOptions.length > 0) {
        for (const chromeOption of chromeOptions) {
          electron.app.commandLine.appendSwitch(chromeOption.name, chromeOption.value)
        }
        const cfgName = config.HELP_EXTENDED_OPTION_NAME
        logger.log(`See '-${cfgName}' to learn why these options were enabled.`)
      }
    })
  }

  /** Main app entry point. */
  async main(windowSize: config.WindowSize) {
    // We catch all errors here. Otherwise, it might be possible that the app will run partially
    // and enter a "zombie mode", where user is not aware of the app still running.
    try {
      // Light theme is needed for vibrancy to be light colored on Windows.
      // electron.nativeTheme.themeSource = 'light'
      await logger.asyncGroupMeasured('Starting the application', async () => {
        // Note that we want to do all the actions synchronously, so when the window
        // appears, it serves the website immediately.
        await this.startContentServerIfEnabled()
        await this.startBackendIfEnabled()
        await this.createWindowIfEnabled(windowSize)
        this.initIpc()
        await this.loadWindowContent()
        /**
         * The non-null assertion on the following line is safe because the window
         * initialization is guarded by the `createWindowIfEnabled` method. The window is
         * not yet created at this point, but it will be created by the time the
         * authentication module uses the lambda providing the window.
         */
        authentication.initAuthentication(() => this.window!)
      })
    } catch (err) {
      logger.error('Failed to initialize the application, shutting down. Error: ', err)
      electron.app.quit()
    } finally {
      logger.groupEnd()
    }
  }

  /** Run the provided function if the provided option was enabled. Log a message otherwise. */
  async runIfEnabled(option: contentConfig.Option<boolean>, fn: () => Promise<void> | void) {
    if (option.value) {
      await fn()
    } else {
      logger.log(`The app is configured not to use ${option.name}.`)
    }
  }

  /** Start the backend processes. */
  async startBackendIfEnabled() {
    await this.runIfEnabled(this.args.options.engine, async () => {
      // The first return value is the original string, which is not needed.
      // These all cannot be null as the format is known at runtime.
      const [, projectManagerHost, projectManagerPort] =
        GLOBAL_CONFIG.projectManagerEndpoint.match(/^ws:\/\/(.+):(.+)$/)!
      this.projectManagerHost ??= projectManagerHost!
      this.projectManagerPort ??= await portfinder.getPortPromise({
        port: parseInt(projectManagerPort!),
      })
      const projectManagerUrl = `ws://${this.projectManagerHost}:${this.projectManagerPort}`
      this.args.groups.engine.options.projectManagerUrl.value = projectManagerUrl
      const backendVerboseOpts = this.args.groups.debug.options.verbose.value ? ['-vv'] : []
      const backendProfileTime =
        this.args.groups.debug.options.profileTime.value ?
          ['--profiling-time', String(this.args.groups.debug.options.profileTime.value)]
        : ['--profiling-time', '120']
      const backendProfileOpts =
        this.args.groups.debug.options.profile.value ?
          ['--profiling-path', 'profiling.npss', ...backendProfileTime]
        : []
      const backendOpts = [...backendVerboseOpts, ...backendProfileOpts]
      const backendEnv = Object.assign({}, process.env, {
        SERVER_HOST: this.projectManagerHost,
        SERVER_PORT: `${this.projectManagerPort}`,
      })
      projectManager.spawn(this.args, backendOpts, backendEnv)
    })
  }

  /** Start the content server, which will serve the application content (HTML) to the window. */
  async startContentServerIfEnabled() {
    await this.runIfEnabled(this.args.options.server, async () => {
      await logger
        .asyncGroupMeasured('Starting the content server.', async () => {
          const serverCfg = new server.Config({
            dir: paths.ASSETS_PATH,
            port: this.args.groups.server.options.port.value,
            externalFunctions: {
              uploadProjectBundle: projectManagement.uploadBundle,
              runProjectManagerCommand: (cliArguments, body?: NodeJS.ReadableStream) =>
                projectManager.runCommand(this.args, cliArguments, body),
            },
          })
          this.server = await server.Server.create(serverCfg)
        })
        .finally(() => {
          logger.groupEnd()
        })
    })
  }

  /** Create the Electron window and display it on the screen. */
  async createWindowIfEnabled(windowSize: config.WindowSize) {
    await this.runIfEnabled(this.args.options.window, () => {
      logger.groupMeasured('Creating the window.', () => {
        const argGroups = this.args.groups
        const useFrame = this.args.groups.window.options.frame.value
        const macOS = process.platform === 'darwin'
        const useHiddenInsetTitleBar = !useFrame && macOS
        this.args.groups.window.options.vibrancy.value &&= detect.supportsVibrancy()
        const useVibrancy = this.args.groups.window.options.vibrancy.value
        const webPreferences: electron.WebPreferences = {
          preload: pathModule.join(paths.APP_PATH, 'preload.mjs'),
          sandbox: true,
          backgroundThrottling: argGroups.performance.options.backgroundThrottling.value,
          enableBlinkFeatures: argGroups.chrome.options.enableBlinkFeatures.value,
          disableBlinkFeatures: argGroups.chrome.options.disableBlinkFeatures.value,
          spellcheck: false,
          ...(process.env.ENSO_TEST != null && process.env.ENSO_TEST !== '' ?
            { partition: 'test' }
          : {}),
        }
        const windowPreferences: electron.BrowserWindowConstructorOptions = {
          webPreferences,
          width: windowSize.width,
          height: windowSize.height,
          frame: useFrame,
          titleBarStyle: useHiddenInsetTitleBar ? 'hiddenInset' : 'default',
          ...(useVibrancy ?
            {
              vibrancy: 'fullscreen-ui',
              backgroundMaterial: 'acrylic',
              ...(os.platform() === 'win32' || os.platform() === 'linux' ?
                { transparent: true }
              : {}),
            }
          : {}),
        }
        const window = new electron.BrowserWindow(windowPreferences)
        window.setMenuBarVisibility(false)
        const oldMenu = electron.Menu.getApplicationMenu()
        if (oldMenu != null) {
          const items = oldMenu.items.map(item => {
            if (item.role !== 'help') {
              return item
            } else {
              // `click` is a property that is intentionally removed from this
              // destructured object, in order to satisfy TypeScript.
              // eslint-disable-next-line @typescript-eslint/no-unused-vars
              const { click, ...passthrough } = item
              return new electron.MenuItem({
                ...passthrough,
                submenu: electron.Menu.buildFromTemplate([
                  new electron.MenuItem({
                    label: `About ${common.PRODUCT_NAME}`,
                    click: () => {
                      window.webContents.send(ipc.Channel.showAboutModal)
                    },
                  }),
                ]),
              })
            }
          })
          const newMenu = electron.Menu.buildFromTemplate(items)
          electron.Menu.setApplicationMenu(newMenu)
        }

        if (this.args.groups.debug.options.devTools.value) {
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

        window.on('close', event => {
          if (!this.isQuitting && !this.args.groups.window.options.closeToQuit.value) {
            event.preventDefault()
            window.hide()
          }
        })

        electron.app.on('activate', () => {
          if (!this.args.groups.window.options.closeToQuit.value) {
            window.show()
          }
        })

        window.webContents.on('render-process-gone', (_event, details) => {
          logger.error('Error, the render process crashed.', details)
        })

        this.window = window
      })
    })
  }

  /**
   * Initialize Inter-Process Communication between the Electron application and the served
   * website.
   */
  initIpc() {
    electron.ipcMain.on(ipc.Channel.error, (_event, data) => {
      logger.error(`IPC error: ${JSON.stringify(data)}`)
    })
    const argProfiles = this.args.groups.profile.options.load.value
    const profilePromises: Promise<string>[] = argProfiles.map((path: string) =>
      fs.readFile(path, 'utf8'),
    )
    const profilesPromise = Promise.all(profilePromises)
    electron.ipcMain.on(ipc.Channel.loadProfiles, event => {
      void profilesPromise.then(profiles => {
        event.reply('profiles-loaded', profiles)
      })
    })
    const profileOutPath = this.args.groups.profile.options.save.value
    if (profileOutPath) {
      electron.ipcMain.on(ipc.Channel.saveProfile, (_event, data: string) => {
        fsSync.writeFileSync(profileOutPath, data)
      })
    }
    electron.ipcMain.on(ipc.Channel.openGpuDebugInfo, () => {
      if (this.window != null) {
        void this.window.loadURL('chrome://gpu')
      }
    })
    electron.ipcMain.on(ipc.Channel.quit, () => {
      electron.app.quit()
    })
    electron.ipcMain.on(
      ipc.Channel.importProjectFromPath,
      (event, path: string, directory: string | null, title: string) => {
        const directoryParams = directory == null ? [] : [directory]
        const info = projectManagement.importProjectFromPath(path, ...directoryParams, title)
        event.reply(ipc.Channel.importProjectFromPath, path, info)
      },
    )
    electron.ipcMain.on(
      ipc.Channel.downloadURL,
      (_event, url: string, headers?: Record<string, string>) => {
        electron.BrowserWindow.getFocusedWindow()?.webContents.downloadURL(
          url,
          headers ? { headers } : {},
        )
      },
    )
    electron.ipcMain.on(ipc.Channel.showItemInFolder, (_event, fullPath: string) => {
      electron.shell.showItemInFolder(fullPath)
    })
    electron.ipcMain.handle(
      ipc.Channel.openFileBrowser,
      async (_event, kind: 'default' | 'directory' | 'file' | 'filePath', defaultPath?: string) => {
        logger.log('Request for opening browser for ', kind, defaultPath)
        let retval = null
        if (kind === 'filePath') {
          // "Accept", as the file won't be created immediately.
          const { canceled, filePath } = await electron.dialog.showSaveDialog({
            buttonLabel: 'Accept',
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
      this.window?.webContents.goBack()
    })

    electron.ipcMain.on(ipc.Channel.goForward, () => {
      this.window?.webContents.goForward()
    })
  }

  /**
   * The server port. In case the server was not started, the port specified in the configuration
   * is returned. This might be used to connect this application window to another, existing
   * application server.
   */
  serverPort(): number {
    return this.server?.config.port ?? this.args.groups.server.options.port.value
  }

  /** Redirect the web view to `localhost:<port>` to see the served website. */
  async loadWindowContent() {
    if (this.window != null) {
      const searchParams: Record<string, string> = {}
      for (const option of this.args.optionsRecursive()) {
        if (option.value !== option.default && option.passToWebApplication) {
          searchParams[option.qualifiedName()] = option.value.toString()
        }
      }
      const address = new URL('https://localhost')
      address.port = this.serverPort().toString()
      address.search = new URLSearchParams(searchParams).toString()
      logger.log(`Loading the window address '${address.toString()}'.`)
      if (process.env.ELECTRON_DEV_MODE === 'true') {
        // Vite takes a while to be `import`ed, so the first load almost always fails.
        // Reload every second until Vite is ready
        // (i.e. when `index.html` has a non-empty body).
        const window = this.window
        const onLoad = () => {
          void window.webContents.mainFrame
            // Get the HTML contents of `document.body`.
            .executeJavaScript('document.body.innerHTML')
            .then(html => {
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
  async printVersion(): Promise<void> {
    const indent = ' '.repeat(buildUtils.INDENT_SIZE)
    let maxNameLen = 0
    for (const name in debug.VERSION_INFO) {
      maxNameLen = Math.max(maxNameLen, name.length)
    }
    console.log('Frontend:')
    for (const [name, value] of Object.entries(debug.VERSION_INFO)) {
      const label = naming.capitalizeFirstLetter(name)
      const spacing = ' '.repeat(maxNameLen - name.length)
      console.log(`${indent}${label}:${spacing} ${value}`)
    }
    console.log('')
    console.log('Backend:')
    const backend = await projectManager.version(this.args)
    if (backend == null) {
      console.log(`${indent}No backend available.`)
    } else {
      const lines = backend.split(/\r?\n/).filter(line => line.length > 0)
      for (const line of lines) {
        console.log(`${indent}${line}`)
      }
    }
  }

  /** Register keyboard shortcuts. */
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

          const cmdQ = meta && !control && !alt && !shift && code === 'KeyQ'
          const ctrlQ = !meta && control && !alt && !shift && code === 'KeyQ'
          const altF4 = !meta && !control && alt && !shift && code === 'F4'
          const ctrlW = !meta && control && !alt && !shift && code === 'KeyW'
          const quitOnMac = process.platform === 'darwin' && (cmdQ || altF4)
          const quitOnWin = process.platform === 'win32' && (altF4 || ctrlW)
          const quitOnLinux = process.platform === 'linux' && (altF4 || ctrlQ || ctrlW)
          const quit = quitOnMac || quitOnWin || quitOnLinux
          if (quit) {
            electron.app.quit()
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
