/** @file Definition of an Electron application, which entails the creation of a rudimentary HTTP
 * server and the presentation of a Chrome web view, designed for optimal performance and
 * compatibility across a wide range of hardware configurations. The application's web component
 * is then served and showcased within the web view, complemented by the establishment of an
 * Inter-Process Communication channel, which enables seamless communication between the served web
 * application and the Electron process. */

import * as fs from 'node:fs/promises'
import * as fsSync from 'node:fs'
import * as os from 'node:os'
import * as pathModule from 'node:path'
import process from 'node:process'

import * as electron from 'electron'
import * as portfinder from 'portfinder'

import * as common from 'enso-common'
import * as contentConfig from 'enso-content-config'

import * as authentication from 'authentication'
import * as config from 'config'
import * as configParser from 'config/parser'
import * as debug from 'debug'
import * as detect from 'detect'
import * as fileAssociations from 'file-associations'
import * as ipc from 'ipc'
import * as log from 'log'
import * as naming from 'naming'
import * as paths from 'paths'
import * as projectManagement from 'project-management'
import * as projectManager from 'bin/project-manager'
import * as security from 'security'
import * as server from 'bin/server'
import * as urlAssociations from 'url-associations'
import * as utils from '../../../utils'

import GLOBAL_CONFIG from '../../../../gui/config.yaml' assert { type: 'yaml' }

const logger = contentConfig.logger

// ===========
// === App ===
// ===========

/** The Electron application. It is responsible for starting all the required services, and
 * displaying and managing the app window. */
class App {
    windows = new Set<electron.BrowserWindow>()
    lastFocusedWindow: electron.BrowserWindow | null = null
    server: server.Server | null = null
    projectManagerHost: string | null = null
    projectManagerPort: number | null = null
    isQuitting = false

    /** Initialize and run the Electron application. */
    async run() {
        log.addFileLog()
        this.becomeSingleInstance()
        urlAssociations.registerAssociations()
        // Register file associations for macOS.
        fileAssociations.setOpenFileEventHandler(id => {
            this.setProjectToOpenOnStartup(args, id)
        })

        const args = config.CONFIG.clone()
        // `urlToOpen` is ignored. If it is not `null`, the app should have exited when it tried to
        // become the sole instance.
        const { windowSize, chromeOptions, fileToOpen } = this.processArguments(
            args,
            fileAssociations.CLIENT_ARGUMENTS
        )
        this.handleItemOpening(args, fileToOpen)
        if (args.options.version.value) {
            await this.printVersion(args)
            electron.app.quit()
        } else if (args.groups.debug.options.info.value) {
            await electron.app.whenReady().then(async () => {
                await debug.printInfo()
                electron.app.quit()
            })
        } else {
            this.setChromeOptions(args, chromeOptions)
            security.enableAll()
            electron.app.on('before-quit', () => (this.isQuitting = true))
            /** TODO [NP]: https://github.com/enso-org/enso/issues/5851
             * The `electron.app.whenReady()` listener is preferable to the
             * `electron.app.on('ready', ...)` listener. When the former is used in combination with
             * the `authentication.initModule` call that is called in the listener, the application
             * freezes. This freeze should be diagnosed and fixed. Then, the `whenReady()` listener
             * should be used here instead. */
            electron.app.on('ready', () => {
                logger.log('Electron application is ready.')
                void this.main(args, windowSize)
            })
            this.registerShortcuts()
        }
    }

    /** Try to acquire the single-instance lock. If it fails, a new window will be opened in the
     * original instance, so this instance should be closed. */
    becomeSingleInstance() {
        const didAcquireLock = electron.app.requestSingleInstanceLock(
            fileAssociations.CLIENT_ARGUMENTS
        )
        if (didAcquireLock) {
            electron.app.on('second-instance', (_event, _argv, _cwd, argv) => {
                const args = config.CONFIG.clone()
                // `chromeOptions` MUST NOT be used, as changing Chrome options causes the renderer
                // to crash.
                const { windowSize, fileToOpen, urlToOpen } = this.processArguments(
                    args,
                    // This is SAFE, as the type of `argv` is statically known.
                    // eslint-disable-next-line no-restricted-syntax
                    argv as string[]
                )
                this.handleItemOpening(args, fileToOpen)
                // If `urlToOpen` is not `null`, then it will be handled by `url-associations.ts`.
                if (urlToOpen == null) {
                    void (async () => {
                        const window = await this.createWindowIfEnabled(args, windowSize)
                        if (window != null) {
                            this.loadWindowContent(args, window)
                        }
                    })()
                }
            })
        } else {
            // The existing instance will open a new window.
            process.exit(0)
        }
        return didAcquireLock
    }

    /** Process the command line arguments. */
    processArguments(args: typeof config.CONFIG, commandLineArguments: string[]) {
        // We parse only "client arguments", so we don't have to worry about the Electron-Dev vs
        // Electron-Proper distinction.
        const fileToOpen = fileAssociations.argsDenoteFileOpenAttempt(commandLineArguments)
        const urlToOpen = urlAssociations.argsDenoteUrlOpenAttempt(commandLineArguments)
        // If we are opening a file (i.e. we were spawned with just a path of the file to open as
        // the argument) or URL, it means that effectively we don't have any non-standard arguments.
        // We just need to let caller know that we are opening a file.
        const argsToParse = fileToOpen != null || urlToOpen != null ? [] : commandLineArguments
        return { ...configParser.parseArgs(args, argsToParse), fileToOpen, urlToOpen }
    }

    /** Set the project to be opened on application startup.
     *
     * This method should be called before the application is ready, as it only
     * modifies the startup options. If the application is already initialized,
     * an error will be logged, and the method will have no effect.
     *
     * @param args - The command-line configuration.
     * @param projectId - The ID of the project to be opened on startup. */
    setProjectToOpenOnStartup(args: typeof config.CONFIG, projectId: string) {
        // Make sure that we are not initialized yet, as this method should be called before the
        // application is ready.
        if (!electron.app.isReady()) {
            logger.log(`Setting the project to open on startup to '${projectId}'.`)
            args.groups.startup.options.project.value = projectId
        } else {
            logger.error(
                "Cannot set the project to open on startup to '" +
                    projectId +
                    "', as the application is already initialized."
            )
        }
    }

    /** This method is invoked when the application was spawned due to being a default application
     * for a URL protocol or file extension. */
    handleItemOpening(args: typeof config.CONFIG, fileToOpen: string | null) {
        logger.log('Opening file.', { fileToOpen })
        try {
            if (fileToOpen != null) {
                // This makes the IDE open the relevant project. Also, this prevents us from using
                // this method after the IDE has been fully set up, as the initializing code
                // would have already read the value of this argument.
                const projectId = fileAssociations.handleOpenFile(fileToOpen)
                this.setProjectToOpenOnStartup(args, projectId)
            }
        } catch (e) {
            // If we failed to open the file, we should enter the usual welcome screen.
            // The `handleOpenFile` function will have already displayed an error message.
        }
    }

    /** Set Chrome options based on the app configuration. For comprehensive list of available
     * Chrome options refer to: https://peter.sh/experiments/chromium-command-line-switches. */
    setChromeOptions(args: typeof config.CONFIG, chromeOptions: configParser.ChromeOption[]) {
        const addIf = (
            opt: contentConfig.Option<boolean>,
            chromeOptName: string,
            value?: string
        ) => {
            if (opt.value) {
                const chromeOption = new configParser.ChromeOption(chromeOptName, value)
                const chromeOptionStr = chromeOption.display()
                const optionName = opt.qualifiedName()
                logger.log(`Setting '${chromeOptionStr}' because '${optionName}' was enabled.`)
                chromeOptions.push(chromeOption)
            }
        }
        const add = (option: string, value?: string) => {
            chromeOptions.push(new configParser.ChromeOption(option, value))
        }
        logger.groupMeasured('Setting Chrome options', () => {
            const perfOpts = args.groups.performance.options
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
    async main(args: typeof config.CONFIG, windowSize: config.WindowSize) {
        // We catch all errors here. Otherwise, it might be possible that the app will run partially
        // and enter a "zombie mode", where user is not aware of the app still running.
        try {
            // Light theme is needed for vibrancy to be light colored on Windows.
            // electron.nativeTheme.themeSource = 'light'
            await logger.asyncGroupMeasured('Starting the application', async () => {
                // Note that we want to do all the actions synchronously, so when the window
                // appears, it serves the website immediately.
                await this.startBackendIfEnabled(args)
                await this.startContentServerIfEnabled(args)
                const window = await this.createWindowIfEnabled(args, windowSize)
                this.initIpc(args)
                authentication.initModule(() => this.lastFocusedWindow)
                if (window != null) {
                    this.loadWindowContent(args, window)
                }
            })
        } catch (err) {
            console.error('Failed to initialize the application, shutting down. Error:', err)
            electron.app.quit()
        }
    }

    /** Run the provided function if the provided option was enabled. Log a message otherwise. */
    async runIfEnabled<T = void>(option: contentConfig.Option<boolean>, fn: () => Promise<T> | T) {
        if (option.value) {
            return await fn()
        } else {
            logger.log(`The app is configured not to use ${option.name}.`)
            return null
        }
    }

    /** Start the backend processes. */
    async startBackendIfEnabled(args: typeof config.CONFIG) {
        await this.runIfEnabled(args.options.engine, async () => {
            // The first return value is the original string, which is not needed.
            // These all cannot be null as the format is known at runtime.
            const [, projectManagerHost, projectManagerPort] =
                // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                GLOBAL_CONFIG.projectManagerEndpoint.match(/^ws:\/\/(.+):(.+)$/)!
            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
            this.projectManagerHost ??= projectManagerHost!
            this.projectManagerPort ??= await portfinder.getPortPromise({
                // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                port: parseInt(projectManagerPort!),
            })
            const projectManagerUrl = `ws://${this.projectManagerHost}:${this.projectManagerPort}`
            args.groups.engine.options.projectManagerUrl.value = projectManagerUrl
            const backendOpts = args.groups.debug.options.verbose.value ? ['-vv'] : []
            const backendEnv = Object.assign({}, process.env, {
                // These are environment variables, and MUST be in CONSTANT_CASE.
                // eslint-disable-next-line @typescript-eslint/naming-convention
                SERVER_HOST: this.projectManagerHost,
                // eslint-disable-next-line @typescript-eslint/naming-convention
                SERVER_PORT: `${this.projectManagerPort}`,
            })
            projectManager.spawn(args, backendOpts, backendEnv)
        })
    }

    /** Start the content server, which will serve the application content (HTML) to the window. */
    async startContentServerIfEnabled(args: typeof config.CONFIG) {
        await this.runIfEnabled(args.options.server, async () => {
            await logger.asyncGroupMeasured('Starting the content server.', async () => {
                const serverCfg = new server.Config({
                    dir: paths.ASSETS_PATH,
                    port: args.groups.server.options.port.value,
                    externalFunctions: {
                        uploadProjectBundle: projectManagement.uploadBundle,
                    },
                })
                this.server = await server.Server.create(serverCfg)
            })
        })
    }

    /** Create the Electron window and display it on the screen. */
    async createWindowIfEnabled(args: typeof config.CONFIG, windowSize: config.WindowSize) {
        return await this.runIfEnabled(args.options.window, () => {
            return logger.groupMeasured('Creating the window.', () => {
                const argGroups = args.groups
                const useFrame = args.groups.window.options.frame.value
                const macOS = process.platform === 'darwin'
                const useHiddenInsetTitleBar = !useFrame && macOS
                const useVibrancy = args.groups.window.options.vibrancy.value
                const webPreferences: electron.WebPreferences = {
                    preload: pathModule.join(paths.APP_PATH, 'preload.cjs'),
                    sandbox: true,
                    backgroundThrottling: argGroups.performance.options.backgroundThrottling.value,
                    enableBlinkFeatures: argGroups.chrome.options.enableBlinkFeatures.value,
                    disableBlinkFeatures: argGroups.chrome.options.disableBlinkFeatures.value,
                    spellcheck: false,
                }
                const windowPreferences: electron.BrowserWindowConstructorOptions = {
                    webPreferences,
                    width: windowSize.width,
                    height: windowSize.height,
                    frame: useFrame,
                    titleBarStyle: useHiddenInsetTitleBar ? 'hiddenInset' : 'default',
                    ...(useVibrancy && detect.supportsVibrancy()
                        ? {
                              vibrancy: 'fullscreen-ui',
                              backgroundMaterial: 'acrylic',
                              ...(os.platform() === 'win32' ? { transparent: true } : {}),
                          }
                        : {}),
                }
                const window = new electron.BrowserWindow(windowPreferences)
                window.setMenuBarVisibility(false)
                if (args.groups.debug.options.devTools.value) {
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
                    }
                )

                window.on('focus', () => {
                    this.lastFocusedWindow = window
                })

                window.on('close', event => {
                    if (!this.isQuitting && !args.groups.window.options.closeToQuit.value) {
                        event.preventDefault()
                        window.hide()
                    }
                })

                electron.app.on('activate', () => {
                    if (!args.groups.window.options.closeToQuit.value) {
                        window.show()
                    }
                })

                window.webContents.on('render-process-gone', (_event, details) => {
                    logger.error('Error, the render process crashed.', details)
                })

                this.windows.add(window)
                return window
            })
        })
    }

    /** Initialize Inter-Process Communication between the Electron application and the served
     * website. */
    initIpc(args: typeof config.CONFIG) {
        electron.ipcMain.on(ipc.Channel.error, (_event, data) => {
            logger.error(`IPC error: ${JSON.stringify(data)}`)
        })
        const argProfiles = args.groups.profile.options.load.value
        const profilePromises: Promise<string>[] = argProfiles.map((path: string) =>
            fs.readFile(path, 'utf8')
        )
        const profilesPromise = Promise.all(profilePromises)
        electron.ipcMain.on(ipc.Channel.loadProfiles, event => {
            void profilesPromise.then(profiles => {
                event.reply('profiles-loaded', profiles)
            })
        })
        const profileOutPath = args.groups.profile.options.save.value
        if (profileOutPath) {
            electron.ipcMain.on(ipc.Channel.saveProfile, (_event, data: string) => {
                fsSync.writeFileSync(profileOutPath, data)
            })
        }
        electron.ipcMain.on(ipc.Channel.openGpuDebugInfo, event => {
            void event.sender.loadURL('chrome://gpu')
        })
        electron.ipcMain.on(ipc.Channel.quit, () => {
            electron.app.quit()
        })
        electron.ipcMain.on(ipc.Channel.importProjectFromPath, (event, path: string) => {
            const info = projectManagement.importProjectFromPath(path)
            event.reply(ipc.Channel.importProjectFromPath, path, info)
        })
    }

    /** The server port. In case the server was not started, the port specified in the configuration
     * is returned. This might be used to connect this application window to another, existing
     * application server. */
    serverPort(args: typeof config.CONFIG): number {
        return this.server?.config.port ?? args.groups.server.options.port.value
    }

    /** Redirect the web view to `localhost:<port>` to see the served website. */
    loadWindowContent(args: typeof config.CONFIG, window: electron.BrowserWindow) {
        const searchParams: Record<string, string> = {}
        for (const option of args.optionsRecursive()) {
            if (option.value !== option.default && option.passToWebApplication) {
                searchParams[option.qualifiedName()] = option.value.toString()
            }
        }
        const address = new URL('http://localhost')
        address.port = this.serverPort(args).toString()
        address.search = new URLSearchParams(searchParams).toString()
        logger.log(`Loading the window address '${address.toString()}'.`)
        void window.loadURL(address.toString())
    }

    /** Print the version of the frontend and the backend. */
    async printVersion(args: typeof config.CONFIG): Promise<void> {
        const indent = ' '.repeat(utils.INDENT_SIZE)
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
        const backend = await projectManager.version(args)
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
                    if (focusedWindow != null) {
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
