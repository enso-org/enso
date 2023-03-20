/** @file Definition of an Electron application, which entails the creation of a rudimentary HTTP
 * server and the presentation of a Chrome web view, designed for optimal performance and
 * compatibility across a wide range of hardware configurations. The application's web component
 * is then served and showcased within the web view, complemented by the establishment of an
 * Inter-Process Communication channel, which enables seamless communication between the served web
 * application and the Electron process. */

import * as fs from 'node:fs/promises'
import * as fsSync from 'node:fs'
import * as pathModule from 'node:path'
import process from 'node:process'

import * as electron from 'electron'

import * as contentConfig from 'enso-content-config'

import * as authentication from 'authentication'
import * as config from 'config'
import * as configParser from 'config/parser'
import * as debug from 'debug'
import * as ipc from 'ipc'
import * as naming from 'naming'
import * as paths from 'paths'
import * as projectManager from 'bin/project-manager'
import * as security from 'security'
import * as server from 'bin/server'

const logger = contentConfig.logger

// =================
// === Constants ===
// =================

/** Indent size for outputting JSON. */
const INDENT_SIZE = 4

// ===========
// === App ===
// ===========

/** The Electron application. It is responsible for starting all the required services, and
 * displaying and managing the app window. */
class App {
    window: electron.BrowserWindow | null = null
    server: server.Server | null = null
    args: config.Args = config.CONFIG
    isQuitting = false

    async run() {
        const { args, windowSize, chromeOptions } = configParser.parseArgs()
        this.args = args
        if (this.args.options.version.value) {
            await this.printVersion()
            process.exit()
        } else if (this.args.groups.debug.options.info.value) {
            await electron.app.whenReady().then(async () => {
                await debug.printInfo()
            })
            process.exit()
        } else {
            this.setChromeOptions(chromeOptions)
            security.enableAll()
            electron.app.on('before-quit', () => (this.isQuitting = true))
            /** TODO [NP]: https://github.com/enso-org/enso/issues/5851
             * The `electron.app.whenReady()` listener is preferable to the
             * `electron.app.on('ready', ...)` listener. When the former is used in combination with
             * the `authentication.initModule` call that is called in the listener, the application
             * freezes. This freeze should be diagnosed and fixed. Then, the `whenReady()` listener
             * should be used here instead. */
            electron.app.on('ready', () => {
                void this.main(windowSize)
            })
            this.registerShortcuts()
        }
    }

    /** Set Chrome options based on the app configuration. For comprehensive list of available
     * Chrome options refer to: https://peter.sh/experiments/chromium-command-line-switches. */
    setChromeOptions(chromeOptions: configParser.ChromeOption[]) {
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
        const add = (option: string, value?: string) =>
            chromeOptions.push(new configParser.ChromeOption(option, value))
        logger.groupMeasured('Setting Chrome options', () => {
            const perfOpts = this.args.groups.performance.options
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
            await logger.asyncGroupMeasured('Starting the application', async () => {
                // Note that we want to do all the actions synchronously, so when the window
                // appears, it serves the website immediately.
                await this.startBackendIfEnabled()
                await this.startContentServerIfEnabled()
                await this.createWindowIfEnabled(windowSize)
                this.initIpc()
                /** The non-null assertion on the following line is safe because the window
                 * initialization is guarded by the `createWindowIfEnabled` method. The window is
                 * not yet created at this point, but it will be created by the time the
                 * authentication module uses the lambda providing the window. */
                // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
                authentication.initModule(() => this.window!)
                this.loadWindowContent()
            })
        } catch (err) {
            console.error('Failed to initialize the application, shutting down. Error:', err)
            electron.app.quit()
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
        await this.runIfEnabled(this.args.options.engine, () => {
            const backendOpts = this.args.groups.debug.options.verbose.value ? ['-vv'] : []
            projectManager.spawn(this.args, backendOpts)
        })
    }

    /** Start the content server, which will serve the application content (HTML) to the window. */
    async startContentServerIfEnabled() {
        await this.runIfEnabled(this.args.options.server, async () => {
            await logger.asyncGroupMeasured('Starting the content server.', async () => {
                const serverCfg = new server.Config({
                    dir: paths.ASSETS_PATH,
                    port: this.args.groups.server.options.port.value,
                })
                this.server = await server.Server.create(serverCfg)
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
                const useVibrancy = this.args.groups.window.options.vibrancy.value
                const webPreferences: electron.WebPreferences = {
                    preload: pathModule.join(paths.APP_PATH, 'preload.cjs'),
                    sandbox: true,
                    backgroundThrottling: argGroups.performance.options.backgroundThrottling.value,
                    devTools: argGroups.debug.options.devTools.value,
                    enableBlinkFeatures: argGroups.chrome.options.enableBlinkFeatures.value,
                    disableBlinkFeatures: argGroups.chrome.options.disableBlinkFeatures.value,
                    spellcheck: false,
                }
                const windowPreferences: electron.BrowserWindowConstructorOptions = {
                    webPreferences,
                    width: windowSize.width,
                    height: windowSize.height,
                    frame: useFrame,
                    transparent: false,
                    titleBarStyle: useHiddenInsetTitleBar ? 'hiddenInset' : 'default',
                    ...(useVibrancy ? { vibrancy: 'fullscreen-ui' } : {}),
                }
                const window = new electron.BrowserWindow(windowPreferences)
                window.setMenuBarVisibility(false)
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
                    }
                )

                window.on('close', evt => {
                    if (!this.isQuitting && !this.args.groups.window.options.closeToQuit.value) {
                        evt.preventDefault()
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

    /** Initialize Inter-Process Communication between the Electron application and the served
     * website. */
    initIpc() {
        electron.ipcMain.on(ipc.Channel.error, (_event, data) => {
            logger.error(`IPC error: ${JSON.stringify(data)}`)
        })
        const argProfiles = this.args.groups.profile.options.loadProfile.value
        const profilePromises: Promise<string>[] = argProfiles.map((path: string) =>
            fs.readFile(path, 'utf8')
        )
        const profilesPromise = Promise.all(profilePromises)
        electron.ipcMain.on(ipc.Channel.loadProfiles, event => {
            void profilesPromise.then(profiles => {
                event.reply('profiles-loaded', profiles)
            })
        })
        const profileOutPath = this.args.groups.profile.options.saveProfile.value
        if (profileOutPath) {
            electron.ipcMain.on(ipc.Channel.saveProfile, (_event, data: string) => {
                fsSync.writeFileSync(profileOutPath, data)
            })
        }
        electron.ipcMain.on(ipc.Channel.quit, () => {
            electron.app.quit()
        })
    }

    /** The server port. In case the server was not started, the port specified in the configuration
     * is returned. This might be used to connect this application window to another, existing
     * application server. */
    serverPort(): number {
        return this.server?.config.port ?? this.args.groups.server.options.port.value
    }

    /** Redirect the web view to `localhost:<port>` to see the served website. */
    loadWindowContent() {
        if (this.window != null) {
            const urlCfg: Record<string, string> = {}
            for (const option of this.args.optionsRecursive()) {
                if (option.value !== option.default && option.passToWebApplication) {
                    urlCfg[option.qualifiedName()] = String(option.value)
                }
            }
            const params = server.urlParamsFromObject(urlCfg)
            const address = `http://localhost:${this.serverPort()}${params}`
            logger.log(`Loading the window address '${address}'.`)
            void this.window.loadURL(address)
        }
    }

    printVersion(): Promise<void> {
        const indent = ' '.repeat(INDENT_SIZE)
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
        return projectManager.version(this.args).then(backend => {
            if (!backend) {
                console.log(`${indent}No backend available.`)
            } else {
                const lines = backend.split(/\r?\n/).filter(line => line.length > 0)
                for (const line of lines) {
                    console.log(`${indent}${line}`)
                }
            }
        })
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

const APP = new App()
void APP.run()
