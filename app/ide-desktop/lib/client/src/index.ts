/** @file Definition of an Electron application, which entails the creation of a rudimentary HTTP
 * server and the presentation of a Chrome web view, designed for optimal performance and
 * compatibility across a wide range of hardware configurations. The application's web component
 * is then served and showcased within the web view, complemented by the establishment of an
 * Inter-Process Communication channel, which enables seamless communication between the served web
 * application and the Electron process. */

import * as config from 'config'
import * as configParser from 'config/parser'
import * as content from 'enso-content-config'
import * as debug from 'debug'
import * as electron from 'electron'
import * as fs from 'node:fs/promises'
import * as fss from 'node:fs'
import * as ipc from 'ipc'
import * as naming from 'naming'
import * as path from 'node:path'
import * as paths from 'paths'
import * as projectManager from 'bin/project-manager'
import * as security from 'security'
import * as server from 'bin/server'
import * as process from 'process'
const logger = content.logger

// ===========
// === App ===
// ===========

/** The Electron application. It is responsible for starting all the required services, and
 * displaying and managing the app window. */
class App {
    window: null | electron.BrowserWindow = null
    server: null | server.Server = null
    args: config.Args
    isQuitting = false
    constructor() {
        this.args = config.config
    }

    async run() {
        const { args, windowSize, chromeOptions } = configParser.parseArgs()
        this.args = args
        if (this.args.options.version.value) {
            await this.printVersion()
            process.exit()
        } else if (this.args.groups.debug.options.info.value) {
            await electron.app.whenReady().then(async () => await debug.printInfo())
            process.exit()
        } else {
            this.setChromeOptions(chromeOptions)
            security.enableAll()
            electron.app.on('before-quit', () => (this.isQuitting = true))
            electron.app.whenReady().then(() => this.main(windowSize))
            this.registerShortcuts()
        }
    }

    /** Set Chrome options based on the app configuration. For comprehensive list of available
     * Chrome options refer to: https://peter.sh/experiments/chromium-command-line-switches. */
    setChromeOptions(chromeOptions: configParser.ChromeOption[]) {
        const addIf = (opt: content.Option<boolean>, chromeOptName: string, value?: string) => {
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
            chromeOptions.sort()
            if (chromeOptions.length > 0) {
                for (const chromeOption of chromeOptions) {
                    electron.app.commandLine.appendSwitch(chromeOption.name, chromeOption.value)
                }
                const cfgName = config.helpExtendedOptionName
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
                this.loadWindowContent()
            })
        } catch (err) {
            console.error('Failed to initialize the application, shutting down. Error:', err)
            electron.app.quit()
        }
    }

    /** Run the provided function if the provided option was enabled. Log a message otherwise. */
    async runIfEnabled(option: content.Option<boolean>, fn: () => Promise<void>) {
        option.value ? await fn() : logger.log(`The app is configured not to use ${option.name}.`)
    }

    /** Start the backend processes. */
    async startBackendIfEnabled() {
        await this.runIfEnabled(this.args.options.engine, async () => {
            const backendOpts = this.args.groups.debug.options.verbose.value ? ['-vv'] : []
            projectManager.spawn(this.args, backendOpts)
        })
    }

    /** Start the content server, which will serve the application content (HTML) to the window. */
    async startContentServerIfEnabled() {
        await this.runIfEnabled(this.args.options.server, async () => {
            await logger.asyncGroupMeasured('Starting the content server.', async () => {
                let serverCfg = new server.Config({
                    dir: paths.assets,
                    port: this.args.groups.server.options.port.value,
                })
                this.server = await server.Server.create(serverCfg)
            })
        })
    }

    /** Create the Electron window and display it on the screen. */
    async createWindowIfEnabled(windowSize: config.WindowSize) {
        await this.runIfEnabled(this.args.options.window, async () => {
            logger.groupMeasured('Creating the window.', () => {
                const argGroups = this.args.groups
                const useFrame = this.args.groups.window.options.frame.value
                const macOS = process.platform === 'darwin'
                const useHiddenInsetTitleBar = !useFrame && macOS
                const useVibrancy = this.args.groups.window.options.vibrancy.value
                const webPreferences: electron.WebPreferences = {
                    preload: path.join(paths.app, 'preload.cjs'),
                    sandbox: true,
                    backgroundThrottling: argGroups.performance.options.backgroundThrottling.value,
                    devTools: argGroups.debug.options.devTools.value,
                    enableBlinkFeatures: argGroups.chrome.options.enableBlinkFeatures.value,
                    disableBlinkFeatures: argGroups.chrome.options.disableBlinkFeatures.value,
                    spellcheck: false,
                }
                let windowPreferences: electron.BrowserWindowConstructorOptions = {
                    webPreferences,
                    width: windowSize.width,
                    height: windowSize.height,
                    frame: useFrame,
                    transparent: false,
                    titleBarStyle: useHiddenInsetTitleBar ? 'hiddenInset' : 'default',
                    vibrancy: useVibrancy ? 'fullscreen-ui' : undefined,
                }
                const window = new electron.BrowserWindow(windowPreferences)
                window.setMenuBarVisibility(false)
                if (this.args.groups.debug.options.devTools.value) {
                    window.webContents.openDevTools()
                }

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

                window.webContents.on('render-process-gone', (event, details) => {
                    logger.error('Error, the render process crashed.', details)
                })

                this.window = window
            })
        })
    }

    /** Initialize Inter-Process Communication between the Electron application and the served
     * website. */
    initIpc() {
        electron.ipcMain.on(ipc.channel.error, (event, data) => logger.error(`IPC error: ${data}`))
        let profilePromises: Promise<string>[] = []
        const argProfiles = this.args.groups.profile.options.loadProfile.value
        if (argProfiles) {
            profilePromises = argProfiles.map((path: string) => fs.readFile(path, 'utf8'))
        }
        const profiles = Promise.all(profilePromises)
        electron.ipcMain.on(ipc.channel.loadProfiles, event => {
            profiles.then(profiles => {
                event.reply('profiles-loaded', profiles)
            })
        })
        const profileOutPath = this.args.groups.profile.options.saveProfile.value
        if (profileOutPath) {
            electron.ipcMain.on(ipc.channel.saveProfile, (event, data) => {
                fss.writeFileSync(profileOutPath, data)
            })
        }
        electron.ipcMain.on(ipc.channel.quit, () => electron.app.quit())
    }

    /** The server port. In case the server was not started, the port specified in the configuration
     * is returned. This might be used to connect this application window to another, existing
     * application server. */
    serverPort(): number {
        if (this.server != null) {
            return this.server.config.port
        } else {
            return this.args.groups.server.options.port.value
        }
    }

    /** Redirect the web view to `localhost:<port>` to see the served website. */
    loadWindowContent() {
        const window = this.window
        if (window != null) {
            const urlCfg: { [key: string]: string } = {}
            for (const option of this.args.optionsRecursive()) {
                if (option.value != option.default && option.passToWebApplication) {
                    urlCfg[option.qualifiedName()] = String(option.value)
                }
            }
            const params = server.urlParamsFromObject(urlCfg)
            const address = `http://localhost:${this.serverPort()}${params}`
            logger.log(`Loading the window address '${address}'.`)
            window.loadURL(address)
        }
    }

    printVersion(): Promise<void> {
        let indent = ' '.repeat(4)
        let maxNameLen = 0
        for (let name in debug.versionInfo) {
            maxNameLen = Math.max(maxNameLen, name.length)
        }
        console.log('Frontend:')
        for (let [name, value] of Object.entries(debug.versionInfo)) {
            let label = naming.capitalizeFirstLetter(name)
            let spacing = ' '.repeat(maxNameLen - name.length)
            console.log(`${indent}${label}:${spacing} ${value}`)
        }

        console.log('')
        console.log('Backend:')
        return projectManager.version(this.args).then(backend => {
            if (!backend) {
                console.log(`${indent}No backend available.`)
            } else {
                let lines = backend.split(/\r?\n/).filter(line => line.length > 0)
                for (let line of lines) {
                    console.log(`${indent}${line}`)
                }
            }
        })
    }

    registerShortcuts() {
        electron.app.on('web-contents-created', (webContentsCreatedEvent, webContents) => {
            webContents.on('before-input-event', (beforeInputEvent, input) => {
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

                    let cmd_q = meta && !control && !alt && !shift && code === 'KeyQ'
                    let ctrl_q = !meta && control && !alt && !shift && code === 'KeyQ'
                    let alt_f4 = !meta && !control && alt && !shift && code === 'F4'
                    let ctrl_w = !meta && control && !alt && !shift && code === 'KeyW'
                    let quit_on_mac = process.platform === 'darwin' && (cmd_q || alt_f4)
                    let quit_on_win = process.platform === 'win32' && (alt_f4 || ctrl_w)
                    let quit_on_lin = process.platform === 'linux' && (alt_f4 || ctrl_q || ctrl_w)
                    let quit = quit_on_mac || quit_on_win || quit_on_lin
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

const app = new App()
app.run()
