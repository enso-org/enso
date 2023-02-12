// FIXME: Issues to be resolved:
// - https://github.com/yargs/yargs-parser/issues/468
// - Enable vibrancy on MacOS

import Electron from 'electron'
import isDev from 'electron-is-dev'
import path from 'node:path'
import * as server from 'bin/server'
import * as content from '../../content/src/config'
import * as security from 'security'
import * as naming from 'naming'
import * as config from 'config'
import * as paths from 'paths'
import * as configParser from 'config/parser'
import * as debug from 'debug'
import * as projectManager from 'bin/project-manager'

const logger = content.logger

// ============
// === Help ===
// ============

/** The Electron application. It is responsible for starting all the required services, and
 * displaying and managing the app window. */
class App {
    window: null | Electron.BrowserWindow = null
    server: null | server.Server = null
    args: config.Args
    windowSize: config.WindowSize
    isQuitting = false
    constructor() {
        const { args, windowSize, chromeOptions } = configParser.parseArgs()
        this.args = args
        this.windowSize = windowSize
        this.setChromeOptions(chromeOptions)
        security.enableAll()
        Electron.app.on('before-quit', () => (this.isQuitting = true))
        Electron.app.whenReady().then(() => this.main())
        this.registerShortcuts()
    }

    /** Set Chrome options based on the user-provided ones and the app configuration options. */
    setChromeOptions(chromeOptions: configParser.ChromeOption[]) {
        if (this.args.groups.performance.options.forceHighPerformanceGpu.value) {
            chromeOptions.push(new configParser.ChromeOption('force_high_performance_gpu'))
        }
        if (this.args.groups.performance.options.ignoreGpuBlocklist.value) {
            chromeOptions.push(new configParser.ChromeOption('ignore-gpu-blocklist'))
        }
        if (this.args.groups.performance.options.disableSandbox.value) {
            chromeOptions.push(new configParser.ChromeOption('no-sandbox'))
        }
        if (this.args.groups.performance.options.disableGpuSandbox.value) {
            chromeOptions.push(new configParser.ChromeOption('disable-gpu-sandbox'))
        }
        if (this.args.groups.performance.options.disableGpuVsync.value) {
            chromeOptions.push(new configParser.ChromeOption('disable-gpu-vsync'))
        }
        if (this.args.groups.performance.options.enableNativeGpuMemoryBuffers.value) {
            chromeOptions.push(new configParser.ChromeOption('enable-native-gpu-memory-buffers'))
        }
        if (this.args.groups.performance.options.disableSmoothScrolling.value) {
            chromeOptions.push(new configParser.ChromeOption('disable-smooth-scrolling'))
        }
        chromeOptions.push(
            new configParser.ChromeOption(
                'use-angle',
                this.args.groups.performance.options.angleBackend.value
            )
        )
        chromeOptions.sort()
        if (chromeOptions.length > 0) {
            const desc = chromeOptions.map(t => `'${t.display()}'`).join(', ')
            const explanation = `See '-help-extended' to learn why these options were enabled.`
            logger.log(`Setting Chrome options: ${desc}. ${explanation}`)
            for (const chromeOption of chromeOptions) {
                Electron.app.commandLine.appendSwitch(chromeOption.name, chromeOption.value)
            }
        }
    }

    /** Main app entry point. */
    async main() {
        // We catch all errors here. Otherwise, it might be possible that the app will run partially
        // and the user will not see anything.
        try {
            if (this.args.options.version.value) {
                this.printVersionAndExit()
            } else if (this.args.options.info.value) {
                debug.printDebugInfoAndExit()
            }
            await logger.asyncGroupMeasured('Starting the application', async () => {
                // Note that we want to do all the actions synchronously, so when the window
                // appears, it serves the website immediately.
                await this.startBackend()
                await this.startContentServer()
                await this.createWindow()
                this.loadWindowContent()
            })
        } catch (err) {
            console.error('Failed to initialize the application, shutting down. Error:', err)
            Electron.app.quit()
        }
    }

    /** Run the provided function if the provided option was enabled. Log a message otherwise. */
    async runIfEnabled(option: content.Option<boolean>, fn: () => Promise<void>) {
        if (!option.value) {
            logger.log(`The app is configured not to run the ${option.name}.`)
        } else {
            fn()
        }
    }

    /** Start the backend processes. */
    async startBackend() {
        await this.runIfEnabled(this.args.options.engine, async () => {
            const backendOpts = this.args.groups.debug.options.verbose.value ? ['-vv'] : []
            projectManager.spawn(this.args, backendOpts)
        })
    }

    /** Start the content server, which will serve the application content (HTML) to the window */
    async startContentServer() {
        await this.runIfEnabled(this.args.options.server, async () => {
            await logger.asyncGroupMeasured('Starting the content server.', async () => {
                let serverCfg = new server.Config({
                    dir: paths.app,
                    port: this.args.groups.server.options.port.value,
                    fallback: '/assets/index.html',
                })
                this.server = await server.Server.create(serverCfg)
            })
        })
    }

    /** Create the Electron window and display it on the screen. */
    async createWindow() {
        await this.runIfEnabled(this.args.options.window, async () => {
            logger.groupMeasured('Creating the window.', () => {
                const webPreferences = security.secureWebPreferences()
                webPreferences.preload = path.join(paths.app, 'preload.cjs')
                webPreferences.sandbox = true
                webPreferences.backgroundThrottling =
                    this.args.groups.performance.options.backgroundThrottling.value
                webPreferences.devTools = this.args.groups.debug.options.devTools.value
                webPreferences.enableBlinkFeatures =
                    this.args.groups.chrome.options.enableBlinkFeatures.value
                webPreferences.disableBlinkFeatures =
                    this.args.groups.chrome.options.disableBlinkFeatures.value
                webPreferences.spellcheck = false

                let windowPreferences: Electron.BrowserWindowConstructorOptions = {
                    webPreferences,
                    width: this.windowSize.width,
                    height: this.windowSize.height,
                    frame: this.args.groups.window.options.frame.value,
                    transparent: false,
                    titleBarStyle: 'default',
                }

                if (windowPreferences.frame === false && process.platform === 'darwin') {
                    windowPreferences.titleBarStyle = 'hiddenInset'
                }

                if (this.args.groups.window.options.vibrancy.value) {
                    windowPreferences.vibrancy = 'fullscreen-ui'
                }

                const window = new Electron.BrowserWindow(windowPreferences)
                window.setMenuBarVisibility(false)

                if (this.args.groups.debug.options.devTools.value) {
                    window.webContents.openDevTools()
                }

                Electron.ipcMain.on('error', (event, data) => console.error(data))

                // FIXME
                // let profilePromises = []
                // if (args.loadProfile) {
                //     profilePromises = args.loadProfile.map((path: string) => fsp.readFile(path, 'utf8'))
                // }
                // const profiles = Promise.all(profilePromises)
                // Electron.ipcMain.on('load-profiles', event => {
                //     profiles.then(profiles => {
                //         event.reply('profiles-loaded', profiles)
                //     })
                // })
                // if (args.saveProfile) {
                //     Electron.ipcMain.on('save-profile', (event, data) => {
                //         fss.writeFileSync(args.saveProfile, data)
                //     })
                // }
                // if (args.workflow) {
                //     // @ts-ignore
                //     urlCfg.testWorkflow = args.workflow
                // }

                Electron.ipcMain.on('quit-ide', () => {
                    Electron.app.quit()
                })

                window.on('close', evt => {
                    if (!this.isQuitting && !this.args.groups.window.options.closeToQuit.value) {
                        evt.preventDefault()
                        window.hide()
                    }
                })

                Electron.app.on('activate', () => {
                    if (!this.args.groups.window.options.closeToQuit.value) {
                        window.show()
                    }
                })

                window.webContents.on('render-process-gone', (event, details) => {
                    // TODO: Ask the issue reporter for logs output and check if we can auto-fix it:
                    //   https://github.com/enso-org/enso/issues/3801
                    logger.error('Error, the render process crashed.', details)
                })

                this.window = window
            })
        })
    }

    serverPort(): number {
        if (this.server != null) {
            return this.server.config.port
        } else {
            return this.args.groups.server.options.port.value
        }
    }

    loadWindowContent() {
        const window = this.window
        if (window != null) {
            const urlCfg: { [key: string]: string } = {}
            for (const option of this.args.optionsRecursive()) {
                if (option.setByUser && option.passToApplication) {
                    urlCfg[option.qualifiedName()] = String(option.value)
                }
            }
            const params = server.urlParamsFromObject(urlCfg)
            const address = `http://localhost:${this.serverPort()}${params}`
            logger.log(`Loading the window address '${address}'.`)
            window.loadURL(address)
        }
    }

    printVersionAndExit() {
        let indent = ' '.repeat(4)
        let maxNameLen = 0
        for (let name in debug.versionInfo) {
            if (name.length > maxNameLen) {
                maxNameLen = name.length
            }
        }

        console.log('Frontend:')
        for (let [name, value] of Object.entries(debug.versionInfo)) {
            let label = naming.capitalizeFirstLetter(name)
            let spacing = ' '.repeat(maxNameLen - name.length)
            console.log(`${indent}${label}:${spacing} ${value}`)
        }

        console.log('')
        console.log('Backend:')
        projectManager.version(this.args).then(backend => {
            if (!backend) {
                console.log(`${indent}No backend available.`)
            } else {
                let lines = backend.split(/\r?\n/).filter(line => line.length > 0)
                for (let line of lines) {
                    console.log(`${indent}${line}`)
                }
            }
            process.exit()
        })
    }

    registerShortcuts() {
        Electron.app.on('web-contents-created', (webContentsCreatedEvent, webContents) => {
            webContents.on('before-input-event', (beforeInputEvent, input) => {
                const { code, alt, control, shift, meta, type } = input
                if (type !== 'keyDown') {
                    return
                }
                if (control && alt && shift && !meta && code === 'KeyI') {
                    const focusedWindow = Electron.BrowserWindow.getFocusedWindow()
                    if (focusedWindow) {
                        focusedWindow.webContents.toggleDevTools()
                        // FIXME: what if not
                    }
                }
                if (control && alt && shift && !meta && code === 'KeyR') {
                    const focusedWindow = Electron.BrowserWindow.getFocusedWindow()
                    if (focusedWindow) {
                        focusedWindow.reload()
                        // FIXME: what if not
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
                    Electron.app.quit()
                }
            })
        })
    }
}

// ==============
// === Events ===
// ==============

const app = new App()

// =================
// === Shortcuts ===
// =================
